#!/usr/bin/env python3
"""Ad-hoc fuzzing harness for the COBOL -> CBL-C pipeline."""

from __future__ import annotations

import argparse
import os
import random
import shutil
import string
import subprocess
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Sequence, Tuple

DEFAULT_ITERATIONS = 50
DEFAULT_TIMEOUT = 8.0
DEFAULT_MODE = "all"
VALID_MODES = ("grammar", "mutation", "all")
SAMPLE_ROOT = Path("samples/cobol")
FAILURE_ROOT = Path("build/fuzz_failures")
COBOL_KEYWORDS = (
    "ADD",
    "CALL",
    "COMPUTE",
    "DISPLAY",
    "IF",
    "MOVE",
    "PERFORM",
    "STOP RUN.",
    "SUBTRACT",
    "STRING",
)
STRING_ALPHABET = string.ascii_uppercase + string.digits + " '"


@dataclass
class FuzzResult:
    mode: str
    iteration: int
    returncode: int
    crashed: bool
    diagnostics: bool
    elapsed: float
    input_path: Path


class FuzzHarness:
    def __init__(self, binary: Path, seed: int | None, keep_failures: bool, timeout: float) -> None:
        resolved = binary.expanduser()
        if not resolved.is_absolute():
            resolved = Path.cwd() / resolved
        self._binary = resolved.resolve()
        self._rng = random.Random(seed)
        self._keep_failures = keep_failures
        self._timeout = timeout
        self._ensure_binary()
        self._ensure_failure_root()

    def _ensure_binary(self) -> None:
        if not self._binary.exists():
            raise SystemExit(f"Fuzzing requires a compiled transpiler binary at {self._binary!s}")
        if not os.access(self._binary, os.X_OK):
            raise SystemExit(f"Binary {self._binary!s} is not executable")

    def _ensure_failure_root(self) -> None:
        if self._keep_failures:
            FAILURE_ROOT.mkdir(parents=True, exist_ok=True)

    def run(self, iterations: int, modes: Iterable[str]) -> List[FuzzResult]:
        results: List[FuzzResult] = []
        for mode in modes:
            for iteration in range(1, iterations + 1):
                result = self._run_single(mode, iteration)
                results.append(result)
        return results

    def _run_single(self, mode: str, iteration: int) -> FuzzResult:
        if mode == "grammar":
            source = self._generate_grammar_program()
        elif mode == "mutation":
            source = self._mutate_sample_program()
        else:
            raise ValueError(f"Unsupported mode: {mode}")

        with tempfile.NamedTemporaryFile("w", suffix=".cob", delete=False) as cob_file:
            cob_path = Path(cob_file.name)
            cob_file.write(source)
        with tempfile.NamedTemporaryFile("w", suffix=".cblc", delete=False) as output_file:
            output_path = Path(output_file.name)

        try:
            start = time.monotonic()
            completed = subprocess.run(
                [
                    str(self._binary),
                    "--direction",
                    "cobol-to-cblc",
                    "--input",
                    str(cob_path),
                    "--output",
                    str(output_path),
                    "--diagnostics",
                    "silent",
                ],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                timeout=self._timeout,
                check=False,
            )
            elapsed = time.monotonic() - start
            crashed = completed.returncode < 0 or completed.returncode >= 128
            diagnostics = completed.returncode not in (0,) and not crashed
            if crashed and self._keep_failures:
                self._archive_failure(cob_path, mode, iteration, completed)
            return FuzzResult(
                mode=mode,
                iteration=iteration,
                returncode=completed.returncode,
                crashed=crashed,
                diagnostics=diagnostics,
                elapsed=elapsed,
                input_path=cob_path,
            )
        finally:
            if output_path.exists():
                output_path.unlink()
            if cob_path.exists():
                cob_path.unlink()

    def _archive_failure(self, cob_path: Path, mode: str, iteration: int, completed: subprocess.CompletedProcess[str]) -> None:
        timestamp = time.strftime("%Y%m%d-%H%M%S")
        destination = FAILURE_ROOT / f"{timestamp}_{mode}_{iteration:04d}.cob"
        shutil.copy2(cob_path, destination)
        (destination.with_suffix(".log")).write_text(
            f"returncode={completed.returncode}\n"
            f"stdout:\n{completed.stdout}\n"
            f"stderr:\n{completed.stderr}\n",
            encoding="utf-8",
        )

    def _generate_grammar_program(self) -> str:
        program_name = self._random_identifier(prefix="FUZZ")
        numeric_defs, numeric_names = self._generate_numeric_definitions()
        string_entries = self._generate_string_definitions()
        declarations = numeric_defs + [definition for definition, _ in string_entries]
        if not declarations:
            declarations.append("       01 FUZZ-NUM PIC 9(4) VALUE 1234.")
            numeric_names.append("FUZZ-NUM")

        statements = self._generate_statements(numeric_names, string_entries)
        lines = [
            "       IDENTIFICATION DIVISION.",
            f"       PROGRAM-ID. {program_name}.",
            "       DATA DIVISION.",
            "       WORKING-STORAGE SECTION.",
        ]
        lines.extend(declarations)
        lines.append("       PROCEDURE DIVISION.")
        lines.append("       MAIN-PARA.")
        lines.extend(statements)
        lines.append("           STOP RUN.")
        return "\n".join(lines) + "\n"

    def _generate_numeric_definitions(self) -> Tuple[List[str], List[str]]:
        count = self._rng.randint(1, 3)
        defs: List[str] = []
        names: List[str] = []
        for _ in range(count):
            name = self._random_identifier(prefix="NUM")
            width = self._rng.randint(2, 8)
            value = self._rng.randint(0, 10 ** min(width, 6) - 1)
            defs.append(f"       01 {name} PIC 9({width}) VALUE {value}.")
            names.append(name)
        return defs, names

    def _generate_string_definitions(self) -> List[Tuple[str, int]]:
        count = self._rng.randint(0, 2)
        entries: List[Tuple[str, int]] = []
        for _ in range(count):
            name = self._random_identifier(prefix="STR")
            width = self._rng.randint(3, 12)
            literal = self._random_string_literal(width)
            definition = f"       01 {name} PIC X({width}) VALUE \"{literal}\"."
            entries.append((definition, width))
        return entries

    def _generate_statements(self, numeric_names: Sequence[str], string_vars: Sequence[Tuple[str, int]]) -> List[str]:
        statement_count = self._rng.randint(3, 6)
        statements: List[str] = []
        for _ in range(statement_count):
            choice = self._rng.choice(["display-string", "display-numeric", "move-numeric", "move-string"])
            if choice == "display-string" and string_vars:
                target_name, _ = self._rng.choice(string_vars)
                statements.append(f"           DISPLAY {target_name}.")
            elif choice == "display-numeric" and numeric_names:
                target = self._rng.choice(numeric_names)
                statements.append(f"           DISPLAY {target}.")
            elif choice == "move-numeric" and numeric_names:
                target = self._rng.choice(numeric_names)
                value = self._rng.randint(0, 10 ** 6 - 1)
                statements.append(f"           MOVE {value} TO {target}.")
            elif choice == "move-string" and string_vars:
                target_name, width = self._rng.choice(string_vars)
                literal = self._random_string_literal(width)
                statements.append(f"           MOVE \"{literal}\" TO {target_name}.")
            else:
                literal = self._random_string_literal(self._rng.randint(3, 10))
                statements.append(f"           DISPLAY \"{literal}\".")
        return statements

    def _random_identifier(self, prefix: str) -> str:
        suffix = "-".join(
            "".join(self._rng.choice(string.ascii_uppercase) for _ in range(self._rng.randint(3, 6)))
            for _ in range(self._rng.randint(1, 2))
        )
        return f"{prefix}-{suffix}"

    def _random_string_literal(self, width: int) -> str:
        length = self._rng.randint(1, max(1, width))
        return "".join(self._rng.choice(STRING_ALPHABET) for _ in range(length)).strip() or "FUZZ"

    def _mutate_sample_program(self) -> str:
        samples = list(SAMPLE_ROOT.rglob("*.cob"))
        if not samples:
            raise SystemExit("Mutation fuzzing requires COBOL samples under samples/cobol/")
        seed_path = self._rng.choice(samples)
        contents = seed_path.read_text(encoding="utf-8")
        mutated = list(contents)
        operations = self._rng.randint(1, max(2, len(mutated) // 32))
        for _ in range(operations):
            if not mutated:
                mutated.append(" ")
            action = self._rng.choice(("replace", "insert", "delete"))
            index = self._rng.randint(0, len(mutated) - 1)
            if action == "replace":
                mutated[index] = self._random_mutation_fragment()[0]
            elif action == "delete" and len(mutated) > 1:
                del mutated[index]
            else:
                fragment = self._random_mutation_fragment()
                mutated[index:index] = list(fragment)
        return "".join(mutated)

    def _random_mutation_fragment(self) -> str:
        if self._rng.random() < 0.3:
            keyword = self._rng.choice(COBOL_KEYWORDS)
            return f"\n           {keyword} "
        return self._rng.choice(STRING_ALPHABET)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Fuzz the COBOL to CBL-C transpiler")
    parser.add_argument("--binary", default="./ctoc_cobol_transpiler", type=Path, help="Path to the transpiler executable")
    parser.add_argument("--iterations", default=DEFAULT_ITERATIONS, type=int,
                        help="Number of iterations per mode")
    parser.add_argument("--mode", choices=VALID_MODES, default=DEFAULT_MODE,
                        help="Which fuzzing strategy to run")
    parser.add_argument("--seed", type=int, default=None, help="Seed for deterministic runs")
    parser.add_argument("--keep-failures", action="store_true",
                        help="Preserve crashing inputs under build/fuzz_failures/")
    parser.add_argument("--timeout", type=float, default=DEFAULT_TIMEOUT, help="Per-run timeout in seconds")
    return parser.parse_args()


def summarize(results: Iterable[FuzzResult]) -> None:
    total = 0
    crashes = 0
    diagnostics = 0
    successes = 0
    for result in results:
        total += 1
        if result.crashed:
            crashes += 1
        elif result.diagnostics:
            diagnostics += 1
        else:
            successes += 1
    print(f"Completed {total} fuzz runs: {successes} succeeded, {diagnostics} reported diagnostics, {crashes} crashed.")
    if crashes:
        print("Crashing inputs were preserved under build/fuzz_failures/.")


def main() -> None:
    args = parse_args()
    modes = VALID_MODES[:-1] if args.mode == "all" else (args.mode,)
    harness = FuzzHarness(args.binary, args.seed, args.keep_failures, args.timeout)
    results = harness.run(args.iterations, modes)
    summarize(results)


if __name__ == "__main__":
    main()
