#!/usr/bin/env python3
"""Aggregate gcov coverage results and enforce minimum thresholds."""

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Optional, Sequence, Tuple


@dataclass
class CoverageRecord:
    path: Path
    lines_percent: float
    lines_total: int
    branches_percent: float
    branches_total: int

    @property
    def lines_executed(self) -> float:
        return (self.lines_percent / 100.0) * self.lines_total

    @property
    def branches_executed(self) -> float:
        return (self.branches_percent / 100.0) * self.branches_total


FILE_RE = re.compile(r"^File '(.+)'$")
LINES_RE = re.compile(r"^Lines executed:(\d+(?:\.\d+)?)% of (\d+)")
BRANCHES_RE = re.compile(r"^Branches executed:(\d+(?:\.\d+)?)% of (\d+)")
CREATED_RE = re.compile(r"^Creating '(.+\.gcov)'$")


def parse_arguments(argv: Sequence[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--object-dir",
        action="append",
        dest="object_dirs",
        default=None,
        help="Directory containing *.gcno/*.gcda files (repeat for multiple directories).",
    )
    parser.add_argument(
        "--threshold-lines",
        type=float,
        default=80.0,
        help="Minimum acceptable line coverage percentage.",
    )
    parser.add_argument(
        "--threshold-branches",
        type=float,
        default=65.0,
        help="Minimum acceptable branch coverage percentage.",
    )
    parser.add_argument(
        "--include-tests",
        action="store_true",
        help="Include files under tests/ in the aggregate results (default: disabled).",
    )
    return parser.parse_args(argv)


def locate_gcov() -> Path:
    gcov_path = shutil.which("gcov")
    if not gcov_path:
        raise RuntimeError("gcov executable not found. Ensure gcc is installed and gcov is available on PATH.")
    return Path(gcov_path)


def discover_gcno_files(object_dirs: Iterable[Path]) -> List[Path]:
    gcno_files: List[Path] = []
    for directory in object_dirs:
        if not directory.exists():
            raise RuntimeError(
                f"Object directory '{directory}' does not exist. Build the project with COVERAGE=1 before running coverage checks."
            )
        gcno_files.extend(directory.rglob("*.gcno"))
    return gcno_files


def should_include(rel_path: Path, include_tests: bool) -> bool:
    parts = rel_path.parts
    if "libft" in parts:
        return False
    if not include_tests and "tests" in parts:
        return False
    return rel_path.suffix in {".c", ".cc", ".cpp", ".cxx"}


def run_gcov(gcov: Path, repo_root: Path, gcno_path: Path) -> Tuple[str, str]:
    process = subprocess.run(
        [str(gcov), "-b", "-c", "-o", str(gcno_path.parent), str(gcno_path)],
        cwd=repo_root,
        capture_output=True,
        text=True,
    )
    if process.returncode != 0:
        raise RuntimeError(f"gcov failed for {gcno_path} with exit code {process.returncode}:\n{process.stderr}")
    return process.stdout, process.stderr


def normalize_path(repo_root: Path, raw_path: str) -> Optional[Path]:
    candidate = Path(raw_path)
    try:
        if candidate.is_absolute():
            return candidate.resolve().relative_to(repo_root)
        return (repo_root / candidate).resolve().relative_to(repo_root)
    except FileNotFoundError:
        return None
    except ValueError:
        return None


def parse_gcov_output(stdout: str, repo_root: Path, include_tests: bool) -> Tuple[List[CoverageRecord], List[Path]]:
    records: List[CoverageRecord] = []
    generated: List[Path] = []
    current_rel: Optional[Path] = None
    lines_percent: float | None = None
    lines_total: int | None = None
    branches_percent: float | None = None
    branches_total: int | None = None

    def finalize_current() -> None:
        nonlocal current_rel, lines_percent, lines_total, branches_percent, branches_total
        if current_rel is None or lines_percent is None or lines_total is None:
            return
        rel_path = current_rel
        if not should_include(rel_path, include_tests):
            return
        branch_percent = 100.0 if branches_percent is None else branches_percent
        branch_total = 0 if branches_total is None else branches_total
        records.append(
            CoverageRecord(
                path=repo_root / rel_path,
                lines_percent=lines_percent,
                lines_total=lines_total,
                branches_percent=branch_percent,
                branches_total=branch_total,
            )
        )

    for raw_line in stdout.splitlines():
        line = raw_line.strip()
        file_match = FILE_RE.match(line)
        if file_match:
            finalize_current()
            current_rel = normalize_path(repo_root, file_match.group(1))
            lines_percent = None
            lines_total = None
            branches_percent = None
            branches_total = None
            continue

        if current_rel is None:
            created_match = CREATED_RE.match(line)
            if created_match:
                generated_path = repo_root / created_match.group(1)
                generated.append(generated_path)
            continue

        match = LINES_RE.match(line)
        if match:
            lines_percent = float(match.group(1))
            lines_total = int(match.group(2))
            continue

        match = BRANCHES_RE.match(line)
        if match:
            branches_percent = float(match.group(1))
            branches_total = int(match.group(2))
            continue

        created_match = CREATED_RE.match(line)
        if created_match:
            generated_path = repo_root / created_match.group(1)
            generated.append(generated_path)

    finalize_current()
    return records, generated


def collect_coverage_records(
    gcov: Path, repo_root: Path, gcno_files: Iterable[Path], include_tests: bool
) -> Tuple[List[CoverageRecord], List[Path]]:
    all_records: List[CoverageRecord] = []
    generated_files: List[Path] = []
    for gcno_path in gcno_files:
        stdout, _ = run_gcov(gcov, repo_root, gcno_path)
        records, produced = parse_gcov_output(stdout, repo_root, include_tests)
        all_records.extend(records)
        generated_files.extend(produced)
    return all_records, generated_files


def aggregate(records: Sequence[CoverageRecord]) -> Tuple[float, float]:
    total_lines = sum(record.lines_total for record in records if record.lines_total > 0)
    executed_lines = sum(record.lines_executed for record in records if record.lines_total > 0)
    total_branches = sum(record.branches_total for record in records if record.branches_total > 0)
    executed_branches = sum(record.branches_executed for record in records if record.branches_total > 0)

    line_coverage = 100.0 if total_lines == 0 else (executed_lines / total_lines) * 100.0
    branch_coverage = 100.0 if total_branches == 0 else (executed_branches / total_branches) * 100.0
    return line_coverage, branch_coverage


def cleanup_temp_files(files: Iterable[Path]) -> None:
    for file_path in files:
        try:
            file_path.unlink()
        except FileNotFoundError:
            continue


def main(argv: Sequence[str]) -> int:
    args = parse_arguments(argv)
    repo_root = Path(__file__).resolve().parents[1]
    object_dirs = args.object_dirs or ["objs"]
    resolved_dirs = [repo_root / Path(directory) for directory in object_dirs]

    gcov = locate_gcov()
    gcno_files = discover_gcno_files(resolved_dirs)
    if not gcno_files:
        raise RuntimeError("No .gcno files found. Run the build with COVERAGE=1 and execute the tests before collecting coverage.")

    records, generated_files = collect_coverage_records(gcov, repo_root, gcno_files, args.include_tests)
    cleanup_temp_files(generated_files)

    if not records:
        raise RuntimeError("No coverage data collected. Verify that the project sources were compiled with coverage instrumentation.")

    line_coverage, branch_coverage = aggregate(records)
    print(f"Line coverage: {line_coverage:.2f}%")
    print(f"Branch coverage: {branch_coverage:.2f}%")

    failures: List[str] = []
    if line_coverage + 1e-9 < args.threshold_lines:
        failures.append(
            f"Line coverage {line_coverage:.2f}% is below the required threshold of {args.threshold_lines:.2f}%."
        )
    if branch_coverage + 1e-9 < args.threshold_branches:
        failures.append(
            f"Branch coverage {branch_coverage:.2f}% is below the required threshold of {args.threshold_branches:.2f}%."
        )

    if failures:
        for failure in failures:
            print(failure, file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    try:
        sys.exit(main(sys.argv[1:]))
    except RuntimeError as exc:
        print(str(exc), file=sys.stderr)
        sys.exit(1)
