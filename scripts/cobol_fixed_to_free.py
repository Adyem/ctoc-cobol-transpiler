#!/usr/bin/env python3
"""Normalize generated fixed-format COBOL continuation lines to free format."""

from __future__ import annotations

import argparse
from pathlib import Path


def normalize(path: Path) -> None:
    source_lines = path.read_text(encoding="utf-8").splitlines()
    output_lines = [">>SOURCE FORMAT IS FREE"]
    for line in source_lines:
        if len(line) >= 7 and line[:6].isspace() and line[6] == "-":
            continuation = line[7:].lstrip()
            if output_lines[-1].endswith((" ", "\t")):
                output_lines[-1] += continuation
            else:
                output_lines[-1] += " " + continuation
        else:
            output_lines.append(line)
    path.write_text("\n".join(output_lines) + "\n", encoding="utf-8", newline="\n")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("paths", nargs="+", type=Path)
    arguments = parser.parse_args()
    for path in arguments.paths:
        normalize(path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
