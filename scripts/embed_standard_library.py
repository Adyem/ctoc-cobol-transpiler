#!/usr/bin/env python3
"""Embed the native CBL-C standard-library sources into a C++ header."""

from __future__ import annotations

import argparse
import json
from pathlib import Path


def program_name(path: Path) -> str:
    stem = path.stem.upper().replace("_", "-")
    return "CBLC-" + stem


def c_string_lines(text: str) -> list[str]:
    lines = text.splitlines(keepends=True)
    if not lines:
        lines = [""]
    return ["    " + json.dumps(line) for line in lines]


def generate(source_dir: Path, output: Path) -> None:
    sources = sorted(source_dir.glob("*.cblc"), key=lambda item: item.name.lower())
    if not sources:
        raise SystemExit(f"no .cblc sources found in {source_dir}")

    output.parent.mkdir(parents=True, exist_ok=True)
    chunks: list[str] = [
        "#pragma once",
        "",
        "#include <cstddef>",
        "",
        "struct s_embedded_standard_library_source",
        "{",
        "    const char *program_name;",
        "    const char *source;",
        "};",
        "",
    ]
    entries: list[str] = []
    for index, source_path in enumerate(sources):
        symbol = f"g_embedded_standard_library_source_{index}"
        chunks.append(f"static const char {symbol}[] =")
        chunks.extend(c_string_lines(source_path.read_text(encoding="utf-8")))
        chunks.append("    \"\";")
        chunks.append("")
        entries.append(f'    {{"{program_name(source_path)}", {symbol}}}')
    chunks.extend(
        [
            "static const s_embedded_standard_library_source",
            "    g_embedded_standard_library_sources[] = {",
            ",\n".join(entries),
            "};",
            "",
            "static const std::size_t g_embedded_standard_library_source_count =",
            "    sizeof(g_embedded_standard_library_sources) / sizeof(g_embedded_standard_library_sources[0]);",
            "",
        ]
    )
    output.write_text("\n".join(chunks), encoding="utf-8", newline="\n")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source-dir", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    arguments = parser.parse_args()
    generate(arguments.source_dir, arguments.output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
