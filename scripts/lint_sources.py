#!/usr/bin/env python3
"""Repository lint checks for CI pipelines."""

import os
import sys


def should_examine(path):
    """Determine whether a path should be scanned for whitespace issues."""
    if path.endswith(".cpp"):
        return True
    if path.endswith(".hpp"):
        return True
    if path.endswith(".c"):
        return True
    if path.endswith(".h"):
        return True
    if path.endswith(".md"):
        return True
    if path.endswith(".txt"):
        return True
    if path.endswith(".cob"):
        return True
    if path.endswith(".cblc"):
        return True
    if os.path.basename(path) == "Makefile":
        return True
    if path.endswith(".mk"):
        return True
    return False


def is_skipped_dir(name):
    """Return True when a directory should not be traversed."""
    if name.startswith("."):
        return True
    if name == "objs":
        return True
    if name == "objs_debug":
        return True
    if name == "objs_tests":
        return True
    if name == "libft":
        return True
    if name == "data":
        return True
    return False


def collect_files(root):
    """Gather candidate file paths starting at the provided root."""
    pending = [root]
    results = []
    while len(pending) > 0:
        current = pending.pop()
        try:
            entries = os.listdir(current)
        except OSError:
            continue
        index = 0
        count = len(entries)
        while index < count:
            entry = entries[index]
            index += 1
            full_path = os.path.join(current, entry)
            if os.path.isdir(full_path):
                if not is_skipped_dir(entry):
                    pending.append(full_path)
                continue
            if should_examine(full_path):
                results.append(full_path)
    return results


def check_file(path):
    """Scan a file for trailing whitespace and tab characters."""
    issues = []
    allow_tabs = os.path.basename(path) == "Makefile"
    try:
        handle = open(path, "r", encoding="utf-8")
    except OSError as error:
        issues.append(f"{path}: unable to open file ({error})")
        return issues
    try:
        lines = handle.readlines()
    except OSError as error:
        handle.close()
        issues.append(f"{path}: unable to read file ({error})")
        return issues
    handle.close()
    index = 0
    total = len(lines)
    while index < total:
        line = lines[index]
        index += 1
        line_number = index
        stripped_line = line.rstrip("\n")
        stripped_line = stripped_line.rstrip("\r")
        no_trailing = stripped_line.rstrip(" \t")
        if stripped_line != no_trailing:
            issues.append(f"{path}:{line_number}: trailing whitespace detected")
        if not allow_tabs:
            char_index = 0
            length = len(stripped_line)
            while char_index < length:
                if stripped_line[char_index] == "\t":
                    issues.append(f"{path}:{line_number}: tab character found")
                    break
                char_index += 1
    return issues


def run():
    """Execute lint checks across the repository."""
    root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    files = collect_files(root)
    index = 0
    problems = []
    total = len(files)
    while index < total:
        path = files[index]
        index += 1
        issues = check_file(path)
        if len(issues) > 0:
            issue_index = 0
            issue_count = len(issues)
            while issue_index < issue_count:
                problems.append(issues[issue_index])
                issue_index += 1
    if len(problems) > 0:
        problem_index = 0
        problem_count = len(problems)
        while problem_index < problem_count:
            sys.stderr.write(problems[problem_index] + "\n")
            problem_index += 1
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(run())
