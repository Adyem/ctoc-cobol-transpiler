# VS Code Support for CBL-C

This directory contains an unofficial TextMate grammar that provides syntax highlighting for `.cblc` files in Visual Studio Code.

## Installation

1. Copy `cblc.tmLanguage.json` into your VS Code user snippets directory, for example:
   * **macOS:** `~/Library/Application Support/Code/User/syntaxes/`
   * **Linux:** `~/.config/Code/User/syntaxes/`
   * **Windows:** `%APPDATA%\Code\User\syntaxes\`
2. If the `syntaxes/` directory does not exist yet, create it manually.
3. Add the following entry to your VS Code `settings.json` so the editor associates `.cblc` files with the grammar:

```json
"files.associations": {
    "*.cblc": "cblc"
},
"editor.semanticHighlighting.enabled": true,
"editor.quickSuggestions": {
    "comments": false,
    "strings": true,
    "other": true
}
```

After reloading VS Code, CBL-C source files gain keyword, type, literal, and comment highlighting. The quick suggestion toggle keeps inline completions enabled for identifiers while leaving comments untouched.

## Extending the Grammar

The grammar intentionally focuses on the constructs that the transpiler currently supports (file declarations, functions, control flow, and runtime helpers). If you introduce new language constructs, update the `keywords` and `types` repositories inside `cblc.tmLanguage.json` accordingly. Reload the window to pick up any changes.

For deeper integration (e.g., diagnostics or formatting), consider wiring the transpiler's CLI into a [VS Code Task](https://code.visualstudio.com/docs/editor/tasks) or using the `--dump-ast` output to drive future language-server experiments.
