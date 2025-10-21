# IDE Integration Guide

CBL-C development benefits from lightweight editor support for syntax highlighting and task automation. This guide outlines the current recommendations for Visual Studio Code and describes how to hook the transpiler into other editors.

## Visual Studio Code

1. Copy the TextMate grammar stored at [`editors/vscode/cblc.tmLanguage.json`](../editors/vscode/cblc.tmLanguage.json) into your VS Code user `syntaxes/` directory. The README in [`editors/vscode/`](../editors/vscode/README.md) lists platform-specific paths.
2. Associate the `.cblc` extension with the grammar by adding the snippet below to your `settings.json`:
   ```json
   "files.associations": {
       "*.cblc": "cblc"
   },
   "editor.semanticHighlighting.enabled": true
   ```
3. Reload the editor. CBL-C keywords, declarations, literals, and runtime helper calls now inherit familiar C-like highlighting. Enable "Format on Save" if you pair this setup with the `cblc_formatter` binary.
4. Optional: add a VS Code Task that runs `make test` or invokes `./ctoc_cobol_transpiler` so you can trigger transpilation directly from the editor.

## Vim / Neovim

The generated TextMate grammar also works with [`bat`](https://github.com/sharkdp/bat) and [`tree-sitter`](https://tree-sitter.github.io/tree-sitter/) bridges such as [`nvim-treesitter`](https://github.com/nvim-treesitter/nvim-treesitter). Convert the grammar to a Tree-sitter bundle using [`jsontoml`](https://github.com/tree-sitter/tree-sitter/tree/master/cli#grammar-conversion) and register it under the `cblc` filetype. Map `*.cblc` buffers to the new highlighter via an autocommand:

```vim
augroup cblc_highlight
    autocmd!
    autocmd BufRead,BufNewFile *.cblc set filetype=cblc
augroup END
```

Pair the highlighting with `:make` integration by adding a project-local `.vimrc` that runs `make test` or the forward transpilation target.

## Emacs

For Emacs users, leverage [`tree-sitter-langs`](https://github.com/emacs-tree-sitter/tree-sitter-langs) or [`generic-mode`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Generic-Modes.html) to register the same keyword set. A minimal major mode can reuse the keyword and type lists from the VS Code grammar:

```elisp
(define-generic-mode 'cblc-mode
  '("//")
  '("file" "function" "record" "if" "else" "while" "return" "open" "close" "read" "write" "display")
  '(("\"\\(?:\\\\.\|[^\"]\)*\"" . font-lock-string-face)
    ("\\b[0-9]+\\(?:\\.[0-9]+\\)?\\b" . font-lock-constant-face))
  '("\\.cblc\\'")
  nil)
```

Add the mode to your configuration to activate it automatically:

```elisp
(add-to-list 'auto-mode-alist '("\\.cblc\\'" . cblc-mode))
```

## Next Steps

This initial editor support focuses on rapid feedback while writing CBL-C. Future work will explore:

- Surfacing diagnostics via `ctoc_cobol_transpiler --dump-ast` and the existing semantic error reporting.
- Auto-completion hooks that consult the transpiler's module and record registries.
- Language-server scaffolding so IDEs can request formatting, goto-definition, and hover metadata.

Contributions that expand on these ideas should update this guide and the `editors/` directory so new users can opt in easily.
