# CBL-C Grammar Specification

This document defines the authoritative grammar for the CBL-C language consumed by the transpiler.  The notation is a relaxed Extended Backus–Naur Form (EBNF) tuned for readability.

* `::=` introduces a production rule.
* `|` separates alternatives.
* `[]` denotes an optional element.
* `{ item }` denotes zero or more repetitions of `item`.
* Literal tokens appear in single quotes.
* Uppercase names refer to terminal tokens emitted by the lexer.

The grammar captures the subset required for the forward (CBL-C → COBOL) and reverse (COBOL → CBL-C) compilation pipelines.  Any constructs outside these rules are rejected by the parser until future versions expand the syntax.

---

## 1. Program Structure

```
program             ::= file_section record_section procedure_section
file_section        ::= { file_declaration }
record_section      ::= { record_declaration }
procedure_section   ::= { statement }
```

A source file is a flat sequence: file declarations, record declarations, then executable statements.  Declarations may be interleaved but appear before the first executable statement.

---

## 2. File Declarations

```
file_declaration    ::= 'file' identifier file_role string_literal file_storage_spec ';'
file_role           ::= 'input' | 'output' | 'data'
file_storage_spec   ::= [ file_fixed_record_spec ]
file_fixed_record_spec ::= 'fixed' '(' INTEGER_LITERAL ')'
```

* `input` and `output` default to line-sequential text files.
* `data fixed(n)` declares a fixed-length binary record file.

---

## 3. Record Declarations

```
record_declaration  ::= 'record' identifier '{' { record_field } '}' ';'
record_field        ::= scalar_type identifier array_suffix ';'
scalar_type         ::= 'int' | 'char' | 'bool'
array_suffix        ::= '[' INTEGER_LITERAL ']' | ε
```

Records define COBOL group items.  Arrays map to `PIC X(n)` when `char` and to equivalent numeric storage when `int`.

---

## 4. Statements

```
statement           ::= assignment_statement
                      | if_statement
                      | while_statement
                      | perform_statement
                      | call_statement
                      | open_statement
                      | close_statement
                      | read_statement
                      | write_statement
                      | display_statement
                      | return_statement
                      | ';'

assignment_statement ::= identifier assignment_tail
assignment_tail     ::= '=' expression ';'

if_statement        ::= 'if' '(' expression ')' statement [ 'else' statement ]
while_statement     ::= 'while' '(' expression ')' statement
perform_statement   ::= 'perform' identifier [ '(' argument_list ')' ] ';'
call_statement      ::= identifier '(' argument_list ')' ';'
open_statement      ::= 'open' '(' identifier ',' string_literal ')' ';'
close_statement     ::= 'close' '(' identifier ')' ';'
read_statement      ::= 'read' '(' identifier ',' identifier ')' ';'
write_statement     ::= 'write' '(' identifier ',' identifier ')' ';'
display_statement   ::= 'display' '(' expression ')' ';'
return_statement    ::= 'return' [ expression ] ';'

argument_list       ::= [ expression { ',' expression } ]
```

Single `;` lines permit empty statements that simplify parser error recovery.

---

## 5. Expressions

```
expression                  ::= logical_or_expression
logical_or_expression       ::= logical_and_expression { '||' logical_and_expression }
logical_and_expression      ::= equality_expression { '&&' equality_expression }
equality_expression         ::= relational_expression { equality_operator relational_expression }
equality_operator           ::= '==' | '!='
relational_expression       ::= additive_expression { relational_operator additive_expression }
relational_operator         ::= '<' | '<=' | '>' | '>='
additive_expression         ::= multiplicative_expression { additive_operator multiplicative_expression }
additive_operator           ::= '+' | '-'
multiplicative_expression   ::= unary_expression { multiplicative_operator unary_expression }
multiplicative_operator     ::= '*' | '/' | '%'
unary_expression            ::= unary_operator unary_expression | primary_expression
unary_operator              ::= '!' | '-'
primary_expression          ::= literal
                              | identifier
                              | call_expression
                              | '(' expression ')'
call_expression             ::= identifier '(' argument_list ')'
identifier                  ::= IDENTIFIER
literal                     ::= INTEGER_LITERAL | STRING_LITERAL | CHAR_LITERAL | 'true' | 'false'
```

The grammar delegates to the lexer for tokenization of identifiers, integers, strings, and character literals.

---

## 6. Lexical Summary

* `IDENTIFIER` tokens follow the lexer rules documented in `docs/cblc_sample_inventory.md`.
* `INTEGER_LITERAL` values are base-10 without separators; negative numbers use unary `-`.
* `STRING_LITERAL` supports escape sequences handled by the runtime helpers.
* `CHAR_LITERAL` wraps a single character or escape sequence in single quotes.
* Keywords are reserved and cannot be used as identifiers.

---

## 7. Future Extensions

The grammar is intentionally conservative.  Upcoming milestones will extend the productions with:

* Additional statement forms (e.g., `for`, `switch`).
* Enhanced file declarations (collating sequences, optional ORGANIZATION overrides).
* Numeric picture clauses and OCCURS tables for complex records.
* Paragraph labels and sections for COBOL interoperability.

All changes should update this specification and add parser tests that ensure the new forms round-trip through the compiler pipeline.
