# Coding Guidelines

These rules apply to all files in this repository. They are adapted from the libft project's guidance, trimmed to the conventions that are relevant here.

* Use `return ;` for void functions, with a space before the semicolon.
* Use `return (value);` for non-void returns, with a space before the opening parenthesis.
* Do not use `for` loops, ternary operators, or `switch` statements.
* Indent code using 4 spaces per level.
* Use Allman style braces (opening brace on a new line).
* Function and variable names must use `snake_case`, and prefer descriptive words over single-letter identifiers.
* When introducing classes, indent access specifiers by 4 spaces, indent member declarations within them by 8 spaces, place private members before public ones, and separate those sections with an empty line.
* Within classes, prefix member variables with `_` and access members through `this`.
* Implement class member functions outside the class definition, placing declarations in `.hpp` files and definitions in corresponding `.cpp` files.
* Every class must declare and define a constructor and destructor, even if they simply `return ;`.
* Track class errors with a mutable `_error_code` member, a private `set_error` helper that updates `ft_errno`, and public `get_error` / `get_error_str` accessors.
* Only `.cpp` files should be prefixed with the module name; `.hpp` files do so only when they are for internal use.
* Isolate any platform-specific behavior inside helper functions in a `compatibility` module.
* When calling libft helpers, rely only on functions declared in headers that are not marked as internal.
