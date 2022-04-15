# Hisp

Hisp is a dialect of Lisp, with an interpreter written in Haskell.

# Data types
Hisp has 7 data types:

- `int` written like `125`, `0`
- `bool` written like `true`, `false`
- `string` written like `"hello"`, `""`
- `list` written like `(1 2 3)`, `(print 5)`, `("foo" "bar")`
- `nil`, with the ony value `nil`
- `symbol`, written like `foo`, `a`
- `function`, created using the `fn` built-in

# Evaluation

Hisp is evaluated in typical Lisp fashion:
- Lists are evaluated as function calls, with the first element being the function and the rest being the function arguments.
Evaluating a list where the first element is not a function will cause an error to be thrown.
- Symbols are evaluated by looking up, in order:
    - Any function arguments in lexical scope
    - Any symbols in global scope (bound using `def`)
    - Any built-ins

    If no mapping for the symbol is found, an error will be thrown.
- All other types of values are evaluated to themselves

The `'` built-in can be used to avoid evaluating values. Inside a `'`, `~` can be used to force evaluation of values.

## Built-ins

### core
- `def` Bind a symbol to a value in global scope
- `fn` Define a function
- `macro` Define a macro
- `'` Quote a value, causing it to not be evaluated
- `~` Unquote a value, causing it to be evaluated. Only valid inside a quotation
- `eval` Evaluate a value
- `error` Throw an error
- `apply` Apply a list of length n to an n-arity function
- `import` Import another hisp file 
- `type` Return a string representing the type of the value

### I/O
- `print` Print to stdout
- `read-file` Read a file into a string

### arithmetic
Perform arithmetic on integers

- `+`
- `-`
- `*`
- `/`

### comparison
- `>` Only valid for integers
- `=` Valid for any value

### logic
- `nand` Logical NAND operation, only valid for booleans

### control flow
- `if` Return second argument if first is `true`, return third argument if first is `false` or `nil`

### lists
- `cons` Prepend a value to a list
- `head` Return the first value in a list. Errors on empty list.
- `tail` Return list without the first value

### strings
- `str` Turn any number of values into a single string
- `split` Split a string into a list of characters
