# Interpreter 
- Make type tag in custom types something else than just a string?
- More hygienic macros
- Namespace-private definitions
- Only import each file/module once globally
- Nested destructuring
- Floats
- CLI
- Better REPL
- Actual macros, that only run once on module import?
- More I/O actions
- Catching errors

# Stdlib
- Smarter impl matching for methods:
    - choose impl with the most matching args. If two has equal amount, fail?
    - if impl with same types already exists, fail
- Vectors
- Sets
- More general collection functions based on `reduce` (`=`, `empty?`, `to-list` etc.)
- Channels