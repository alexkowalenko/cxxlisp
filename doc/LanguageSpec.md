# Common Lisp C++ Rewrite

## Lexical Information

Special characters are `(` `)` `.` `"` `'`  `;` \` `,` `,@`, `#`

* `()` marks a list structure.

* `.` marks a cons pair inside a list structure.

* `;` indicates a comment, and input is ignored from the semi-colon until the end of line.

* `#|` and `|#` indicate bounded comments. Anything between the symbols is a comment, including new lines.

* `"` is used to introduce strings.

* `'` introduces a quoted expression - transformed in the parser to `(quote ...)`

* \` introduces a partial-quoted expression - transformed in the parser to `(backquote ...)`

* `,` is used in partial-quoted expression to mark that the next expression is to be evaluated and the result inserted into the form. This is transformed to `unquote` symbol by the parser.

* `,@` is used in partial-quoted expressions to mark that the next expression is to be evaluated and if the expression is a list, to be spliced into the form, else it is inserted. This is transformed to `splice-unquote` by the parser.

* `#` is used to introduce various types and syntax features.
  * `#\c` is used for character type.
  * `#'function` is used for the function type.

## Basic data types

* Symbols - i.e. `t`, `nil`, `long_atom`. Any collection of letters without the special characters can be a symbol. All unicode letters in theory, emoji. Symbols are case sensitive. The following are accepted for symbols:

`- + * / < = > ! ? : $ % _ & ~ ^ \ { }`.

Note that the standard also accepts `@` as part of symbol identifers. `? !` are not recommended.

Symbol `t` is defined as the standard true value, and `nil` is defined as the null or false value. '() is equivalent to `nil`.

* Keywords are symbols which start with `:`. They evaluate to themselves, for use as argument keywords for functions. For example, :keyword => :keyword. Keywords with `&` are used as part of syntax (e.g. function optional arguments `&optional`)

* Lists - i.e., `()` `(a b c)`.

* Cons pair - i.e., `(a . b)`. Not all functions take cons pairs.

* Integers - i.e., `1`, `-2`, `0`. All integers are 64-bit integers.

* Floats - i.e. 1.2, 1.2345e-08. All floats are 64-bit floating-points.

* Strings - i.e., `"this string"`. Strings handle unicode text. 

* Characters - i.e., `#\a`, `#\newline`, `#\space`. Characters handle unicode characters.

* Function references, i.e., `#'atom`.

All types apart from Lists are atomic.

## Basic functions

`(quote x)` - do not evaluate `x`, this also is equivalent to `'x`.

`(backquote x)` - do not evaluate `x` except for `unquote` and `splice-unquote` forms, this also is equivalent to \`x. `unquote` is equivalent to `,` and `splice-unquote` is equivalent to `,@`. Thus the form \`(x ,z) transforms to `(backquote (x unquote z))` in the parser, and the only the symbol `z` is evaluated but the whole expression is returned.

`(eq x y)` - is `t` if x and y are the same object.

`(equal x y)` - is `t` if x and y are the same object, or the characters/strings are identical.

`(equalp x y)` - is `t` if x and y are the same object, or the characters/strings and lists are identical.

# Types

## Symbol Types

* `symbol` - unqualified symbol. The current package and the package use list is searched for symbol resolution. If not defined, assumed to be in the current package.

### Predicates 

`(symbolp x)` - is `x` a symbol, returns `t` if so, `nil` if `x` is not an symbol.

`(keywordp x)` - is `x` a keyword, returns `t` if so, `nil` if `x` is not an symbol.

`(type-of x)` - returns a symbol representing the type of `x`. [Not implemented].

### Functions

`(makunbound s)` - unbinds the symbol `s`.

## Number Types

Supported number types are integers and reals (same as floats).

`(numberp n)` - returns `t` is `n` a number.

`(integerp n)` - returns `t` is `n` a integer.

`(floatp n) (realp n)` - returns `t` is `n` a float.

`pi` - the value π.

### Predicate Functions

`(= n m)` `(/= n m)` - returns `t` or `nil` if `n` and `m` are equal, not equal.

`(< n m)` `(<= n m)` - returns `t` or `nil` if `n` is less, less or equals `m`.

`(> n m)` `(>= n m)` - returns `t` or `nil` if `n` is greater, greater or equals `m`.

`(minusp n)` - returns `t` is `n` is negative.

`(plusp n)` - returns `t` is `n` is positive.

`(zerop n)` - returns `t` is `n` is zero.

`(evenp n)` - returns `t` is `n` is even integer.

`(oddp n)` - returns `t` is `n` is odd integer.

### Arithmetic

`(+ n...)`- returns the sum of all the numbers.

`(* n...)`- returns the product of all the numbers.

`(- n...)`- returns the `n` minus the rest of the numbers. Returns the negative `n` with one argument.

`(\ n...)`- returns the `n` divided by the rest of the numbers. No divisor can be zero.

`(mod n m)` `(rem n m)` - returns the modulus of `n` and `m`.

`(expt n m)` `(^ n m)` - returns the exponent of `n` and `m`. (`^` is not in the common Lisp standard)

`(1+ n)` `(1- n)`- returns increment/decrement of `n`.

`(max n m)` `(min n m)` - returns the maximum/minimum of `n` and `m`.

`(round r)` `(floor r)` `(ceiling r)` `(truncate r)` - returns rounded/floor/ceiling/truncated of real number `r`.

`(abs n)`- returns the absolute of `n`.

`(incf n [interval]) (decf n [interval])` increments or decrements the value represented by the symbol `n`, by the value `interval` or 1 if not present. `n` has to be predefined.

### Floating point functions

`(log n) (exp n)`- returns the natural log of `n`, returns e to the power of `n`.

`(sin n) (cos n) (tan n) (asin n) (acos) (atan)`- returns the trigonometric functions of `n`, based on radians.

`(sqrt n)`- returns the square root of `n`.


## String Types

Supported unicode strings.

`(stringp n)` - returns `t` is `n` a string.

### Predicate Functions

`(string= s1 s2)` `(string-equal s1 s2)` `(string\= s1 s2)` `(string-not-equal s1 s2)` - returns `t` if `s1` is equal/not equal to `s2`.

`(string< s1 s2)` `(string-lessp s1 s2)` `(string> s1 s2)` `(string-greaterp s1 s2)` - returns `t` if `s1` is less/greater to `s2`.

`(string<= s1 s2)` `(string-not-greaterp s1 s2)` `(string>= s1 s2)` `(string-not-lessp s1 s2)` - returns `t` if `s1` is less/greater or equal to `s2`.

`(string-ci= s1 s2)` `(string-ci< s1 s2)` `(string-ci> s1 s2)` `(string-ci<= s1 s2)` `(string-ci>= s1 s2)` - case insensitive variants.

### String Functions

`(string s)` - convert `s` into a string.

`(string-upcase s) (string-downcase s)` - convert `s` into a string, and apply the transformation.

## Character Type

Supported unicode characters.

`(characterp n)` - returns `t` is `n` a character.

### Predicate Functions

`(char= c1 c2)` `(char-equal c1 c2)` `(char\= c1 c2)` `(char-not-equal c1 c2)` - returns `t` if `c1` is equal/not equal to `c2`.

`(char< c1 c2)` `(char-lessp c1 c2)` `(char> c1 c2)` `(char-greaterp c1 c2)` - returns `t` if `c1` is less/greater to `c2`.

`(char<= c1 c2)` `(char-not-greaterp c1 c2)` `(char>= c1 c2)` `(char-not-lessp c1 c2)` - returns `t` if `c1` is less/greater or equal to `c2`.

`(char-ci= c1 c2)` `(char-ci< c1 c2)` `(char-ci> c1 c2)` `(char-ci<= c1 c2)` `(char-ci>= c1 c2)` - case insensitive variants.

## List Functions

### Predicates

`(null x)` - tests to see if x is the empty list or `nil`.

`(consp x)` - test to see if x is a list, and not empty.

`(listp x)` - test to see if x is a list.

`(atom x)`  - is `x` an atom (symbol or integer) or the null list `()`, returns `t` if so, `nil` if `x` is not an atom.

`(member x list)` returns the tail of `list` with the first element matching `x`.

### Lists

`(cons x y)` - return the new cons `(x . y)`.

`(list x ...)` - return a new list of the items `x ...`. Return `nil` if empty.

`(list-length x) (length x)` - return the length of the list `x`.

`(car x) (first x)` - return the first element of the list `x`. If `x` is `nil` return `nil`.

`(cdr x)` `(rest x)` - return the rest of the list `x`. If `x` is `nil` return `nil`.

`(c[ad]r x)` - return the function combination of car/cdr - to 4 levels.

`(second x) (third x) (fourth x)` - return the second, third or fourth element of a list.

`(last x [n])` - return the last (`n`th) element of `x` as a list. [Not implemented]

`(nth n list)` - return the nth element of the `list`. Indexing starts at 0.

`(nth-tail n list)` - return the nth last element of the `list`. Indexing starts at 0.

`(reverse list)` - returns the list in reverse order.

`(append [list]* x)` - appends the lists together, and if x is an atom, adds it to the list.

`(replaca cons x)` - replace the car of `cons` with `x`.

`(replacd cons x)` - replace the cdr of `cons` with `x`.

`(mapcar f list...)`  `(maplist f list...)` - apply the function `f` to the lists `list...`, return as list. `f` is a function or a lambda expression.

`(fold f list a)` - apply the function `f` (taking two elements), to the values of `list`, with initial value `a`. [Not implemented]

# Sequences

Sequences are an abstraction of various types, as thus can be the arguments to functions that work on sequences. Strings, and lists sequences.

## Predicates

`(every fn seq) (notevery fn seq)` - returns `t` if every/not every member of `seq` satisfies the test `f`. `f` is a function reference.

`(some fn seq) (notany fn seq)` - returns `t` if some/not ant member of `seq` satisfies the test `f`. `f` is a function reference.

## Functions

`(length s)` - returns the length of a sequence.

`(elt seq i)` - returns the `i`th element of the sequence.

`(elt-set seq i val)`, `(setf (elt seq i) val)` - sets the`i`th element of the sequence to `val`. `elt-set` not standard common lisp.

`(subseq seq start [end])` - returns a subsequence of `seq` starting at `start`, until the end or optional ending at `end`.

`(copy-seq seq)` - returns a copy of `seq`.

`(fill seq x)` - fill the `seq` with `x`. If `seq` is `string` then `x` must be a character.

`(find x seq)` - find `x` in `seq`. If `seq` is `string` then `x` must be a character.

`(find-if f seq)` `(find-if-not f seq)`-  if `f` returns `t` or does not return `t` for an element in `seq`, return that element. `f` is a function reference or lambda expression.

`(position x seq)` - find the position of `x` in `seq`. If `seq` is `string` then `x` must be a character.

`(position-if f seq)` `(position-if-not f seq)`- if `f` returns `t`/ or not `t` return the position of that element. If `seq` is `string` then `x` must be a character.

`(make-sequence type-name size [:initial-element c])` - constructs a new sequence of type `type-name`, and size `size`. Optional initial elements `c` to be filled in the sequence.

# Functions

## Predicates 

`(functionp x)` - is `x` a function reference or lambda expression?

`(fboundp x)` - is `x` a bound function or macro expression?

## Value generation functions

`(defconstant name val [doc string])` - set `name` to `val` in global symbol table, `val` is evaluated. This value is to considered as a constant.

`(defvar name val [doc string])` - set `name` to `val`, `val` is evaluated.

`(defparameter name val [doc string])` - set `name` to `val` in global symbol table, `val` is evaluated. The difference between `defvar` and `defparameter`, is that a change to `defparameter` is considered to be a change to the program, not a change by the program.

`(setq name val ...)` - set `name` to `val`, `val` is evaluated, and returned.

`(setf name val ...)` - set `name` to `val`, `val` is evaluated, and returned.

## Function generation

`(defun f (args ...) body...)` - define a function `f` with `args` and the `body` to be evaluated. Returns the value of the last in `body` or `nil` if not present.

`(lambda (args ...) body...)` - define a anonymous function with `args` and the `body` to be evaluated. Returns the value of the last in `body` or `nil` if not present.

`(funcall f args...)` - apply the arguments `args...` to the function `f`. If `args` is a single list, this becomes the list of arguments. `f` is a function reference or a lambda expression.

`(apply f args)` - apply the list of arguments `args` to the function `f`. `f` is a function reference or a lambda expression.

`(fmakunbound f)` - unbinds the function `f`.

`(identity x)` - returns `x` (identity function).

`(constantly x)` - returns a function that always returns `x` regardless of argument(s).

`(complement x)` - returns a function that always returns the opposite predicate of `x`.

## Macro generation

`(demacro f (args ...) body)` - define a macro `f` with `args` and the `body`. Returns the macro expansion and then evaluation of `body` or `nil` if not present.

`(macro (args ...) body)` - return a anonymous macro with `args` and the `body`.

### Argument formats

The list of arguments to function, lambda, macro definitions can be a sequence of symbols with the following special keywords:

* `&optional` - the following arguments are optional. If a symbol is present then if the argument is missing, `nil` is assigned to the symbol. If a two-member list is present, and if the argument is missing the second value is assigned to the list as a default value.
* `&rest` - the rest of the arguments are placed into the following parameter of the function. If there are no more, then the value of the parameter is `nil`.

# Program Control

`(not x)` - returns `t` if x is `nil` else returns `nil`.

`(and x y ...)` - Returns `t` if all the arguments evaluate to `t`. If one evaluates to `nil` return `nil` immediately.

`(or x y ...)` - Returns `nil` if all the arguments evaluate to `nil`. If one evaluates to `t` return `t` immediately.

`(if cond then [else])` - Returns the value of `then` if `cond` is `t`, else returns the value of `else` if present, or `nil`.

`(cond [(test then)]*)` - Returns the value of then if `test` is `t` for the first pair with a `test` that evaluates to `t`. Returns `nil` otherwise.

`(when test x ...) (unless test x ...)` - Returns the value of the last element, if `test` is `t` for `when`, or `test` is `nil` for `unless`.

`(progn x ...)` - Evaluates the list of arguments, returns the value of the last one.

`(prog1 x ...)` - Evaluates the list of arguments, returns the value of the first one.

`(dotimes (var limit [result]) expr expr ...)` - Performs the sequence of expressions, with `var` starting at 0, until `limit`-1, returning the value of `result` if present or `nil`. `result` needs to be assigned to in the evaluations. 

`(dolist (var list [result]) expr expr ...)` - Performs the sequence of expressions, with `var` bound to the successive elements of `list`, returning the value of `result` if present or `nil`. `result` needs to be assigned to in the evaluations.

# Programming functions

`(error msg)` - throws an error `msg` and returns to the REPL, or exits the runtime with an error.

`(quit [n])` - stops the runtime, and exits with code `n` if provided or 0.

# Running the Executable

These are the options in running the executable:

* `-h --help` Show program options and quits.
* `-s --silent` Silent operation, don't print the prompt.
* `-r --noreadline` on't use readline input stream (used for testing and batch operation).
* `-p` only parse input and print result.

* `-D string` Debug options, `string` contains the characters:
  * `e` print compilation and execution.

# Diferences to Common Lisp

Apart from what is not implemented.

* Don't support `#'(lambda ....)`. Use `(lambda ...)` directly.
```
(mapcar #'(lambda (x) (micro-eval x environment))
		     (rest form))
```
Should have the function reference removed, i.e.:
```
(mapcar (lambda (x) (micro-eval x environment))
		     (rest form))
```
  