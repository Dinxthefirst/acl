# ACL - a coding language
A programming language using the hindly-milner type system

## Features
The language is built on lambda calculus with let-polymorphism. 
Syntactic sugar for function binding in the let expression and binary/unary operations is also added.


### Examples
A lambda abstraction is of the form
```
x -> x + 1
```
which translates to $\lambda x . x + 1$ in lambda calculus.

Lambda abstractions body can contain additional lambda abstractions
```
x -> y -> x + y
```
which translates to $\lambda x. \lambda y. x + y$.

Let expressions can bind variables
```
let x = 1 in x
```
Let expressions can also bind lambda abstractions
```
let inc = x -> x + 1 in inc
```
which is the function which adds two integers together.

The function can also have the variables of the lambda abstraction as arguments
```
let inc' x = x + 1 in inc'
``
which is equivalent to `inc`.

Application is of the form
```
inc 1
```
which increments one and results in the value `2`. In pure lambda calculus with binary operations it is equivalant to
```
(x -> x + 1) 1
```

Let expressions can also be polymorphic
```
let id x = x in id
```
This function returns function which returns itself no matter the type of the argument of the function.


## Tools
Built using ocaml and using the dune build system for ocaml. 
The Menhir LR(1) parser is used to turn program text into AST.

## Develop using Dune
To the project
```
dune build
```
To test the project
```
dune test
```
To type check a file
```
dune exec sem [FILE]
```
To run a file
```
dune exec run [FILE]
```

## Devolop using Nix
To enter the devolpment shell
```
nix develop .
```
Then all dune commands are avaible to use

## Further Work
- Recursive types (`let rec`)
- Lists
