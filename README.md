Alonzo
======

A simple CLI for working with [lambda calculus][1] expressions.

## Usage

```bash
# Syntax check an expression
alonzo -c [expression]

# Simplify an expression
alonzo [expression]

# Simplify an expression, showing each β-reduction step
alonzo -v [expression]

# Identify free variables in an expression
alonzo -f [expression]

# Identify bound variables in an expression
alonzo -b [expression]
```

## Syntax

Taken from [_An Introduction to Functional Programming Through Lambda Calculus_][2].

```
<expression> ::= <name> | <function> | <application>

<name> ::= [a-z]+(-[a-z]+)*

<function> ::= λ <name> . <body>
  <body> ::= <expression>

<application> ::= ( <function expression> <argument expression> )
  <function expression> ::= <expression>
  <argument expression> ::= <expression>
```

Whitespace is not required except in `<application>`, to separate the `<function expression>` from the `<argument expression>`.

[1]: https://en.wikipedia.org/wiki/Lambda_calculus
[2]: http://www.amazon.com/Introduction-Functional-Programming-Calculus-Mathematics/dp/0486478831

