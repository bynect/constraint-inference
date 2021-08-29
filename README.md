# Constraint inference

This repository contains an indipendent implementation of the type
inference algorithm described in the paper
[Generalizing Hindley-Milner Type Inference Algorithms](#ref1).
The paper [Constraint Based Type Inferencing in Helium](#ref2) describes
similar forms of type constraint.

## Implementation

All the code contained in this repository is public domain.

There may be small divergences between this implementation and
the paper pseudo-implementation.

This repo contains the following branches:

- [main](https://github.com/bynect/constraint-inference/tree/main) Original algorithm implementation with some additions, unstable.
- [original](https://github.com/bynect/constraint-inference/tree/original) Original algorithm implementation with detailed comments.
- [clean](https://github.com/bynect/constraint-inference/tree/clean) A copy of the original branch without the comments.
- [parser](https://github.com/bynect/constraint-inference/tree/parser) A copy of the original branch with the addition of a parser and repl.
- [if-expr](https://github.com/bynect/constraint-inference/tree/if-expr) Same as the parser branch with the addition of n-tuples and if expressions.

## Grammar

The `main` branch contains a parser and a little repl to check what types is inferred to an expression.
Additionally, the `main` branch extends the algorithm described in the paper with n-tuples and if expressions.
There is also the addition of a limited form of recursive bindings.

```txt
E ::=
  | x
  | E1 E2
  | \x -> E
  | let x = E1 in E2
  | rec x = E1 in E2
  | E1, ..., En
  | lit
```

### Example

The factorial of 10 implemented using a recursive binding.

Note: The current implementation of recursive bindings is not correct. However the type of the factorial function is inferred correctly.

```txt
rec fact = \n -> if eq n 1
		then 1
		else mul n (fact (sub n 1))
in fact 10
```

## References

[<a id="ref1">[1][paper-1]</a>] Bastiaan Heeren, Jurriaan Hage, and Doaitse Swierstra.
Generalizing Hindley-Milner Type Inference Algorithms. Institute of Information and Computing Sciences,
Utrecht University, Netherlands, 2002.

[<a id="ref2">[2][paper-2]</a>] Bastiaan Heeren, Jurriaan Hage, and S. Doaitse Swierstra.
Constraint Based Type Inferencing in Helium. Institute of Information and Computing Science,
Universiteit Utrecht, Netherlands, 2003.

[paper-1]: http://www.cs.uu.nl/research/techreps/repo/CS-2002/2002-031.pdf
[paper-2]: http://www.open.ou.nl/bhr/heeren-cp03.pdf
