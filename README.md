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

- [main](https://github.com/bynect/constraint-inference/tree/main) Original algorithm implementation with some additions (parser and repl)
- [original](https://github.com/bynect/constraint-inference/tree/original) Original algorithm implementation with detailed comments
- [clean](https://github.com/bynect/constraint-inference/tree/clean) A copy of the original branch without the comments

## References

[<a id="ref1">[1][paper-1]</a>] Bastiaan Heeren, Jurriaan Hage, and Doaitse Swierstra.
Generalizing Hindley-Milner Type Inference Algorithms. Institute of Information and Computing Sciences,
Utrecht University, Netherlands, 2002.

[<a id="ref2">[2][paper-2]</a>] Bastiaan Heeren, Jurriaan Hage, and S. Doaitse Swierstra.
Constraint Based Type Inferencing in Helium. Institute of Information and Computing Science,
Universiteit Utrecht, Netherlands, 2003.

[paper-1]: http://www.cs.uu.nl/research/techreps/repo/CS-2002/2002-031.pdf
[paper-2]: http://www.open.ou.nl/bhr/heeren-cp03.pdf
