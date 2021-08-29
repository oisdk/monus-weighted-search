# monus-weighted-search

This is a Haskell package containing the heap monad and some of the examples in the paper 

* "Algebras for weighted search", Donnacha Oisín Kidney and Nicolas Wu. 2021.
  Proc. ACM Program. Lang. 5, ICFP, Article 72 (August 2021), 30 pages.
  DOI:[https://doi.org/10.1145/3473577](https://doi.org/10.1145/3473577)

It contains three main components:

* The `Monus` class (and some instances).
* The `HeapT` monad, as described in the last section of the paper.
* Examples using the `HeapT` monad.

The `Monus` class can be found in `Data.Monus`, `HeapT` is in `Control.Monad.Heap`, and the examples are
in the `MonusWeightedSearch.Examples` directory.

The examples include:

* Dijkstra's algorithm (`MonusWeightedSearch.Examples.Dijkstra`)
* Probabilistic parsing (`MonusWeightedSearch.Examples.Parsing`)
* Probabilistic sampling (`MonusWeightedSearch.Examples.Sampling`)
* The Viterbi algorithm (`MonusWeightedSearch.Examples.Viterbi`)

Thought he primary purpose of this package is to demonstrate the ideas in the
paper, the heap monad has been packaged so it can be used as-is in "real" code.

The documentation is compiled on every commit, it can be seen 
[here](https://oisdk.github.io/monus-weighted-search/docs/).

Benchmarks are run occasionally, their output can be seen 
[here](https://oisdk.github.io/monus-weighted-search/benchmark/results.html).
