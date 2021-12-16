![SATan](./SATAN.png)

# SATan

SATan is a solver for the Boolean Satisfiability Problem (SAT) written in Haskell. It contains the following features:
- DPLL-based procedure
- Optimized unit propagation with 2 Watched Literals (2WL)
- Heat-based static variable selection heuristic (inspired by SATZOO)
- Reference solver implementations for comparison:
    - Naive solver that tries all assignments
    - DPLL solver with unoptimized unit propagation and pure literal elimination
- Parsing of SAT formulas in DIMACS format

Authors: Jediah Katz, Josh Cohen

This program was created as a final project for [CIS 552](https://www.cis.upenn.edu/~cis552/current/index.html) in Fall 2019 at the University of Pennsylvania.
