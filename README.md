## Logical Inference

Implementation of logical inference algorithms. I'm writing these mostly for autodidactism. Algorithms closely follow discussion in Russell and Norvig.

Algorithms currently implemented:

* resolution for propositional logic
* unification
* logic programming examples
    * river crossing puzzle -- this uses a Writer-State-List monad stack
          * writer is used to record path/move information
          * state is used to keep track of puzzle state (obviously!)
          * list is used to take all possible paths
