# hm-inference

The implementation is based on the paper [Algorithm W Step by Step][algo-w],
with the addition of n-tuples.

```txt
exp ::= x                           -- variable
      | exp exp                     -- application
      | exp `x` exp                 -- infix application
      | fun x -> exp                -- lambda
      | let x = exp in exp          -- let binding
      | if exp then exp else exp    -- if expression
      | i                           -- int literal
      | b                           -- bool literal
      | exp, exp                    -- tuple

ty  ::= a0...an
      | int
      | bool
      | ty -> ty
      | ty * ty
```

[algo-w]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf
