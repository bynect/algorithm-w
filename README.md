# hm-inference

The type inference is based on the paper [Algorithm W Step by Step](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf).

```txt
exp ::= x                           -- variable
      | exp exp                     -- application
      | fun x -> exp                -- lambda
      | let x = exp in exp          -- let binding
      | if exp then exp else exp    -- if expression
      | i                           -- int literal
      | b                           -- bool literal

ty  ::= a0...an
      | int
      | bool
      | ty -> ty
```
