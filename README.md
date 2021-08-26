# Algorithm W implementation

The implementation is based on the paper [Algorithm W Step by Step][algo-w],
with the addition of n-tuples, custom operators and if expressions.

## Grammar

```txt
exp ::= x                           -- variable
      | exp exp                     -- application
      | exp `x` exp                 -- infix application
      | exp op exp                  -- infix operation
      | fun x0...xn -> exp          -- anonymous function
      | let x = exp in exp          -- let binding
      | if exp then exp else exp    -- if expression
      | exp, exp                    -- tuple
      | i                           -- int literal
      | b                           -- bool literal
      | exp :: ty                   -- type annotation

ty  ::= a0...an
      | int
      | bool
      | unit
      | ty -> ty
      | ty * ty
```

## References

* [Algorithm W Step by Step][algo-w]: Original paper
* [Write you an inference in F#][fsharp]: Various type inference algorithm implemented in F#

[algo-w]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf
[fsharp]: https://github.com/7sharp9/write-you-an-inference-in-fsharp
