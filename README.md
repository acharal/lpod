# LPOD solver

A solver for Logic Programs with Ordered Disjunction (LPODs) written in SWI-Prolog. It translates LPODs into Answer Set Programming (ASP) and utilizes `asprin` and `clingo` to find preferred solutions.

## Installation

To use LPOD solver you need to have:

1. SWI-Prolog 
2. [`asprin`](https://github.com/potassco/asprin) and `clingo`

We have tested the solver with `Python 3.5.3`, `SWI-Prolog 9.1.21`, `asprin 3.1.1` and `clingo 5.4.0`

## Usage
Make sure `lpodsolver.sh` has execute permissions, then:
```
./lpodsolver.sh example.lpod
```


## LPOD Syntax

The syntax for writing LPODs is as follows:

- Rules with Ordered Disjunction: `a * b :- body.` expresses that `a` is preferred over `b` when the `body` is true.
- Rules with Regular Disjunction: `a ; b :- body.` expresses that either `a` or `b` should be true if `body` is true.
- Rules with both Ordered and Regular Disjunction: 
  `a ; b * c * d ; e :- body` expresses that given `body` is true, either 
  `a` or `b` is preferred over `c` but there is no preference between them, and `c` is preferred over `d` or `e`.
- Regular Rules: Standard logic program rules can also be included.
- Constraints: `:- body` expresses the fact that `body` should not be true in any solution.


Example `example.lpod` file:

```asp
p * q :- r.
q :- s.
r.
s.
```

In this example, `p` is preferred over `q` when `r` is true.

## Related projects

1. [psmodels](http://www.tcs.hut.fi/Software/smodels/priority/)
2. [lpod.pl](https://www.dc.fi.udc.es/~cabalar/lpod/)
3. [lpod2asprin](https://github.com/zhunyoung/lpod2asprin)
