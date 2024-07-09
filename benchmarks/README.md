## Overview
A script for generating random LPODs based on specified parameters and a script for running benchmarks.


## LPOD generator
### Prerequisite
The generator script has been tested with `Python 3.5.3`

### Usage
```
python generator.py num_rules max_atoms max_n
```
where:
- **num_rules** is the number of total rules (ordered disjunctive + regular)
- **max_atoms** is the maximum number of (distinct) atoms used in the program
- **max_n** is an upper bound for the maximum ordered disjunction

### Example
The execution of
```
python generator.py 5 6 5
```
could produce the following LPOD:
```
a1 * -b1 * c1 * d1 * b1 :- -a1.
-a1 * c1.
c1 :- -a1.
a1 :- not d1, not b1, not -b1.
-a1 :- not b1, not c1, not -b1, not a1.
```

## Running the tests
Make sure that scripts `runtests.sh` and `runtest.sh` have execute permissions, then:
```
./runtests.sh
```
for running all tests or:
```
./runtest.sh test{n} n
```
for running a specific test, for example:
```
./runtest.sh test14 14
```
