import random
import sys

max_atoms = 0
num_rules = 0
atom_set = []

def generate_atom_set(max_atoms):
    global atom_set
    atom_set = []
    alphabet = 'abcdefghijklmnopqrstuvwxyz'
    idx = 1
    while len(atom_set) < max_atoms:
        for letter in alphabet:
            atom_set.append('{}{}'.format(letter,idx))
            if len(atom_set) >= max_atoms:
                break
            atom_set.append('-{}{}'.format(letter,idx))
        idx += 1

def generate_literal(pos, atom_set):
    atom = random.choice(atom_set)
    atom_set.remove(atom)
    return atom_set, atom if pos else 'not ' + atom  # make it a negative literal if specified

def generate_rule(min_n, max_n, max_atoms):
    rule = ':('
    global atom_set
    rule_atoms = list(atom_set)
    n = random.randint(min_n, max_n)
    
    head_atoms = set()
    while len(head_atoms) < n:
        rule_atoms, atom = generate_literal(True, rule_atoms)
        head_atoms.add(atom)
    
    tot_body_atoms = random.randint(0, max_atoms - n)

    tot_pos = random.randint(0, tot_body_atoms)
    pos_body_atoms = set()
    while len(pos_body_atoms) < tot_pos:
        rule_atoms, atom = generate_literal(True, rule_atoms)
        if atom not in head_atoms:  # ensure not in head
            pos_body_atoms.add(atom)
    
    tot_neg = tot_body_atoms - tot_pos
    neg_body_atoms = set()
    while len(neg_body_atoms) < tot_neg:
        rule_atoms, atom = generate_literal(False, rule_atoms)
        if atom not in head_atoms and atom not in pos_body_atoms:  # ensure not in head or positve part of body
            neg_body_atoms.add(atom)
    
    head = ' * '.join(head_atoms)
    if tot_body_atoms > 0:
        pos_body = ', '.join(pos_body_atoms)
        neg_body = ', '.join(neg_body_atoms)
        if pos_body and neg_body:
            rule = '{} :- {}, {}.'.format(head,pos_body,neg_body)
        elif pos_body:
            rule = '{} :- {}.'.format(head,pos_body)
        elif neg_body:
            rule = '{} :- {}.'.format(head,neg_body)
    else:
        rule = '{}.'.format(head)
    
    return rule

def main():
    if len(sys.argv) != 4:
        print("Usage: python generator.py <number of rules> <max atoms> <max n>")
        sys.exit(1)

    global max_atoms
    global atom_set
    global num_rules

    num_rules = int(sys.argv[1])
    max_atoms = int(sys.argv[2])
    max_n = int(sys.argv[3])

    generate_atom_set(max_atoms)

    m = random.randint(1, int(0.75*num_rules) + 1)
    for _ in range(m):  # generate m o.d. rules
        rule = generate_rule(2, max_n, max_atoms)
        print(rule)

    for _ in range(num_rules - m):  # generate regular rules
        rule = generate_rule(1, 1, max_atoms)
        print(rule)

if __name__ == '__main__':
    main()
