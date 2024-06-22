import random
import sys

def generate_atoms(num_atoms):
    atoms = []
    for _ in range(num_atoms):
        letter = random.choice('abcdefghijklmnopqrstuvwxyz')
        # number = random.randint(1, 9)
        # atom = letter + str(number)
        # atoms.append(atom)
        atoms.append(letter)
    return atoms

def concat_sep(items, separator):
    return separator.join(items)

def generate_random_lpods(num_rules, atoms, maxn):
    lpods = []
    num_atoms = len(atoms)
    atoms_per_part = num_atoms // 3
    
    for _ in range(num_rules):
        random.shuffle(atoms)
        headatoms = atoms[:maxn]
        bodyatoms = atoms
        notbodyatoms = atoms
        
        head_atoms = random.sample(headatoms, random.randint(1, max(2, maxn)))
        body_atoms = random.sample(bodyatoms, random.randint(0, atoms_per_part))
        not_body_atoms = random.sample(notbodyatoms, random.randint(0, atoms_per_part))


        head = concat_sep(head_atoms, ' * ')
        body = concat_sep(body_atoms, ', ')
        not_body = concat_sep(not_body_atoms, ', not ')
        
        rule = ''

        if body:
            rule += head + ' :- ' + body
        if not_body:
            rule += ', not ' + not_body
        if not (body and not_body):
            rule = head
        
        rule += '.'
        
        lpods.append(rule)
    
    return lpods

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: python script_name.py num_rules num_atoms maxn")
        sys.exit(1)

    try:
        num_rules = int(sys.argv[1])
        num_atoms = int(sys.argv[2])
        maxn = int(sys.argv[3])
    except ValueError:
        print("Error: Both num_rules and num_atoms must be integers")
        sys.exit(1)

    atoms = generate_atoms(num_atoms)
    random_lpods = generate_random_lpods(num_rules, atoms,maxn)
    for lpod in random_lpods:
        print(lpod)
