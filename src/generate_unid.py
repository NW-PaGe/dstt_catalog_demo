import random

# generate_unid
# description:
#   Produces a new id as a 10-digit integer unique to the input
# params:
#   existing_unid: list of existing unid ints
# returns:
#   unid: int, 10-digit int unique to input list
def generate_unid(existing_unid):
    random.seed() # set seed
    unid = random.randrange(10e8, 10e9) # set random int
    while unid in existing_unid: # check that random int is unique to input
        unid = random.randrange(10e8, 10e9) # if not, set new random int
    return unid
