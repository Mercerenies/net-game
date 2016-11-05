
import random
import math

def escape(str_):
    """Escapes a string which might contain unprintable characters in a format suitable for output to a console."""
    return repr(str_.encode('utf-8'))

def rnd(lst):
    """Selects a random element from the list, which must be nonempty."""
    return lst[math.floor(random.random() * len(lst))]

def clamp(x, a, b):
    """
    Clamps the value x between a and b. The values supplied should be totally ordered using less-than
    and greater-than operators. The value x will be returned if it is between a and b. If it is not,
    then the appropriate bounding value is returned.
    """
    if x < a:
        return a
    elif x > b:
        return b
    else:
        return x
