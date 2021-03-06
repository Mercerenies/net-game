
import random
import math
import itertools

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

def dict_to_list(dct):
    """
    Flattens a list into a dictionary in the same way that Ruby or Perl would. The result is what
    Lisp would call a plist. The first element is a key, and the second is the corresponding value,
    then the third is another key, the fourth the next value, and so on.
    """
    return [item for pair in dct.items() for item in pair]

def group_into(iterable, n):
    """
    Groups elements of the iterable into groups of n elements each. The new iterator returned
    is semi-lazy, in that it will not compute the whole result in advance but does force each
    group when requested.
    """
    it = iter(iterable)
    while True:
        curr = tuple(itertools.islice(it, n))
        if not curr:
            break
        yield curr
