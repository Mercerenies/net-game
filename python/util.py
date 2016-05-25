
import random
import math

def escape(str_):
    return repr(str_.encode('utf-8'))

def rnd(lst):
    return lst[math.floor(random.random() * len(lst))]

def clamp(x, a, b):
    if x < a:
        return a
    elif x > b:
        return b
    else:
        return x
