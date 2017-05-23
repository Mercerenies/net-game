
from util import dict_to_list
from tokenize import TokenizeError

class Command:

    def __init__(self, head, **args):
        self.head = head
        self.args = args

    def __str__(self):
        expr = dict_to_list(self.args)
        expr[:0] = [self.head]
        return ' '.join(map(str, expr))

