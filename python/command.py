
from util import dict_to_list
from tokenize import *
from basis import Basis
from algorithm import Spider

# TODO Support reinforcement learning here

def _resolve_basetype(b, t):
    # At most one of base: or type: is allowed to be a wildcard
    if is_wildcard(b) and is_wildcard(t):
        raise TokenizeError("Tokenizer Error: Type and base cannot both be wildcards")
    # If either one is a wildcard, resolve it now
    if is_wildcard(b):
        b = t
    if is_wildcard(t):
        t = b
    # Resolve the base page first
    if is_simple_symbol(b):
        b = Basis.basis[b]
    elif isinstance(b, str):
        _tmp1 = b
        b = lambda: _tmp1
    else:
        raise TokenizeError("Tokenizer Error: Base page expected to be string or symbol")
    # Then resolve the type
    if is_simple_symbol(b):
        t = Basis.query[t]
    else:
        raise TokenizeError("Tokenizer Error: Query type expected to be a symbol")
    return b, t

def _crawl_cmd(parts, **kwargs):
    # Required and allowed keywords
    allowed = {'depth:', 'tries:', 'count:', 'type:', 'base:'}
    required = {'type:', 'base:'}
    if not required <= kwargs.keys() <= allowed:
        raise TokenizeError("Tokenizer Error: Invalid argument list {}".format(list(kwargs.keys())))
    # Load the keywords, with appropriate defaults
    depth = kwargs.get('depth:', 5)
    tries = kwargs.get('tries:', 3)
    count = kwargs.get('count:', 1)
    type_ = kwargs['type:']
    base  = kwargs['base:']
    # Check types where necessary
    assert_type(depth, int)
    assert_type(tries, int)
    assert_type(count, int)
    # Resolve bases and types
    base, type1 = _resolve_basetype(base, type_)
    # Construct the spider
    spider = Spider(depth = depth, max_tries = tries)
    # Crawl
    results = [spider.crawl_times(base(), type1) for i in range(0, count)]
    parts[type_] = parts.get(type_, [])
    parts += list(filter(lambda x: x is not None, results))
    # TODO Should we have a return value here? Maybe just report success?

_builtin = {
    'CRAWL': _crawl_cmd
}

class Command:

    def __init__(self, head, **args):
        self.head = head
        self.args = args

    def __str__(self):
        expr = dict_to_list(self.args)
        expr[:0] = [self.head]
        return ' '.join(map(str, expr))

    def execute(self, parts):
        cmd = _builtin.get(self.head, None)
        if not cmd:
            raise TokenizeError("Tokenizer Error: Unknown command " + str(self.head))
        return cmd(parts, **self.args)

def parse(symbols):
    symbols_ = iter(symbols)
    commands = []
    cmd = None
    try:
        while True:
            head = next(symbols_)
            token_assert(head, Symbol)
            cmd = Command(str(head))
            for kv in group_into(takewhile(lambda x: x is not Separator(), symbols_), 2):
                if len(kv) < 2:
                    raise TokenizeError("Tokenizer Error: Keyword lists should have even length")
                k, v = kv
                token_assert(k, Symbol)
                cmd.args[str(k)] = v
            commands.append(cmd)
    except StopIteration:
        pass
    if cmd:
        commands.append(cmd)
    return commands

def read(string):
    return parse(scan(tokenize(string)))