
from util import dict_to_list
from tokenize import *
from basis import Basis
from algorithm import Spider

# TODO Support reinforcement learning here

def check_arglist(args, *, allowed, required):
    keys = args.keys()
    if not required <= keys and not keys <= allowed:
        raise TokenizeError(
            "Tokenizer Error: Expected additional argument(s) {} and not {}".format(
                required - keys,
                keys - allowed
            )
        )
    if not required <= keys:
        raise TokenizeError(
            "Tokenizer Error: Expected additional argument(s) {}".format(
                required - keys
            )
        )
    if not keys <= allowed:
        raise TokenizeError(
            "Tokenizer Error: Unexpected argument(s) {}".format(
                keys - allowed
            )
        )

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
        b = Basis.basis[b.lower()]
    elif isinstance(b, str):
        _tmp1 = b
        b = lambda: _tmp1
    else:
        raise TokenizeError("Tokenizer Error: Base page expected to be string or symbol")
    # Then resolve the type
    if is_simple_symbol(t):
        t = Basis.query[t.lower()]
    else:
        raise TokenizeError("Tokenizer Error: Query type expected to be a symbol")
    return b, t

def _crawl_cmd(parts, **kwargs):
    # Required and allowed keywords
    allowed = {'DEPTH:', 'TRIES:', 'COUNT:', 'TYPE:', 'BASE:'}
    required = {'TYPE:', 'BASE:'}
    check_arglist(kwargs, allowed = allowed, required = required)
    # Load the keywords, with appropriate defaults
    depth = kwargs.get('DEPTH:', 5)
    tries = kwargs.get('TRIES:', 3)
    count = kwargs.get('COUNT:', 1)
    type_ = kwargs['TYPE:']
    base  = kwargs['BASE:']
    # Check types where necessary
    token_assert(depth, int)
    token_assert(tries, int)
    token_assert(count, int)
    try:
        # Resolve bases and types
        base, type1 = _resolve_basetype(base, type_)
        type2 = Basis.plural[type_.lower()]
    except KeyError as e:
        raise TokenizeError("Tokenizer Error: Unknown keyword " + str(e))
    # Construct the spider
    spider = Spider(depth = depth, max_tries = tries)
    # Crawl
    results = [spider.crawl_times(base(), type1) for i in range(0, count)]
    parts[type2] = parts.get(type2, [])
    parts[type2] += list(filter(lambda x: x is not None, results))
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
            cmd = None
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
