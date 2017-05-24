
from util import dict_to_list
from tokenize import *
from basis import Basis
from algorithm import Spider
import links
import reinforcement

# TODO Support reinforcement learning here

def check_arglist(args, *, allowed, required):
    """
    Verifies that the arglist, which should be a dict, contains keys corresponding to the
    specification. The two keyword arguments should be set-like objects with required <= allowed.
    An appropriate error of type TokenizeError will be raised if required <= keys <= allowed is
    not true, where keys is the set of keys in the argument list provided. All of the keys in the
    argument list should be hashable objects, as should all of the elements in both provided sets.
    If the specifications are satisfied and no error is raised, the function will return silently.
    """
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
    import sys
    if is_wildcard(b):
        b = t
    if is_wildcard(t):
        t = b
    # Resolve the base page first
    if is_simple_symbol(b):
        b = Basis.basis[b.lower()]
    elif isinstance(b, str): # TODO Write a is_string which distinguishes between str and Symbol
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
    allowed = {'DEPTH:', 'TRIES:', 'COUNT:', 'TYPE:', 'BASE:', 'REIN:'}
    required = {'TYPE:', 'BASE:'}
    check_arglist(kwargs, allowed = allowed, required = required)
    # Load the keywords, with appropriate defaults
    depth = kwargs.get('DEPTH:', 5)
    tries = kwargs.get('TRIES:', 3)
    count = kwargs.get('COUNT:', 1)
    rein  = kwargs.get('REIN:', False)
    type_ = kwargs['TYPE:']
    base  = kwargs['BASE:']
    # Check types where necessary
    token_assert(depth, int)
    token_assert(tries, int)
    token_assert(count, int)
    token_assert(rein, bool)
    try:
        # Resolve bases and types
        base1, type1 = _resolve_basetype(base, type_)
        # TODO This next line is very disorganized and hard to follow
        type2 = Basis.plural[base.lower() if is_wildcard(type_) else type_.lower()]
    except KeyError as e:
        raise TokenizeError("Tokenizer Error: Unknown keyword " + str(e))
    # Construct the spider
    if rein:
        selector = reinforcement.ReinLinkSelector(type2)
    else:
        selector = links.NoDupLinkSelector()
    spider = Spider(depth = depth, max_tries = tries, selector = selector)
    # Crawl
    results = [spider.crawl_times(base1(), type1) for i in range(0, count)]
    spider.finished()
    parts[type2] = parts.get(type2, [])
    parts[type2] += list(filter(lambda x: x is not None, results))
    # TODO Should we have a return value here? Maybe just report success?

_builtin = {
    'CRAWL': _crawl_cmd
}

class Command:
    """
    A command consists of a head, or the name of the function, together with a single dictionary
    of named arguments. The head, as well as all keys in the dictionary, should be simple strings.
    """

    def __init__(self, head, **args):
        """Creates a command with the given head and argument list."""
        self.head = head
        self.args = args

    def __str__(self):
        """
        Converts the command to a canonical form close to that which was parsed in originally.
        """
        # TODO Make strings map to bracketed expressions so that the str/parse relationship holds.
        expr = dict_to_list(self.args)
        expr[:0] = [self.head]
        return ' '.join(map(str, expr))

    def execute(self, parts):
        """
        Executes the command, where the head is a built-in command name and the argument list
        is appropriate for the command. If the head does not exist as a command or the argument
        list is incorrect, a TokenizeError will be raised. If a different kind of error, brought
        on by a wrongly formatted command, occurs then a TokenizeError should also be raised.
        """
        cmd = _builtin.get(self.head, None)
        if not cmd:
            raise TokenizeError("Tokenizer Error: Unknown command " + str(self.head))
        return cmd(parts, **self.args)

def parse(symbols):
    """
    Takes an iterable of tokens, such as the one produced by scan(), and produces a list of
    commands. The iterable is separated into lists separated by Separator() tokens. Each list
    will be read as the name of the command (a Symbol instance) followed by one or more
    key-value pairs. Each key should be a symbol and each value should be a symbol, string, or
    number. Violation of any of these conditions results in a TokenizeError.
    """
    symbols_ = iter(symbols)
    commands = []
    cmd = None
    try:
        while True:
            cmd = None
            head = next(symbols_)
            if head is Separator():
                continue
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
    """
    read() is a convenience function which performs tokenize(), then scan(), then parse(), in
    sequence.
    """
    return parse(scan(tokenize(string)))
