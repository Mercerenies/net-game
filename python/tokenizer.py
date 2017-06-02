
from singleton import Singleton
from util import group_into
from itertools import takewhile

class Symbol:
    """
    A crude symbol class designed simply to distinguish between symbols and straight strings.
    Following the Common Lisp precedent, symbols are always converted to uppercase when
    constructed.
    """

    def __init__(self, string):
        self.string = string.upper()

    def __str__(self):
        return self.string

    def __repr__(self):
        return "Symbol(" + repr(self.string) + ")"

    def __eq__(self, other):
        return self.string == other.string

    def __ne__(self, other):
        return not (self == other)

class TokenizeError(Exception):
    """
    A class for any errors in the tokenization and command execution process. Any errors
    occurring from invalid or syntactically incorrect commands should be handled using this
    exception class or a child thereof. Any errors or mistakes in the code itself should
    still use standard Python exceptions and should not use this class.
    """

    def __init__(self, *args, **key):
        super().__init__(*args, **key)

class Separator(metaclass = Singleton):
    """
    A singleton for a separator in the tokenized string. Separators are either semicolons or
    newlines. This class does not distinguish between semicolons and newlines; they are
    treated as synonymous constructs.
    """

    def __str__(self):
        return "Separator()"

    def __repr__(self):
        return "Separator()"

def token_assert(obj, type_):
    """
    Verifies that the given object is an instance of the specified type. If it is, this function
    does not do anything further. If it does not, a TokenizeError is raised.
    """
    if not isinstance(obj, type_):
        raise TokenizeError("Tokenizer Error: Expected {}, got {}".format(type_, type(obj)))

def is_wildcard(obj):
    """
    Returns whether or not the object is a wildcard. That is, whether or not the object is an
    instance of Symbol whose string value is exactly equal to the '*' character.
    """
    return isinstance(obj, Symbol) and obj == Symbol('*')

def is_symbol(obj):
    """
    Returns whether or not the object is an instance of the Symbol class. is_symbol(obj) is
    equivalent to isinstance(obj, Symbol).
    """
    return isinstance(obj, Symbol)

def is_simple_symbol(obj):
    """Returns true if and only if the object is a symbol which is not a wildcard symbol."""
    return is_symbol(obj) and not is_wildcard(obj)

def tokenize(string):
    """
    Converts a string into a list of substrings, broken by the token separation rules. This function
    does not examine the individual substrings, nor does it make Symbol instances; it merely returns
    a list of appropriately separated substrings. The string is separated at any point containing at
    least one space character, except inside of strings delimited by square brackets []. In square
    brackets, no tokenization is performed, but escaping of backslash sequences is performed instead.
    An opening bracket always begins a new token, and a newline or semicolon always acts as an
    independent token, except in strings.
    """
    # Tokenizer states:
    # 0 - Standard state; parsing token
    # 1 - String state; parsing string
    # 2 - Backslash state; parsing backslash character
    tokens = []
    token = ""
    state = 1
    def push():
        nonlocal token
        if token != "":
            tokens.append(token)
            token = ""
        return True
    def append(c):
        nonlocal token
        token += c
        return True
    def goto(n):
        nonlocal state
        state = n
        return True
    handlers = {
        (1, ' ' ): lambda c: push(),
        (1, '[' ): lambda c: push() and append('[') and goto(2),
        (1, ';' ): lambda c: push() and append(';') and push(),
        (1, '\n'): lambda c: push() and append(';') and push(),
        (1, ''  ): lambda c: append(c),
        (2, ']' ): lambda c: append(']') and push() and goto(1),
        (2, '\\'): lambda c: goto(3),
        (2, ''  ): lambda c: append(c),
        (3, ''  ): lambda c: append(c) and goto(2),
    }
    for char in string:
        handler = handlers.get((state, char), None) or handlers[(state, '')]
        handler(char)
    push()
    if state != 1:
        if state == 2 or state == 3:
            raise TokenizeError("Tokenizer Error: Unclosed string literal")
        else:
            # If this case occurs, we forgot to account for a new state here. If that
            # happens, fix it!
            raise TokenizeError("Tokenizer Error: Parse ended in state {}, not 1".format(state))
    return tokens

def scan(tokens):
    """
    Given an iterable of strings, such as those produced by tokenize(), convert each
    element to its appropriate command form. Elements equal to a semicolon are scanned
    into the Separator() instance. Elements surrounded by [] will be left as strings
    without the outside [] characters. Elements which consist only of decimal digits
    are parsed into integers, and all other elements are converted to Symbol instances..
    """
    for token in tokens:
        if token == ';':
            # Separators are parsed as simple semicolons
            yield Separator()
        elif token[0] == '[' and token[-1] == ']':
            # Strings are enclosed in brackets []
            yield token[1:-1]
        elif token.isdigit(): # TODO This allows some Unicode characters we don't want allowed here
            # Integers consist of digits; currently there is no support for negative integers
            yield int(token)
        elif token.upper() == 'YES':
            # The Boolean true value is the text "YES" (case insensitive)
            yield True
        elif token.upper() == 'NO':
            # The Boolean false value is the text "NO" (case insensitive)
            yield False
        else:
            # Symbols are any other sequence of non-space characters
            yield Symbol(token)
