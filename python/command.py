
class Symbol(str):

    def __new__(cls, contents):
        return super().__new__(cls, contents.upper())

_separator = object()

def separator():
    return _separator

def tokenize(string):
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
    return tokens

def scan(tokens):
    for token in tokens:
        if token == ';':
            # Separators are parsed as simple semicolons
            yield separator()
        elif token[0] == '[' and token[-1] == ']':
            # Strings are enclosed in brackets []
            yield token[1:-1]
        elif token.isdigit(): # TODO This allows some Unicode characters we don't want allowed here
            # Integers consist of digits; currently there is no support for negative integers
            yield int(token)
        else:
            # Symbols are any other sequence of non-space characters
            yield Symbol(token)

# ///// Next step: parse as a command to perform an action
