
import wikipedia

class WikiErrorWrapper:

    def __init__(self, err):
        self.err = err

    def type(self):
        return type(self.err)

    def title(self):
        return "???"

    def options(self):
        return []

    def __str__(self):
        return self.type().__name__ + "(" + repr(self.title()) + ")"

class RedLinkErrorWrapper(WikiErrorWrapper):

    def title(self):
        return self.err.pageid

class AmbiguousErrorWrapper(WikiErrorWrapper):

    def title(self):
        return self.err.title

    def options(self):
        return self.err.options

class DefaultErrorWrapper(WikiErrorWrapper):

    pass

def wrap(err):
    wrapper = DefaultErrorWrapper
    if isinstance(err, wikipedia.PageError):
        wrapper = RedLinkErrorWrapper
    elif isinstance(err, wikipedia.DisambiguationError):
        wrapper = AmbiguousErrorWrapper
    return wrapper(err)
