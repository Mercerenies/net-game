
from sys import stderr

class Logger:
    """A logger class to control output to STDERR."""
    instance = None
    def __init__(self, debug_level = 0):
        self.debug_level = debug_level
    def echo(self, *args, level = 1, flush = False, **key):
        """
        Prints out the arguments, as though passed to `print', if and only if
        the debug level is greater than or equal to the level passed in. If the
        flush argument is true, the output is flushed immediately.
        """
        if self.debug_level >= level:
            print(*args, file = stderr, **key)
            if flush:
                stderr.flush()
    @staticmethod
    def get():
        if Logger.instance is None:
            Logger.instance = Logger()
        return Logger.instance

def set_global_debug_level(lvl):
    """Sets the debug level of the default logger provided in Logger."""
    Logger.get().debug_level = lvl

def echo(*args, level = 1, flush = False, **key):
    """Echoes to the default logger."""
    Logger.get().echo(*args, level = level, flush = flush, **key)
