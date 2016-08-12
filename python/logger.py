
from sys import stderr

class Logger:
    instance = None
    def __init__(self, debug_level = 0):
        self.debug_level = debug_level
    def echo(self, *args, level = 1, flush = False):
        if self.debug_level >= level:
            print(*args, file = stderr)
            if flush:
                stderr.flush()
    @staticmethod
    def get():
        if Logger.instance is None:
            Logger.instance = Logger()
        return Logger.instance

def set_global_debug_level(lvl):
    Logger.get().debug_level = lvl

def echo(*args, level = 1, flush = False):
    Logger.get().echo(*args, level = level, flush = flush)
