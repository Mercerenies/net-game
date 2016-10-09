
import fcntl

class Locker:
    """
    The Locker class is a wrapper for the Python fcntl bindings which allows fcntl
    locks to be used with Python's `with'-statement syntax. The common entrypoint
    into this class is through the openl() function.
    """

    def __init__(self, filename, mode = "r+b", exclusive = True, blocking = True):
        self.file = None
        self.filename = filename
        self.filemode = mode
        self.flags = 0
        if exclusive:
            self.flags |= fcntl.LOCK_EX
        else:
            self.flags |= fcntl.LOCK_SH
        if not blocking:
            self.flags |= fcntl.LOCK_NB

    def __enter__(self):
        self.file = open(self.filename, self.filemode)
        fcntl.flock(self.file, self.flags)
        return self.file

    def __exit__(self, type, value, traceback):
        fcntl.flock(self.file, fcntl.LOCK_UN)
        self.file.close()

def openl(filename, **key):
    """
    Constructs a simple Locker, passing key arguments onto the Locker class. This function does NOT
    perform a lock, nor does it open a file. It is intended to be used as the subject of a `with'-
    statement.
    """
    return Locker(filename, **key)
