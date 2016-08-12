
import fcntl

class Locker:

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
    return Locker(filename, **key)
