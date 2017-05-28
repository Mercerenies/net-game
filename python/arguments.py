
from getopt import getopt, GetoptError
from basis import Basis
from logger import echo

class Arguments:
    """
    The Arguments class abstracts the details of using getopt() on the command line arguments, by
    supplying common argument list formats and the means to access them.
    """

    def __init__(self, argv, arg_set):
        """
        Parses the argument list using getopt. The specific allowable arguments should be
        passed as the second argument, as an instance of ArgSet.
        """
        try:
            arglist = arg_set.string
            self._args = dict(getopt(argv, arglist)[0])
        except GetoptError as e:
            self._args = {}
            echo("Error in arguments:", e)

    def get(self, key, default):
        """
        Retrieves the specified argument from the list, where the key should begin with a dash (-).
        If the key does not exist, the given default is returned.
        """
        return self._args.get(key, default)

    def get_int(self, key):
        """
        Retrieves the specified argument as an integer, defaulting to 0 if the key does not exist
        or the value is not an integer.
        """
        try:
            return int(self.get(key, "0"))
        except ValueError:
            return 0 # TODO Handle this and print an error (or a warning)?

    def get_string(self, key):
        """
        Retrieves the specified argument, defaulting to the empty string if the key does not
        exist.
        """
        return self.get(key, "")

    def __contains__(self, key):
        """
        Returns whether or not the key (which should begin with a dash) was supplied in the argument
        list.
        """
        return key in self._args

class LegacyArguments(Arguments):
    """
    The LegacyArguments class and corresponding argument list is used when a legacy-crawl is
    requested and parses the argument to that command.
    """

    def __init__(self, argv):
        super().__init__(argv, ArgSet.LEGACY)

    def celebs(self):
        return self.get_int("-c")

    def people(self):
        return self.get_int("-p")

    def places(self):
        return self.get_int("-P")

    def weapons(self):
        return self.get_int("-w")

    def monsters(self):
        return self.get_int("-m")

    def animals(self):
        return self.get_int("-a")

    def foods(self):
        return self.get_int("-f")

    def rein(self):
        return "-r" in self

    def standard_sequence(self):
        # TODO This needs to be neatened (shorter columns) and documented
        return [
            ArgEntry(key = 'celebs'  , basis = Basis.celebrity, count = self.celebs()   , selector = 'people'),
            ArgEntry(key = 'people'  , basis = Basis.person   , count = self.people()  ),
            ArgEntry(key = 'places'  , basis = Basis.place    , count = self.places()  ),
            ArgEntry(key = 'weapons' , basis = Basis.weapon   , count = self.weapons() ),
            ArgEntry(key = 'monsters', basis = Basis.monster  , count = self.monsters()),
            ArgEntry(key = 'animals' , basis = Basis.animal   , count = self.animals()  , rein = False),
            ArgEntry(key = 'foods'   , basis = Basis.food     , count = self.foods()    , rein = False),
        ]

class TopLevelArguments(Arguments):
    """
    When the Python portion of the program starts up, the user's command line arguments are
    read and sent to TopLevelArguments to be parsed.
    """

    def __init__(self, argv):
        super().__init__(argv, ArgSet.TOP_LEVEL)

    def debug(self):
        return self.get_int("-d")

    def expr(self):
        return self.get_string("-e")

class ArgEntry:

    def __init__(self, *, key, basis, count, selector = None, rein = True):
        self.key = key
        self.basis = basis
        self.count = count
        self.selector = selector or self.key
        self.rein = rein

class ArgSet:

    __slots__ = ('string',)

    def __init__(self, string):
        self.string = string

ArgSet.TOP_LEVEL = ArgSet("d:e:")
ArgSet.LEGACY = ArgSet("c:p:P:w:m:a:f:r")
