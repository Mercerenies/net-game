
import sys

class Keywords:
    """
    Keywords is a class which provides a singleton instance and maintains a list of keywords, used to
    check whether a page is of a specific type by comparing against its categories.
    """
    _inst = None

    def __init__(self):
        """Constructs an empty Keywords object."""
        self.key = None

    def _load_file(self):
        """Internal implementation which loads the keyword list on demand."""
        self.key = dict()
        state = ""
        with open("./data/keywords.txt") as f:
            data = f.read()
        for curr in data.split("\n"):
            if curr == "":
                continue
            if curr[:3] == ":: ":
                state = curr[3:]
            else:
                self.key[state] = self.key.get(state, []) + [curr]

    def match(self, key, c):
        """
        Attempts to match the category c against the match words associated with the keyword key
        provided. Returns whether any matches were found.
        """
        if not self.key:
            self._load_file()
        for x in self.key[key]:
            if x in c.lower(): # TODO Whole words only? Consider the consequences if we switched to that
                return True
        return False

    @staticmethod
    def instance():
        """Accesses the singleton instance."""
        return Keywords._inst

    @staticmethod
    def check_match(key, c):
        """A convenience method which calls match on the default Keywords instance."""
        return Keywords.instance().match(key, c)

Keywords._inst = Keywords()
