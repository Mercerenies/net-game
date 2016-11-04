
import sys

class Keywords:
    _inst = None

    def __init__(self):
        self.key = None

    def _load_file(self):
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
        Attempt to match the category c against the match words associated with the keyword key
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
        return Keywords._inst

    @staticmethod
    def check_match(key, c):
        """A convenience method which calls match on the default Keywords instance."""
        return Keywords.instance().match(key, c)

Keywords._inst = Keywords()
