
import sys
import collections
import locker
import os
import pickle
import links
import random
from logger import echo

class DBError(Exception):
    def __init__(self, *args, **key):
        super().__init__(*args, **key)

class Fraction:

    def __init__(self, numer, denom):
        self.numer = numer
        self.denom = denom

    def __add__(self, other):
        return Fraction(self.numer + other.numer, self.denom + other.denom)

    def __sub__(self, other):
        return Fraction(self.numer - other.numer, self.denom - other.denom)

    def __float__(self):
        if self.denom == 0:
            return 0.0
        else:
            return float(self.numer / self.denom)

    def __str__(self):
        return str(self.numer) + ' / ' + str(self.denom)

    def __repr__(self):
        return "<Fraction " + str(self) + ">"

def _zero_frac():
    # This is unfortunately necessary due to Pickle's strange behavior with regard to
    # default dicts. Using a lambda for this would result in a failed pickle.
    return Fraction(0, 0)

class ReinDatabase:

    def __init__(self, keyword):
        self.keyword = keyword
        self.db = collections.defaultdict(_zero_frac)

    def reset(self):
        self.db = {}

    def copy(self):
        new_db = ReinDatabase(self.keyword)
        new_db.db = self.db.copy()
        return new_db

    def add_info(self, page, frac):
        self.db[page] += frac

    def add_score(self, page, n): # n should be between 0 and 1 inclusive
        self.add_info(page, Fraction(n, 1))

    def get_score(self, page):
        return self.db[page]

    def __add__(self, other):
        if self.keyword != other.keyword:
            raise DBError("Keywords do not match in DB addition")
        zz = self.db.copy()
        for k, v in other.db.items():
            zz[k] += v
        new_db = ReinDatabase(self.keyword)
        new_db.db = zz
        return new_db

    def __sub__(self, other):
        if self.keyword != other.keyword:
            raise DBError("Keywords do not match in DB subtraction")
        zz = self.db.copy()
        for k, v in other.db.items():
            zz[k] -= v
        new_db = ReinDatabase(self.keyword)
        new_db.db = zz
        return new_db

class IncrDatabase:

    def __init__(self, keyword):
        self.keyword = keyword
        self.db = ReinDatabase(keyword)
        self.bak = self.db.copy()

    def load(self):
        filename = "./temp/rein_{}.txt".format(self.keyword)
        if os.path.isfile(filename):
            with locker.openl(filename) as fobj:
                self.db = pickle.load(fobj)
        else:
            self.db = ReinDatabase(self.keyword)
        self.bak = self.db.copy()

    def save(self):
        filename = "./temp/rein_{}.txt".format(self.keyword)
        exists = True
        if not os.path.isfile(filename): # Create the file if it doesn't exist
            with open(filename, 'wb'):
                pass
            exists = False
        with locker.openl(filename) as fobj:
            db1 = self.db
            if exists:
                db2 = pickle.load(fobj)
            else:
                db2 = self.bak.copy()
            changes1 = db1 - self.bak
            changes2 = db2 - self.bak
            db_final = self.bak + changes1 + changes2
            fobj.seek(0)
            fobj.truncate()
            pickle.dump(db_final, fobj)
            self.bak = db_final
            self.db = self.bak.copy()

    def add_info(self, page, frac):
        self.db.add_info(page, frac)

    def add_score(self, page, n): # n should be between 0 and 1 inclusive
        self.add_info(page, Fraction(n, 1))

    def get_score(self, page):
        return self.db.get_score(page)

class ReinLinkSelector(links.LinkSelector):

    def __init__(self, keyword, explore = None):
        if explore is None:
            explore = links.NoDupLinkSelector()
        self.db = IncrDatabase(keyword)
        self.db.load()
        self.pages = []
        self.explore = explore
        self.already_exploring = False

    def finished(self):
        self.explore.finished()
        self.db.save()

    def start_crawl(self):
        self.explore.start_crawl()
        self.pages = []
        self.already_exploring = False

    def end_crawl(self, success):
        self.explore.end_crawl(success)
        score = 1 if success else 0
        for page in self.pages:
            self.db.add_score(page, score)
        # Make the final result worth a half point. This encourages the reinforcement
        # engine to "run down the clock", so to speak, making it more likely to keep
        # scanning new pages and then explore at the end to get unusual pages.
        self.db.add_info(self.pages[-1], Fraction(-0.5, 0))

    def should_explore(self, state):
        if self.already_exploring:
            return True
        numer = state.depth_left()
        denom = state.total_depth()
        cap = 1 - numer / (denom + 1)
        #echo("Explore cap", cap, level = 2)
        res = random.random() < cap
        if res:
            self.already_exploring = True
        return res

    def select_link(self, state):
        page = state.page()
        if not self.pages: # We are on the first page so store this one too
            self.pages.append(page.title)
        pages = filter(lambda x: x not in self.pages, page.links)
        pages = filter(lambda x: self.db.get_score(x).denom > 0, pages)
        pages = map(lambda x: (x, float(self.db.get_score(x))), pages)
        pages = list(pages)
        if not pages or self.should_explore(state):
            echo("Exploring", level = 2)
            result = self.explore.select_link(state)
        else:
            echo("Using prior knowledge", level = 2)
            result = self._weighted_random(pages)
        if result:
            self.pages.append(result)
        return result

    def _weighted_random(self, pages):
        total = sum(map(lambda x: x[1], pages))
        value = random.random() * total
        for curr, n in pages:
            value -= n
            if value < 0:
                return curr
        return None
