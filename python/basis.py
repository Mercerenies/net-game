
import string
import random
from util import rnd
from keywords import Keywords

def get_person_base():
    """Returns a starting page for person crawling."""
    return "Lists of people"

def get_celebrity_base():
    """Returns a starting page for celebrity crawling."""
    return "Lists of celebrities"

def get_place_base():
    """Selects and returns a starting page for location crawling."""
    return rnd(["Lists of places", "List of buildings and structures"])

def get_weapon_base():
    """Selects and returns a starting page for weapon crawling."""
    return rnd(["List of premodern combat weapons", "List of medieval weapons"])

def get_monster_base():
    """Selects and returns a starting page for monster crawling."""
    return "List of legendary creatures by type"

def get_animal_base():
    """Returns a starting page for animal crawling."""
    return "List of animals by common name"

def get_food_base():
    """Selects and returns a starting page for food crawling."""
    return rnd(["List of culinary fruits", "List of vegetables"])

def is_person_page(page):
    """Determines whether or not the given page object is a person page."""
    if "list" in page.title.lower():
        return False
    return len([c for c in page.categories
                if Keywords.check_match("people", c)
                and "list" not in c.lower()
                and "errors" not in c.lower()
                and "wikipedia" not in c.lower()]) > 0

def is_place_page(page):
    """Determines whether or not the given page object is a location page."""
    if "list" in page.title.lower():
        return False
    if is_person_page(page):
        return False
    return len([c for c in page.categories
                if Keywords.check_match("places", c)
                and "list" not in c.lower()
                and "errors" not in c.lower()
                and "wikipedia" not in c.lower()]) > 0

def is_weapon_page(page):
    """Determines whether or not the given page object is a weapon page."""
    if "list" in page.title.lower():
        return False
    if is_person_page(page):
        return False
    return len([c for c in page.categories
                if Keywords.check_match("weapons", c)
                and "list" not in c.lower()
                and "errors" not in c.lower()
                and "wikipedia" not in c.lower()]) > 0

def is_monster_page(page):
    """Determines whether or not the given page object is a monster page."""
    if "list" in page.title.lower():
        return False
    if is_person_page(page):
        return False
    return len([c for c in page.categories
                if Keywords.check_match("monsters", c)
                and "list" not in c.lower()
                and "errors" not in c.lower()
                and "wikipedia" not in c.lower()]) > 0

def is_animal_page(page):
    """Determines whether or not the given page object is aa animal page."""
    if "list" in page.title.lower():
        return False
    if is_person_page(page):
        return False
    return len([c for c in page.categories
                if Keywords.check_match("animals", c)
                and "list" not in c.lower()
                and "errors" not in c.lower()
                and "wikipedia" not in c.lower()]) > 0

def is_food_page(page):
    """Determines whether or not the given page object is a food page."""
    if "list" in page.title.lower():
        return False
    if is_person_page(page):
        return False
    return len([c for c in page.categories
                if Keywords.check_match("foods", c)
                and "list" not in c.lower()
                and "errors" not in c.lower()
                and "wikipedia" not in c.lower()]) > 0

class Basis:
    """
    A class grouping basis page functions with match predicates. The basis function should be a function or
    other callable object of zero arguments which returns the name (as a string) of a page to start a crawl
    on. Often, the basis function will either be a constant function or one that uses random numbers to
    determine the base page. The predicate function should be a 1-ary function or callable object which
    returns True if the page matches the given category and False otherwise.
    """

    def __init__(self, base, pred):
        self.get_base = base
        self.is_page = pred

    def with_page(self, base):
        return Basis(base, self.is_page)

    def with_predicate(self, pred):
        return Basis(self.get_base, pred)

Basis.celebrity = Basis(get_celebrity_base, is_person_page )
Basis.person    = Basis(get_person_base   , is_person_page )
Basis.place     = Basis(get_place_base    , is_place_page  )
Basis.weapon    = Basis(get_weapon_base   , is_weapon_page )
Basis.monster   = Basis(get_monster_base  , is_monster_page)
Basis.animal    = Basis(get_animal_base   , is_animal_page )
Basis.food      = Basis(get_food_base     , is_food_page   )
