
import wikipedia
import sys
from sys import stderr
from util import *
import time
import string
from keywords import Keywords

DELAY = 0.5

def recurse(base, match_function, depth = 5, max_tries = 3, debug = False):
    def find_inner(page, depth_):
        time.sleep(DELAY)
        if debug:
            print("Trying", escape(page.title), "at", depth_, file=stderr)
        sys.stderr.flush()
        if match_function(page):
            if debug:
                print("Taking", escape(page.title), file=stderr)
            return page
        elif depth_ >= depth:
            return None
        else:
            link = rnd(page.links)
            new_page = wikipedia.page(link)
            return find_inner(new_page, depth_ + 1)
    toplevel = wikipedia.page(base)
    for i in range(0, max_tries):
        try:
            res = find_inner(toplevel, 0)
        except wikipedia.PageError as e:
            if debug:
                print("Found red link", e, file=stderr)
            res = None
        except wikipedia.DisambiguationError as e:
            if debug:
                print("Ambiguous article found", e, file=stderr)
            res = None
        if res:
            return res

def is_person_page(page):
    if "list" in page.title.lower():
        return False
    return len([c for c in page.categories
                if Keywords.check_match("people", c)
                and "list" not in c.lower()
                and "errors" not in c.lower()
                and "wikipedia" not in c.lower()]) > 0

def find_a_person(depth = 5, max_tries = 3, debug = False):
    return recurse("Lists of people", is_person_page, depth, max_tries, debug)

def find_a_celebrity(**key):
    return recurse("Lists of celebrities", is_person_page, **key)

def is_place_page(page):
    if "list" in page.title.lower():
        return False
    if is_person_page(page):
        return False
    return len([c for c in page.categories
                if Keywords.check_match("places", c)
                and "list" not in c.lower()
                and "errors" not in c.lower()
                and "wikipedia" not in c.lower()]) > 0

def find_a_place(**key):
    basis = rnd(["Lists of places", "List of buildings and structures"])
    return recurse(basis, is_place_page, **key)

def is_weapon_page(page):
    if "list" in page.title.lower():
        return False
    if is_person_page(page):
        return False
    return len([c for c in page.categories
                if Keywords.check_match("weapons", c)
                and "list" not in c.lower()
                and "errors" not in c.lower()
                and "wikipedia" not in c.lower()]) > 0

def find_a_weapon(**key):
    basis = rnd(["List of premodern combat weapons", "List of medieval weapons"])
    return recurse(basis, is_weapon_page, **key)

def is_monster_page(page):
    if "list" in page.title.lower():
        return False
    if is_person_page(page):
        return False
    return len([c for c in page.categories
                if Keywords.check_match("monsters", c)
                and "list" not in c.lower()
                and "errors" not in c.lower()
                and "wikipedia" not in c.lower()]) > 0

def find_a_monster(**key):
    letter = random.choice(string.ascii_uppercase)
    pagename = "List of legendary creatures ({})".format(letter)
    return recurse(pagename, is_monster_page, **key)

def is_animal_page(page):
    if "list" in page.title.lower():
        return False
    if is_person_page(page):
        return False
    return len([c for c in page.categories
                if Keywords.check_match("animals", c)
                and "list" not in c.lower()
                and "errors" not in c.lower()
                and "wikipedia" not in c.lower()]) > 0

def find_a_animal(**key):
    return recurse("List of animals by common name", is_animal_page, **key)

def is_food_page(page):
    if "list" in page.title.lower():
        return False
    if is_person_page(page):
        return False
    return len([c for c in page.categories
                if Keywords.check_match("foods", c)
                and "list" not in c.lower()
                and "errors" not in c.lower()
                and "wikipedia" not in c.lower()]) > 0

def find_a_food(**key):
    basis = rnd(["List of culinary fruits", "List of vegetables"])
    return recurse(basis, is_food_page, **key)

def nearby(x):
    try:
        (lat, lon) = x.coordinates
        return wikipedia.geosearch(lat, lon, radius=10000)
    except KeyError: # Accessing coordinates of a page that doesn't have them appears
                     # to throw a key error, despite documentation to the contrary.
        return None
    return None
