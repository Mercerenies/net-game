#!/usr/bin/python

# ///// This needs to be modified; it mucks up the Perl side of things
#       by printing wrong if there's an apostrophe in the title.
#       We need to migrate this to some sort of markup (XML?) anyway.

import wikipedia
import sys
from sys import stderr
from random import *
import math
import time
from getopt import getopt

DELAY = 0.5

def rnd(lst):
    return lst[math.floor(random() * len(lst))]

def clamp(x, a, b):
    if x < a:
        return a
    elif x > b:
        return b
    else:
        return x

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
                and "wikipedia" not in c.lower()]) > 0

def find_a_place(**key):
    return recurse("Lists of places", is_place_page, **key)

def is_weapon_page(page):
    if "list" in page.title.lower():
        return False
    if is_person_page(page):
        return False
    return len([c for c in page.categories
                if Keywords.check_match("weapons", c)
                and "list" not in c.lower()
                and "wikipedia" not in c.lower()]) > 0

def find_a_weapon(**key):
    basis = rnd(["List of premodern combat weapons", "List of medieval weapons"])
    return recurse(basis, is_weapon_page, **key)

def nearby(x):
    try:
        (lat, lon) = x.coordinates
        return wikipedia.geosearch(lat, lon, radius=10000)
    except KeyError: # Accessing coordinates of a page that doesn't have them appears
                     # to throw a key error, despite documentation to the contrary.
        return None
    return None

def escape(str_):
    return repr(str_.encode('utf-8'))

def do_search(script, number, **key):
    for i in range(0, number):
        curr = script(**key)
        if curr:
            print(escape(curr.title),
                  escape(curr.summary),
                  sep='  ')

class Keywords:
    _key = None
    @staticmethod
    def _load_file():
        Keywords._key = dict()
        state = ""
        with open("./data/keywords.txt") as f:
            data = f.read()
        for curr in data.split("\n"):
            if curr == "":
                continue
            if curr[:3] == ":: ":
                state = curr[3:]
            else:
                Keywords._key[state] = Keywords._key.get(state, []) + [curr]
    @staticmethod
    def check_match(key, c):
        if not Keywords._key:
            Keywords._load_file()
        for x in Keywords._key[key]:
            if x in c.lower():
                return True
        return False

if __name__ == '__main__':
    args = dict(getopt(sys.argv[1:], "c:p:P:w:d")[0])
    celebs = int(args.get("-c", "0"))
    people = int(args.get("-p", "0"))
    places = int(args.get("-P", "0"))
    weapons = int(args.get("-w", "0"))
    debug = "-d" in args
    do_search(find_a_celebrity, celebs, debug = debug)
    print("BREAK")
    do_search(find_a_person, people, debug = debug)
    print("BREAK")
    do_search(find_a_place, places, debug = debug)
    print("BREAK")
    do_search(find_a_weapon, weapons, debug = debug)
    print("BREAK")
