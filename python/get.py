#!/usr/bin/python

# TODO Consider making a reinforcement learning engine to improve the crawling

import wikipedia
import sys
from sys import stderr
from random import *
import math
import time
import string
from getopt import getopt
import xml.etree.ElementTree as ET
import re

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
    return recurse("Lists of places", is_place_page, **key)

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
    letter = choice(string.ascii_uppercase)
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
    arr = []
    for i in range(0, number):
        curr = script(**key)
        if curr:
            arr.append(curr)
    return arr

def xmlify_once(page):
    stack = [ET.Element("page", name = page.title)]
    re_titles = re.compile(r'^(=+) *(.*) *\1$')
    for line in page.content.splitlines():
        match = re.match(re_titles, line)
        if match:
            n = max(len(match.group(1)), 2)
            if len(stack) >= n:
                while len(stack) > n - 1:
                    elem = stack.pop()
                    stack[-1].append(elem)
            stack.append(ET.Element("section",
                                    name = match.group(2).strip(),
                                    depth = str(n)))
        else:
            if stack[-1].text is None:
                stack[-1].text = ''
            stack[-1].text += "\n" + line
    while len(stack) > 1:
        elem = stack.pop();
        stack[-1].append(elem)
    return stack[0]

def xmlify(pages): # Pages should be a dict with key strings and value lists of pages
    root = ET.Element("data")
    for key, value in pages.items():
        curr = ET.Element("pages", type = key)
        for page in value:
            curr.append(xmlify_once(page))
        root.append(curr);
    return root

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
    args = dict(getopt(sys.argv[1:], "c:p:P:w:m:a:d")[0])
    celebs = int(args.get("-c", "0"))
    people = int(args.get("-p", "0"))
    places = int(args.get("-P", "0"))
    weapons = int(args.get("-w", "0"))
    monsters = int(args.get("-m", "0"))
    animals = int(args.get("-a", "0"))
    debug = "-d" in args
    parts = {
        'celebs':  do_search(find_a_celebrity, celebs, debug = debug),
        'people':  do_search(find_a_person, people, debug = debug),
        'places':  do_search(find_a_place, places, debug = debug),
        'weapons': do_search(find_a_weapon, weapons, debug = debug),
        'monsters': do_search(find_a_monster, monsters, debug = debug),
        'animals': do_search(find_a_animal, animals, debug = debug),
    }
    print(ET.tostring(xmlify(parts)).decode())
