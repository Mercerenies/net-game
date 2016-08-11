
import string
import random
from util import rnd
from keywords import Keywords

def get_person_base():
    return "Lists of people"

def get_celebrity_base():
    return "Lists of celebrities"

def get_place_base():
    return rnd(["Lists of places", "List of buildings and structures"])

def get_weapon_base():
    return rnd(["List of premodern combat weapons", "List of medieval weapons"])

def get_monster_base():
    letter = random.choice(string.ascii_uppercase)
    return "List of legendary creatures ({})".format(letter)

def get_animal_base():
    return "List of animals by common name"

def get_food_base():
    return rnd(["List of culinary fruits", "List of vegetables"])

def is_person_page(page):
    if "list" in page.title.lower():
        return False
    return len([c for c in page.categories
                if Keywords.check_match("people", c)
                and "list" not in c.lower()
                and "errors" not in c.lower()
                and "wikipedia" not in c.lower()]) > 0

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

class Basis:
    celebrity = (get_celebrity_base, is_person_page )
    person    = (get_person_base   , is_person_page )
    place     = (get_place_base    , is_place_page  )
    weapon    = (get_weapon_base   , is_weapon_page )
    monster   = (get_monster_base  , is_monster_page)
    animal    = (get_animal_base   , is_animal_page )
    food      = (get_food_base     , is_food_page   )
