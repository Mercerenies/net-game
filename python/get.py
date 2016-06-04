#!/usr/bin/python3

# TODO Consider making a reinforcement learning engine to improve the crawling

import sys
from getopt import getopt
import xml.etree.ElementTree as ET
import algorithm
import xmlify

def do_search(script, number, **key):
    arr = []
    for i in range(0, number):
        curr = script(**key)
        if curr:
            arr.append(curr)
    return arr

if __name__ == '__main__':
    args = dict(getopt(sys.argv[1:], "c:p:P:w:m:a:f:d")[0])
    celebs = int(args.get("-c", "0"))
    people = int(args.get("-p", "0"))
    places = int(args.get("-P", "0"))
    weapons = int(args.get("-w", "0"))
    monsters = int(args.get("-m", "0"))
    animals = int(args.get("-a", "0"))
    foods = int(args.get("-f", "0"))
    debug = "-d" in args
    parts = {
        'celebs':    do_search(algorithm.find_a_celebrity, celebs  , debug = debug),
        'people':    do_search(algorithm.find_a_person   , people  , debug = debug),
        'places':    do_search(algorithm.find_a_place    , places  , debug = debug),
        'weapons':   do_search(algorithm.find_a_weapon   , weapons , debug = debug),
        'monsters':  do_search(algorithm.find_a_monster  , monsters, debug = debug),
        'animals':   do_search(algorithm.find_a_animal   , animals , debug = debug),
        'foods':     do_search(algorithm.find_a_food     , foods   , debug = debug),
    }
    print(ET.tostring(xmlify.xmlify(parts)).decode())
