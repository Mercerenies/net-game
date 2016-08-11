#!/usr/bin/python3

# TODO Consider making a reinforcement learning engine to improve the crawling

import sys
from getopt import getopt
import xml.etree.ElementTree as ET
from basis import Basis
import algorithm
import xmlify

def do_search(tup, number, **key):
    base_script, match_func = tup
    spider = algorithm.Spider(**key)
    arr = []
    for i in range(0, number):
        base = base_script()
        curr = spider.crawl_times(base, match_func)
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
        'celebs':    do_search(Basis.celebrity, celebs  , debug = debug),
        'people':    do_search(Basis.person   , people  , debug = debug),
        'places':    do_search(Basis.place    , places  , debug = debug),
        'weapons':   do_search(Basis.weapon   , weapons , debug = debug),
        'monsters':  do_search(Basis.monster  , monsters, debug = debug),
        'animals':   do_search(Basis.animal   , animals , debug = debug),
        'foods':     do_search(Basis.food     , foods   , debug = debug),
    }
    print(ET.tostring(xmlify.xmlify(parts)).decode())
