#!/usr/bin/python3

import sys
from getopt import getopt
import xml.etree.ElementTree as ET
from basis import Basis
import algorithm
import xmlify
import links
import reinforcement
import logger

def do_search(basis, number, selector = None, **key):
    """
    Performs an attempt to obtain the specified number of pages from the Internet.
    The basis argument should be an instance of the Basis class. The number specifies
    how many full spider attempts should be made. This is an upper bound on the number
    of pages obtained, but the actual number might be less than this argument. The optional
    selector argument specifies the selector to use in the search algorithm. If not supplied,
    a simple default is constructed. This procedure returns a list of resulting pages. Any
    excess key arguments are passed to algorithm.Spider unmodified.
    """
    if number <= 0:
        return []
    if selector is None:
        selector = links.NoDupLinkSelector()
    spider = algorithm.Spider(selector = selector, **key)
    arr = []
    for i in range(0, number):
        base = basis.get_base()
        curr = spider.crawl_times(base, basis.is_page)
        if curr:
            arr.append(curr)
    spider.finished()
    return arr

def make_sel(keyword, rein):
    """Constructs a simple selector or reinforcement learning selector, depending on arguments."""
    if rein:
        return reinforcement.ReinLinkSelector(keyword)
    else:
        return links.NoDupLinkSelector()

if __name__ == '__main__':
    args = dict(getopt(sys.argv[1:], "c:p:P:w:m:a:f:d:r")[0])
    celebs = int(args.get("-c", "0"))
    people = int(args.get("-p", "0"))
    places = int(args.get("-P", "0"))
    weapons = int(args.get("-w", "0"))
    monsters = int(args.get("-m", "0"))
    animals = int(args.get("-a", "0"))
    foods = int(args.get("-f", "0"))
    debug = int(args.get("-d", "0"))
    logger.set_global_debug_level(debug)
    rein = "-r" in args
    parts = {
        'celebs':    do_search(Basis.celebrity, celebs  , make_sel('people',   rein)),
        'people':    do_search(Basis.person   , people  , make_sel('people',   rein)),
        'places':    do_search(Basis.place    , places  , make_sel('places',   rein)),
        'weapons':   do_search(Basis.weapon   , weapons , make_sel('weapons',  rein)),
        'monsters':  do_search(Basis.monster  , monsters, make_sel('monsters', rein)),
        'animals':   do_search(Basis.animal   , animals , make_sel('animals',  False)),
        'foods':     do_search(Basis.food     , foods   , make_sel('foods',    False)),
    }
    print(ET.tostring(xmlify.xmlify(parts)).decode())
