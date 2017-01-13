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

def produce_result(args, mod = (lambda x: x), selector = None, **key):
    """
    Runs all of the algorithms given in the args dict, using the basis modifier to augment each
    basis. The basis modifier should be a 1-argument callable which takes a basis and returns
    a new basis, and it should be side-effect free.
    """
    celebs = int(args.get("-c", "0"))
    people = int(args.get("-p", "0"))
    places = int(args.get("-P", "0"))
    weapons = int(args.get("-w", "0"))
    monsters = int(args.get("-m", "0"))
    animals = int(args.get("-a", "0"))
    foods = int(args.get("-f", "0"))
    rein = "-r" in args

    parts = {
        'celebs':   do_search(mod(Basis.celebrity), celebs  , selector or make_sel('people',   rein ), **key),
        'people':   do_search(mod(Basis.person   ), people  , selector or make_sel('people',   rein ), **key),
        'places':   do_search(mod(Basis.place    ), places  , selector or make_sel('places',   rein ), **key),
        'weapons':  do_search(mod(Basis.weapon   ), weapons , selector or make_sel('weapons',  rein ), **key),
        'monsters': do_search(mod(Basis.monster  ), monsters, selector or make_sel('monsters', rein ), **key),
        'animals':  do_search(mod(Basis.animal   ), animals , selector or make_sel('animals',  False), **key),
        'foods':    do_search(mod(Basis.food     ), foods   , selector or make_sel('foods',    False), **key),
    }
    return xmlify.xmlify(parts)


def standard_run(args):
    print(ET.tostring(produce_result(args)).decode())

def unit_run(args):
    unit = args.get("-u", "").strip()

    def basis_transformer(b):
        return b.with_page(lambda: unit)

    print(ET.tostring(
        produce_result(
            args,
            mod = basis_transformer,
            selector = links.AbortLinkSelector(),
            max_tries = 1
        )
    ).decode())

if __name__ == '__main__':
    args = dict(getopt(sys.argv[1:], "c:p:P:w:m:a:f:d:ru:")[0])
    debug = int(args.get("-d", "0"))
    logger.set_global_debug_level(debug)
    if '-u' in args:
        unit_run(args)
    else:
        standard_run(args)
