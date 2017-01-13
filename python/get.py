#!/usr/bin/python3

import sys
import xml.etree.ElementTree as ET
from arguments import Arguments
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
    Runs all of the algorithms given in the Arguments object, using the basis modifier to augment each
    basis. The basis modifier should be a 1-argument callable which takes a basis and returns a new
    basis, and it should be side-effect free.
    """
    rein = args.rein()

    parts = {}
    for arg in args.standard_sequence():
        sel = selector or make_sel(arg.selector, rein and arg.rein)
        parts[arg.key] = do_search(mod(arg.basis), arg.count, sel, **key)
    return xmlify.xmlify(parts)


def standard_run(args):
    print(ET.tostring(produce_result(args)).decode())

def unit_run(args):
    unit = args.unit().strip()

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
    args = Arguments(sys.argv[1:])
    logger.set_global_debug_level(args.debug())
    if args.unit():
        unit_run(args)
    else:
        standard_run(args)
