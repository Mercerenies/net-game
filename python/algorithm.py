
import wikipedia
import sys
from sys import stderr
from util import escape
from links import BasicLinkSelector
import time

DELAY = 0.5

class Spider:

    def __init__(self, selector = None, depth = 5, max_tries = 3, debug = False):
        if selector is None:
            selector = BasicLinkSelector()
        self.depth = depth
        self.max_tries = max_tries
        self.debug = debug
        self.selector = selector

    def wait(self, delay = DELAY):
        time.sleep(delay)

    def echo(self, *args, flush = False):
        if self.debug:
            print(*args, file=stderr)
            if flush:
                sys.stderr.flush()

    def crawl_once(self, base, match_function):
        def _crawl_once(page, depth_):
            self.wait()
            self.echo("Trying", escape(page.title), "at", depth_, flush = True)
            if match_function(page):
                self.echo("Taking", escape(page.title))
                return page
            elif depth_ >= self.depth:
                return None
            else:
                link = self.selector.select_link(page)
                new_page = wikipedia.page(link)
                return _crawl_once(new_page, depth_ + 1)
        if type(base) is str:
            base = wikipedia.page(base)
        return self.safely_call(lambda: _crawl_once(base, 0))

    def safely_call(self, func):
        try:
            return func()
        except wikipedia.PageError as e:
            self.echo("Found red link", e)
            return None
        except wikipedia.DisambiguationError as e:
            self.echo("Ambiguous article found", e)
            return None

    def crawl_times(self, base, match_function):
        if type(base) is str:
            base = wikipedia.page(base)
        for i in range(0, self.max_tries):
            res = self.crawl_once(base, match_function)
            if res:
                return res

def nearby(x):
    try:
        (lat, lon) = x.coordinates
        return wikipedia.geosearch(lat, lon, radius=10000)
    except KeyError: # Accessing coordinates of a page that doesn't have them appears
                     # to throw a key error, despite documentation to the contrary.
        return None
    return None
