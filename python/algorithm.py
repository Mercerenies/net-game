
import wikipedia
import sys
from util import escape
from links import BasicLinkSelector
import time
from logger import echo
from links import LinkState

DELAY = 0.5

class Spider:
    """
    The Spider class is responsible for the actual crawling of the Internet and stores state information
    such as the link selector being used, as well as the depth and number of attempts to make during
    a crawl.
    """

    def __init__(self, selector = None, depth = 5, max_tries = 3):
        """
        Initialize the Spider. The selector should be either None (in which case it will default to
        a BasicLinkSelector) or a LinkSelector object. The depth and max_tries should be positive
        integers, which respectively indicate the depth of an individual attempt and the number of
        attempts to make during a single crawl.
        """
        if selector is None:
            selector = BasicLinkSelector()
        self.depth = depth
        self.max_tries = max_tries
        self.selector = selector

    def finished(self):
        """A callback indicating that the user is done using the Spider and, by extension, the LinkSelector."""
        self.selector.finished()

    def wait(self, delay = DELAY):
        """Pause for a certain period of time, defaulting to the global constant DELAY."""
        time.sleep(delay)

    def crawl_once(self, base, match_function):
        """
        Make a single crawl, using the Spider's link selector and parameters with the supplied matching
        function. A single crawl will start at the base page supplied and, using the link selector, follow
        links until it finds a match (according to the supplied match_function) or until it has gone
        self.depth layers deep and gives up. The match function should be a function of one argument,
        the page, and the base page should be either a string name or a page object. The result is
        the final page object or None. Note that a direct caller of this method should take care
        to inform the LinkSelector of the start and end of the crawl, as this method only performs
        the actual crawl.
        """
        # TODO Move the start_crawl/end_crawl code from crawl_times to here; it makes more sense here
        def _crawl_once(page, depth_):
            self.wait()
            echo("Trying", escape(page.title), "at", depth_, flush = True)
            if match_function(page):
                echo("Taking", escape(page.title))
                return page
            elif depth_ >= self.depth:
                return None
            else:
                state = LinkState(page, self.depth - depth_, self.depth)
                link = self.selector.select_link(state)
                if link is None:
                    return None
                new_page = wikipedia.page(link)
                return _crawl_once(new_page, depth_ + 1)
        if type(base) is str:
            base = wikipedia.page(base)
        return self.safely_call(lambda: _crawl_once(base, 0))

    def safely_call(self, func):
        """A convenience function which calls the 0-ary function supplied, while handling Wikipedia errors."""
        try:
            return func()
        except wikipedia.PageError as e:
            echo("Found red link", e)
            return None
        except wikipedia.DisambiguationError as e:
            echo("Ambiguous article found", e)
            return None

    def crawl_times(self, base, match_function):
        """
        Perform self.crawl_once until a single match is found or self.max_tries attempts have
        been made.
        """
        if type(base) is str:
            base = wikipedia.page(base)
        for i in range(0, self.max_tries):
            self.selector.start_crawl()
            res = self.crawl_once(base, match_function)
            self.selector.end_crawl(bool(res))
            if res:
                return res

def nearby(x):
    """Locate and return any Wikipedia page whose physical location is near that of the page supplied."""
    try:
        (lat, lon) = x.coordinates
        return wikipedia.geosearch(lat, lon, radius=10000)
    except KeyError: # Accessing coordinates of a page that doesn't have them appears
                     # to throw a key error, despite documentation to the contrary.
        return None
