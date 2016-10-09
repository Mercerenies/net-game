
import sys
from util import rnd
from logger import echo

class LinkState:
    """
    An instance of the LinkState class maintains the current state of the spider algorithm, from
    a link-following perspective. It is passed to LinkSelector instances and is used to determine
    where to go next.
    """
    def __init__(self, page, depth_left, total_depth):
        self._page = page
        self._depth_left = depth_left
        self._total_depth = total_depth
    def page(self):
        return self._page
    def depth_left(self):
        return self._depth_left
    def total_depth(self):
        return self._total_depth

class LinkSelector:
    """
    LinkSelector is an abstract class for selecting links on a page. Children of this class must override
    select_link. The other methods are called at the appropriate times and can be overriden but are optional.
    """
    def finished(self):
        pass
    def start_crawl(self):
        pass
    def end_crawl(self, success):
        pass
    def select_link(self, state):
        raise NotImplementedError("select_link not implemented!")

class BasicLinkSelector(LinkSelector):
    """A basic link selector that chooses a link randomly."""
    def select_link(self, state):
        return rnd(state.page().links)

# TODO The no-dup rule is NOT working because link names can differ in subtle ways not accounted for
#      in this class. Also, same problem in reinforcement.py
class NoDupLinkSelector(LinkSelector):
    """A link selector that attempts to avoid cycles when following links."""
    def __init__(self):
        self.pages = []
    def start_crawl(self):
        self.pages = []
    def select_link(self, state):
        page = state.page()
        self.pages.append(page.title)
        links = list(filter(lambda x: x not in self.pages, page.links))
        if links:
            return rnd(links)
        else:
            return None
