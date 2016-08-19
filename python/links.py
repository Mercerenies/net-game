
import sys
from util import rnd
from logger import echo

class LinkState:
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
    def finished(self):
        pass
    def start_crawl(self):
        pass
    def end_crawl(self, success):
        pass
    def select_link(self, state):
        raise NotImplementedError("select_link not implemented!")

class BasicLinkSelector(LinkSelector):
    def select_link(self, state):
        return rnd(state.page().links)

# TODO The no-dup rule is NOT working for some reason; also, same problem in reinforcement.py
class NoDupLinkSelector(LinkSelector):
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
