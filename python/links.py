
import sys
from util import rnd

class LinkSelector:
    def finished(self):
        pass
    def start_crawl(self):
        pass
    def end_crawl(self, success):
        pass
    def select_link(self, page):
        raise NotImplementedError("select_link not implemented!")

class BasicLinkSelector(LinkSelector):
    def select_link(self, page):
        return rnd(page.links)

class NoDupLinkSelector(LinkSelector):
    def __init__(self):
        self.pages = []
    def start_crawl(self):
        self.pages = []
    def select_link(self, page):
        self.pages.append(page.title)
        links = list(filter(lambda x: x not in self.pages, page.links))
        if links:
            return rnd(links)
        else:
            return None
