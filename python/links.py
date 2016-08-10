
from util import rnd

class LinkSelector:
    def select_link(self, page):
        raise NotImplementedError("select_link not implemented!")

class BasicLinkSelector(LinkSelector):
    def select_link(self, page):
        return rnd(page.links)
