
import algorithm

class Search:
    def run(self):
        """Runs the search, as appropriate for the specific subclass of Search."""
        raise NotImplementedError("run not implemented!")

class BasicSearch(Search):

    def __init__(self, *, basis, number, selector, keys):
        self.basis = basis
        self.number = number
        self.selector = selector
        self.keys = keys

    def run(self):
        """
        Performs an attempt to obtain the specified number of pages from the Internet.
        The basis argument should be an instance of the Basis class. The number specifies
        how many full spider attempts should be made. This is an upper bound on the number
        of pages obtained, but the actual number might be less than this argument. The optional
        selector argument specifies the selector to use in the search algorithm. If not supplied,
        a simple default is constructed. This procedure returns a list of resulting pages. Any
        excess key arguments are passed to algorithm.Spider unmodified.
        """
        if self.number <= 0:
            return []
        spider = algorithm.Spider(selector = self.selector, **self.keys)
        arr = []
        for i in range(0, self.number):
            base = self.basis.get_base()
            curr = spider.crawl_times(base, self.basis.is_page)
            if curr:
                arr.append(curr)
        spider.finished()
        return arr
