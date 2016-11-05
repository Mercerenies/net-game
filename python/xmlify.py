
import re
import xml.etree.ElementTree as ET

def xmlify_links(page):
    """
    Given a Wikipedia page, encodes the links in an XML element.
    """
    elem = ET.Element("links")
    for link in page.links:
        curr = ET.Element("link")
        curr.text = link
        elem.append(curr)
    return elem

def xmlify_content(page):
    """
    Given a single Wikipedia page object, converts its text into an XML structure organized by
    page sections.
    """
    stack = [ET.Element("text")]
    re_titles = re.compile(r'^(=+) *(.*) *\1$')
    for line in page.content.splitlines():
        match = re.match(re_titles, line)
        if match:
            n = max(len(match.group(1)), 2)
            if len(stack) >= n:
                while len(stack) > n - 1:
                    elem = stack.pop()
                    stack[-1].append(elem)
            stack.append(ET.Element("section",
                                    name = match.group(2).strip(),
                                    depth = str(n)))
        else:
            if stack[-1].text is None:
                stack[-1].text = ''
            stack[-1].text += "\n" + line
    while len(stack) > 1:
        elem = stack.pop();
        stack[-1].append(elem)
    return stack[0]

def xmlify_once(page):
    """
    Given a single Wikipedia page object, converts all of its structure into an XML element ready
    for saving.
    """
    content = xmlify_content(page)
    links = xmlify_links(page)
    parent = ET.Element("page", name = page.title)
    parent.append(content)
    parent.append(links)
    return parent

def xmlify(pages): # Pages should be a dict with key strings and value lists of pages
    """
    Organizes a collection of pages into an XML structure, using xmlify_once. The argument
    should be a dictionary containing string 'type' keywords as keys and lists of pages
    satisfying that 'type' of page as values.
    """
    root = ET.Element("data")
    for key, value in pages.items():
        curr = ET.Element("pages", type = key)
        for page in value:
            curr.append(xmlify_once(page))
        root.append(curr);
    return root
