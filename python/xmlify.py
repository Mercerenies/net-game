
import re
import xml.etree.ElementTree as ET

def xmlify_once(page):
    stack = [ET.Element("page", name = page.title)]
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

def xmlify(pages): # Pages should be a dict with key strings and value lists of pages
    root = ET.Element("data")
    for key, value in pages.items():
        curr = ET.Element("pages", type = key)
        for page in value:
            curr.append(xmlify_once(page))
        root.append(curr);
    return root
