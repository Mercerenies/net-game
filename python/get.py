#!/usr/bin/python3

import sys
import xml.etree.ElementTree as ET
from arguments import TopLevelArguments
import xmlify
import links
import reinforcement
import logger
import search
import command
from tokenizer import TokenizeError

def expr_run(args):
    try:
        exprs = command.read(args.expr())
        parts = {}
        for expr in exprs:
            search.CommandSearch(expr, parts).run()
        print(ET.tostring(xmlify.xmlify(parts)).decode())
    except TokenizeError as e:
        logger.echo(str(e))
        print("<data />")

if __name__ == '__main__':
    args = TopLevelArguments(sys.argv[1:])
    logger.set_global_debug_level(args.debug())
    expr_run(args)
