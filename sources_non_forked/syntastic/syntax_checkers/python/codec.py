#!/usr/bin/env python

from __future__ import print_function
from sys import argv, exit

import codecs
import re
import os


if len(argv) != 2:
    exit(1)

try:
    with open(argv[1]) as fle:
        text = fle.readlines()

    if text:
        match = re.match(r"#\s*coding\s*:\s*(?P<coding>\w+)", text[0])
        if match:
            text = codecs.lookup(match.groupdict()["coding"]).incrementaldecoder().decode(
                ''.join(text).encode('utf-8')).encode('utf-8')

    if isinstance(text, list):
        text = ''.join(text).encode('utf-8')

    compile(text, argv[1], 'exec', 0, 1)
except SyntaxError as err:
    print('%s:%s:%s: %s' % (err.filename, err.lineno, err.offset, err.msg))
except Exception as err:
    print('%s:%s:%s: %s' % (os.path.abspath(argv[1]), 1, 0, err))
