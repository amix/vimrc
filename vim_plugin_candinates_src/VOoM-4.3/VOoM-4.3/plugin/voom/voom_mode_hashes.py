# voom_mode_hashes.py
# Last Modified: 2012-05-06
# VOoM -- Vim two-pane outliner, plugin for Python-enabled Vim version 7.x
# Website: http://www.vim.org/scripts/script.php?script_id=2657
# Author: Vlad Irnov (vlad DOT irnov AT gmail DOT com)
# License: This program is free software. It comes without any warranty,
#          to the extent permitted by applicable law. You can redistribute it
#          and/or modify it under the terms of the Do What The Fuck You Want To
#          Public License, Version 2, as published by Sam Hocevar.
#          See http://sam.zoy.org/wtfpl/COPYING for more details.

"""
VOoM markup mode for headlines marked with #'s (atx-headers, a subset of Markdown format).
See |voom_mode_hashes|,  ../../doc/voom.txt#*voom_mode_hashes*

# heading level 1
##heading level 2
### heading level 3
"""

import re

# Marker character can be changed to any ASCII character.
CH = '#'

# Use this if whitespace after marker chars is optional.
headline_match = re.compile(r'^(%s+)' %re.escape(CH)).match
# Use this if a whitespace is required after marker chars (as in org-mode).
#headline_match = re.compile(r'^(%s+)\s' %re.escape(CH)).match


def hook_makeOutline(VO, blines):
    """Return (tlines, bnodes, levels) for Body lines blines.
    blines is either Vim buffer object (Body) or list of buffer lines.
    """
    Z = len(blines)
    tlines, bnodes, levels = [], [], []
    tlines_add, bnodes_add, levels_add = tlines.append, bnodes.append, levels.append
    for i in xrange(Z):
        if not blines[i].startswith(CH):
            continue
        bline = blines[i]
        m = headline_match(bline)
        if not m:
            continue
        lev = len(m.group(1))
        head = bline[lev:].strip()
        # Do this instead if optional closing markers need to be stripped.
        #head = bline[lev:].strip().rstrip(CH).rstrip()
        tline = '  %s|%s' %('. '*(lev-1), head)
        tlines_add(tline)
        bnodes_add(i+1)
        levels_add(lev)
    return (tlines, bnodes, levels)


def hook_newHeadline(VO, level, blnum, tlnum):
    """Return (tree_head, bodyLines).
    tree_head is new headline string in Tree buffer (text after |).
    bodyLines is list of lines to insert in Body buffer.
    """
    tree_head = 'NewHeadline'
    bodyLines = ['%s %s' %(CH * level, tree_head), '']
    return (tree_head, bodyLines)


def hook_changeLevBodyHead(VO, h, levDelta):
    """Increase of decrease level number of Body headline by levDelta."""
    if levDelta==0: return h
    m = headline_match(h)
    level = len(m.group(1))
    return '%s%s' %(CH * (level+levDelta), h[m.end(1):])


