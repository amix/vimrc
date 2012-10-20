# voom_mode_vimwiki.py
# Last Modified: 2011-05-01
# VOoM -- Vim two-pane outliner, plugin for Python-enabled Vim version 7.x
# Website: http://www.vim.org/scripts/script.php?script_id=2657
# Author: Vlad Irnov (vlad DOT irnov AT gmail DOT com)
# License: This program is free software. It comes without any warranty,
#          to the extent permitted by applicable law. You can redistribute it
#          and/or modify it under the terms of the Do What The Fuck You Want To
#          Public License, Version 2, as published by Sam Hocevar.
#          See http://sam.zoy.org/wtfpl/COPYING for more details.

"""
VOoM markup mode for headline markup used by vimwiki plugin:
http://www.vim.org/scripts/script.php?script_id=2226
See |voom_mode_vimwiki|,  ../../doc/voom.txt#*voom_mode_vimwiki*

= headline level 1 =
body text
== headline level 2 ==
body text
  === headline level 3 ===

"""

import re
headline_match = re.compile(r'^\s*(=+).+(\1)\s*$').match


def hook_makeOutline(VO, blines):
    """Return (tlines, bnodes, levels) for Body lines blines.
    blines is either Vim buffer object (Body) or list of buffer lines.
    """
    Z = len(blines)
    tlines, bnodes, levels = [], [], []
    tlines_add, bnodes_add, levels_add = tlines.append, bnodes.append, levels.append
    for i in xrange(Z):
        bline = blines[i].strip()
        if not bline.startswith('='):
            continue
        m = headline_match(bline)
        if not m:
            continue
        lev = len(m.group(1))
        bline = bline.strip()
        head = bline[lev:-lev].strip()
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
    bodyLines = ['%s %s %s' %('='*level, tree_head, '='*level), '']
    return (tree_head, bodyLines)


def hook_changeLevBodyHead(VO, h, levDelta):
    """Increase of decrease level number of Body headline by levDelta."""
    if levDelta==0: return h
    m = headline_match(h)
    level = len(m.group(1))
    s = '='*(level+levDelta)
    return '%s%s%s%s%s' %(h[:m.start(1)], s, h[m.end(1):m.start(2)], s, h[m.end(2):])

