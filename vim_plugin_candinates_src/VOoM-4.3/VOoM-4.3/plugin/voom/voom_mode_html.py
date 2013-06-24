# voom_mode_html.py
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
VOoM markup mode for HTML headings.
See |voom_mode_html|,  ../../doc/voom.txt#*voom_mode_html*

<h1>headline level 1</h1>
some text
 <h2> headline level 2 </h2>
more text
 <H3  ALIGN="CENTER"> headline level 3 </H3>
 <  h4 >    headline level 4       </H4    >
  some text <h4> <font color=red> headline 5 </font> </H4> </td></div>
     etc.
"""

import re
headline_search = re.compile(r'<\s*h(\d+).*?>(.*?)</h(\1)\s*>', re.IGNORECASE).search
html_tag_sub = re.compile('<.*?>').sub


def hook_makeOutline(VO, blines):
    """Return (tlines, bnodes, levels) for Body lines blines.
    blines is either Vim buffer object (Body) or list of buffer lines.
    """
    Z = len(blines)
    tlines, bnodes, levels = [], [], []
    tlines_add, bnodes_add, levels_add = tlines.append, bnodes.append, levels.append
    for i in xrange(Z):
        bline = blines[i]
        if not ('</h' in bline or '</H' in bline):
            continue
        m = headline_search(bline)
        if not m:
            continue
        lev = int(m.group(1))
        head = m.group(2)
        # delete all html tags
        head = html_tag_sub('',head)
        tline = '  %s|%s' %('. '*(lev-1), head.strip())
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
    bodyLines = ['<h%s>%s</h%s>' %(level, tree_head, level), '']
    return (tree_head, bodyLines)


def hook_changeLevBodyHead(VO, h, levDelta):
    """Increase of decrease level number of Body headline by levDelta."""
    if levDelta==0: return h
    m = headline_search(h)
    level = int(m.group(1))
    lev = level+levDelta
    return '%s%s%s%s%s' %(h[:m.start(1)], lev, h[m.end(1):m.start(3)], lev, h[m.end(3):])

