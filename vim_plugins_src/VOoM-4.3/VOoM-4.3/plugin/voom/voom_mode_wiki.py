# voom_mode_wiki.py
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
VOoM markup mode for MediaWiki headline markup.
See |voom_mode_wiki|, ../../doc/voom.txt#*voom_mode_wiki*

= headline level 1 =
some text
== headline level 2 == 
more text
=== headline level 3 === <!--comment-->
==== headline level 4 ====<!--comment-->

"""

# can access main module voom.py, including global outline data
#import sys
#if 'voom' in sys.modules:
    #voom = sys.modules['voom']
    #VOOMS = voom.VOOMS

import re
comment_tag_sub = re.compile('<!--.*?-->\s*$').sub
headline_match = re.compile(r'^(=+).*(\1)\s*$').match


def hook_makeOutline(VO, blines):
    """Return (tlines, bnodes, levels) for Body lines blines.
    blines is either Vim buffer object (Body) or list of buffer lines.
    """
    Z = len(blines)
    tlines, bnodes, levels = [], [], []
    tlines_add, bnodes_add, levels_add = tlines.append, bnodes.append, levels.append
    for i in xrange(Z):
        if not blines[i].startswith('='):
            continue
        bline = blines[i]
        if '<!--' in bline:
            bline = comment_tag_sub('',bline)
        bline = bline.strip()
        m = headline_match(bline)
        if not m:
            continue
        lev = len(m.group(1))
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
    hs = h # need to strip trailing comment tags first
    if '<!--' in h:
        hs = comment_tag_sub('',hs)
    m = headline_match(hs)
    level = len(m.group(1))
    s = '='*(level+levDelta)
    return '%s%s%s%s' %(s, h[m.end(1):m.start(2)], s, h[m.end(2):])

