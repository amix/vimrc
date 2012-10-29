# voom_mode_fmr2.py
# Last Modified: 2012-02-04
# VOoM -- Vim two-pane outliner, plugin for Python-enabled Vim version 7.x
# Website: http://www.vim.org/scripts/script.php?script_id=2657
# Author: Vlad Irnov (vlad DOT irnov AT gmail DOT com)
# License: This program is free software. It comes without any warranty,
#          to the extent permitted by applicable law. You can redistribute it
#          and/or modify it under the terms of the Do What The Fuck You Want To
#          Public License, Version 2, as published by Sam Hocevar.
#          See http://sam.zoy.org/wtfpl/COPYING for more details.

"""
VOoM markup mode. Headline text is after the start fold marker with level.
See |voom_mode_fmr|, ../../doc/voom.txt#*voom_mode_fmr*

{{{1 headline level 1
some text
{{{2 headline level 2
more text
"""

# Define this mode as an 'fmr' mode.
MODE_FMR = True


def hook_makeOutline(VO, blines):
    """Return (tlines, bnodes, levels) for Body lines blines.
    blines is either Vim buffer object (Body) or list of buffer lines.
    """
    marker = VO.marker
    marker_re_search = VO.marker_re.search
    Z = len(blines)
    tlines, bnodes, levels = [], [], []
    tlines_add, bnodes_add, levels_add = tlines.append, bnodes.append, levels.append
    #c = VO.rstrip_chars
    for i in xrange(Z):
        if not marker in blines[i]: continue
        bline = blines[i]
        m = marker_re_search(bline)
        if not m: continue
        lev = int(m.group(1))
        #head = bline[:m.start()].lstrip().rstrip(c).strip('-=~').strip()
        head = bline[m.end():]
        # strip special marks o=
        if head and head[0]=='o': head = head[1:]
        if head and head[0]=='=': head = head[1:]
        tline = ' %s%s|%s' %(m.group(2) or ' ', '. '*(lev-1), head.strip())
        tlines_add(tline)
        bnodes_add(i+1)
        levels_add(lev)
    return (tlines, bnodes, levels)


def hook_newHeadline(VO, level, blnum, ln):
    """Return (tree_head, bodyLines).
    tree_head is new headline string in Tree buffer (text after |).
    bodyLines is list of lines to insert in Body buffer.
    """
    tree_head = 'NewHeadline'
    bodyLines = ['%s%s %s' %(VO.marker, level, tree_head), '']
    return (tree_head, bodyLines)


