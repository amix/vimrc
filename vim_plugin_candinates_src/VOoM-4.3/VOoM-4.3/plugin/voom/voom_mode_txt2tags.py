# voom_mode_txt2tags.py
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
VOoM markup mode for txt2tags titles.
See |voom_mode_txt2tags|,  ../../doc/voom.txt#*voom_mode_txt2tags*
"""

import re
# from txt2tags.py
#   titskel = r'^ *(?P<id>%s)(?P<txt>%s)\1(\[(?P<label>[\w-]*)\])?\s*$'
#   bank[   'title'] = re.compile(titskel%('[=]{1,5}','[^=](|.*[^=])'))
#   bank['numtitle'] = re.compile(titskel%('[+]{1,5}','[^+](|.*[^+])'))
# === headline ===
headline1_match = re.compile(r'^ *(=+)([^=].*[^=]|[^=])(\1)(\[[\w-]*\])?\s*$').match
# +++ headline +++
headline2_match = re.compile(r'^ *(\++)([^+].*[^+]|[^+])(\1)(\[[\w-]*\])?\s*$').match


def hook_makeOutline(VO, blines):
    """Return (tlines, bnodes, levels) for Body lines blines.
    blines is either Vim buffer object (Body) or list of buffer lines.
    """
    Z = len(blines)
    tlines, bnodes, levels = [], [], []
    tlines_add, bnodes_add, levels_add = tlines.append, bnodes.append, levels.append

    areaVerb,areaRaw,areaTagged = False,False,False
    for i in xrange(Z):
        bline = blines[i]

        # ignore Verbatim/Raw/Tagged Areas
        bline_rs = bline.rstrip()
        if bline_rs=='```' and not (areaRaw or areaTagged):
            areaVerb = not areaVerb; continue
        elif bline_rs=='"""' and not (areaVerb or areaTagged):
            areaRaw = not areaRaw; continue
        elif bline_rs=="'''" and not (areaVerb or areaRaw):
            areaTagged = not areaTagged; continue
        if areaVerb or areaRaw or areaTagged: continue

        # there can be leading spaces but not tabs
        bline = bline.lstrip(' ')
        if bline.startswith('='):
            m = headline1_match(bline)
            if not m: continue
            plus = ''
        elif bline.startswith('+'):
            m = headline2_match(bline)
            if not m: continue
            plus = '+ '
        else:
            continue
        lev = len(m.group(1))
        head = '%s%s' %(plus, m.group(2).strip())
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
    # choose = or + headline type -- same as previous headline
    if tlnum > 1:
        prev_head = VO.Body[VO.bnodes[tlnum-1] - 1]
        if prev_head.lstrip()[0] == '=':
            lev = '='*level
        else:
            lev = '+'*level
            tree_head = '+ NewHeadline'
    else:
        lev = '='*level
    bodyLines = ['%s NewHeadline %s' %(lev, lev), '']
    return (tree_head, bodyLines)


def hook_changeLevBodyHead(VO, h, levDelta):
    """Increase of decrease level number of Body headline by levDelta."""
    if levDelta==0: return h
    hLS = h.lstrip()
    if hLS[0] == '=':
        m = headline1_match(h)
        level = len(m.group(1))
        s = '='*(level+levDelta)
    elif hLS[0] == '+':
        m = headline2_match(h)
        level = len(m.group(1))
        s = '+'*(level+levDelta)
    else: assert False
    return '%s%s%s%s%s' %(h[:m.start(1)], s, h[m.end(1):m.start(3)], s, h[m.end(3):])

