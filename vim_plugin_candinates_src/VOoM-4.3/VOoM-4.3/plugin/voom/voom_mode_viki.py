# voom_mode_viki.py
# Last Modified: 2011-10-28
# VOoM -- Vim two-pane outliner, plugin for Python-enabled Vim version 7.x
# Website: http://www.vim.org/scripts/script.php?script_id=2657
# Author: Vlad Irnov (vlad DOT irnov AT gmail DOT com)
# License: This program is free software. It comes without any warranty,
#          to the extent permitted by applicable law. You can redistribute it
#          and/or modify it under the terms of the Do What The Fuck You Want To
#          Public License, Version 2, as published by Sam Hocevar.
#          See http://sam.zoy.org/wtfpl/COPYING for more details.

"""
VOoM markup mode for headline markup used by Vim Viki/Deplate plugin.
See |voom_mode_viki|,  ../../doc/voom.txt#*voom_mode_viki*
"""

import re
headline_match = re.compile(r'^(\*+)\s').match

# Ignore Regions other than #Region
#
#    #Type [OPTIONS] <<EndOfRegion
#    .......
#    EndOfRegion
#
# syntax/viki.vim:
#   syn region vikiRegion matchgroup=vikiMacroDelim
#               \ start=/^[[:blank:]]*#\([A-Z]\([a-z][A-Za-z]*\)\?\>\|!!!\)\(\\\n\|.\)\{-}<<\z(.*\)$/
#               \ end=/^[[:blank:]]*\z1[[:blank:]]*$/
#               \ contains=@vikiText,vikiRegionNames
#
# EndOfRegion can be empty string, leading/trailing white space matters
# Don't know what !!! is for.
#
region_match = re.compile(r'^\s*#([A-Z]([a-z][A-Za-z]*)?)\b.*?<<(.*)').match


def hook_makeOutline(VO, blines):
    """Return (tlines, bnodes, levels) for Body lines blines.
    blines is either Vim buffer object (Body) or list of buffer lines.
    """
    Z = len(blines)
    tlines, bnodes, levels = [], [], []
    tlines_add, bnodes_add, levels_add = tlines.append, bnodes.append, levels.append
    inRegion = False #  EndOfRegion match object when inside a region
    for i in xrange(Z):
        bline = blines[i]

        if inRegion:
            if re.match(inRegion, bline):
                inRegion = False
            continue

        if bline.lstrip().startswith('#') and '<<' in bline:
            r_m = region_match(bline)
            if r_m and r_m.group(1) != 'Region':
                inRegion = '^\s*%s\s*$' %re.escape(r_m.group(3) or '')
                continue
        elif not bline.startswith('*'):
            continue

        m = headline_match(bline)
        if not m:
            continue
        lev = len(m.group(1))
        head = bline[lev:].strip()
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
    bodyLines = ['%s %s' %('*'*level, tree_head), '']
    return (tree_head, bodyLines)


def hook_changeLevBodyHead(VO, h, levDelta):
    """Increase of decrease level number of Body headline by levDelta."""
    if levDelta==0: return h
    m = headline_match(h)
    level = len(m.group(1))
    return '%s%s' %('*'*(level+levDelta), h[m.end(1):])


