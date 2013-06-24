# voom_mode_python.py
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
VOoM markup mode for Python code.
See |voom_mode_python|,  ../../doc/voom.txt#*voom_mode_python*
"""

import token, tokenize
import traceback
import vim


def hook_makeOutline(VO, blines):
    """Return (tlines, bnodes, levels) for Body lines blines.
    blines is either Vim buffer object (Body) or list of buffer lines.
    """
    Z = len(blines)
    tlines, bnodes, levels = [], [], []
    tlines_add, bnodes_add, levels_add = tlines.append, bnodes.append, levels.append

    #ignore_lnums, func_lnums = get_lnums_from_tokenize(blines)
    try:
        ignore_lnums, func_lnums = get_lnums_from_tokenize(blines)
    except (IndentationError, tokenize.TokenError):
        vim.command("call Voom_ErrorMsg('VOoM: EXCEPTION WHILE PARSING PYTHON OUTLINE')")
        # DO NOT print to sys.stderr -- triggers Vim error when default stderr (no PyLog)
        #traceback.print_exc()  --this goes to sys.stderr
        #print traceback.format_exc() --ok but no highlighting
        lines = traceback.format_exc().replace("'","''").split('\n')
        for l in lines:
            vim.command("call Voom_ErrorMsg('%s')" %l)
        return (['= |!!!ERROR: OUTLINE IS INVALID'], [1], [1])

    gotHead = False # True if current line is a headline
    indents = [0,] # indents of previous levels
    funcLevels = [] # levels of previous def or class
    indent_error = '' # inconsistent indent
    for i in xrange(Z):
        if i+1 in ignore_lnums: continue
        bline = blines[i]
        bline_s = bline.strip()
        if not bline_s: continue
        bline_ls = bline.lstrip()

        # compute indent and level
        indent = len(bline) - len(bline_ls)
        if indent > indents[-1]:
            indents.append(indent)
        elif indent < indents[-1]:
            while indents and (indents[-1] > indent):
                indents.pop()
            if indents[-1]==indent:
                indent_error = ''
            else:
                indent_error = '!!! '
        lev = len(indents)

        # first line after the end of a class or def block
        if funcLevels and lev <= funcLevels[-1]:
            gotHead = True
            while funcLevels and funcLevels[-1] >= lev:
                funcLevels.pop()
        # first line of a class or def block
        if i+1 in func_lnums:
            gotHead = True
            if not funcLevels or (lev > funcLevels[-1]):
                funcLevels.append(lev)
        # special comment line (unconditional headline) or line with @decorator
        elif bline_s.startswith('@') or bline_s.startswith('### ') or bline_s.startswith('#---'):
            gotHead = True

        if gotHead:
            gotHead = False
            tline = '  %s|%s%s' %('. '*(lev-1), indent_error, bline_s)
            tlines_add(tline)
            bnodes_add(i+1)
            levels_add(lev)

    return (tlines, bnodes, levels)


class BLines:
    """Wrapper around Vim buffer object or list of Body lines to provide
    readline() method for use with tokenize.generate_tokens().
    """
    def __init__(self, blines):
        self.blines = blines
        self.size = len(blines)
        self.idx = -1

    def readline(self):
        self.idx += 1
        if self.idx == self.size:
            return ''
        return "%s\n" %self.blines[self.idx]


### toktypes of tokens
STRING = token.STRING
NAME = token.NAME
NEWLINE = token.NEWLINE

def get_lnums_from_tokenize(blines):
    """Return dicts. Keys are Body lnums.
    The main purpose is to get list of lnums to ignore: multi-line strings and
    expressions.
    """
    # lnums to ignore: multi-line strings and expressions other than the first line
    ignore_lnums = {}
    # lnums of 'class' and 'def' tokens
    func_lnums = {}

    inName = False

    for tok in tokenize.generate_tokens(BLines(blines).readline):
        toktype, toktext, (srow, scol), (erow, ecol), line = tok
        #print token.tok_name[toktype], tok
        if toktype == NAME:
            if not inName:
                inName = True
                srow_name = srow
            if toktext in ('def','class'):
                func_lnums[srow] = toktext
        elif toktype == NEWLINE and inName:
            inName = False
            if srow_name != erow:
                for i in xrange(srow_name+1, erow+1):
                    ignore_lnums[i] = 0
        elif toktype == STRING:
            if srow != erow:
                for i in xrange(srow+1, erow+1):
                    ignore_lnums[i] = 0

    return (ignore_lnums, func_lnums)


def get_body_indent(body):
    """Return string used for indenting Body lines."""
    et = int(vim.eval("getbufvar(%s,'&et')" %body))
    if et:
        ts = int(vim.eval("getbufvar(%s,'&ts')" %body))
        return ' '*ts
    else:
        return '\t'


def hook_newHeadline(VO, level, blnum, tlnum):
    """Return (tree_head, bodyLines).
    tree_head is new headline string in Tree buffer (text after |).
    bodyLines is list of lines to insert in Body buffer.
    """
    tree_head = '### NewHeadline'
    indent = get_body_indent(VO.body)
    body_head = '%s%s' %(indent*(level-1), tree_head)
    return (tree_head, [body_head])


#def hook_changeLevBodyHead(VO, h, levDelta):
    #"""Increase of decrease level number of Body headline by levDelta."""
    #if levDelta==0: return h


def hook_doBodyAfterOop(VO, oop, levDelta, blnum1, tlnum1, blnum2, tlnum2, blnumCut, tlnumCut):
    # this is instead of hook_changeLevBodyHead()
    #print oop, levDelta, blnum1, tlnum1, blnum2, tlnum2, tlnumCut, blnumCut
    Body = VO.Body
    Z = len(Body)

    ind = get_body_indent(VO.body)
    # levDelta is wrong when pasting because hook_makeOutline() looks at relative indent
    # determine level of pasted region from indent of its first line
    if oop=='paste':
        bline1 = Body[blnum1-1]
        lev = (len(bline1) - len(bline1.lstrip())) / len(ind) + 1
        levDelta = VO.levels[tlnum1-1] - lev

    if not levDelta: return

    indent = abs(levDelta) * ind
    #--- copied from voom_mode_thevimoutliner.py -----------------------------
    if blnum1:
        assert blnum1 == VO.bnodes[tlnum1-1]
        if tlnum2 < len(VO.bnodes):
            assert blnum2 == VO.bnodes[tlnum2]-1
        else:
            assert blnum2 == Z

    # dedent (if possible) or indent every non-blank line in Body region blnum1,blnum2
    blines = []
    for i in xrange(blnum1-1,blnum2):
        line = Body[i]
        if not line.strip():
            blines.append(line)
            continue
        if levDelta > 0:
            line = '%s%s' %(indent,line)
        elif levDelta < 0 and line.startswith(indent):
            line = line[len(indent):]
        blines.append(line)

    # replace Body region
    Body[blnum1-1:blnum2] = blines
    assert len(Body)==Z


