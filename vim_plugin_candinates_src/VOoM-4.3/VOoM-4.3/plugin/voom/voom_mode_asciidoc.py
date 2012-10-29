# voom_mode_asciidoc.py
# Last Modified: 2012-04-02
# VOoM -- Vim two-pane outliner, plugin for Python-enabled Vim version 7.x
# Website: http://www.vim.org/scripts/script.php?script_id=2657
# Author: Vlad Irnov (vlad DOT irnov AT gmail DOT com)
# License: This program is free software. It comes without any warranty,
#          to the extent permitted by applicable law. You can redistribute it
#          and/or modify it under the terms of the Do What The Fuck You Want To
#          Public License, Version 2, as published by Sam Hocevar.
#          See http://sam.zoy.org/wtfpl/COPYING for more details.

"""
VOoM markup mode for AsciiDoc document and section titles.
See |voom_mode_asciidoc|,   ../../doc/voom.txt#*voom_mode_asciidoc*
"""

### NOTES
#
# When outline operation changes level, it has to deal with two ambiguities:
#   a) Level 1-5 headline can use 2-style (underline) or 1-style (=).
#   b) 1-style can have or not have closing ='s.
# To determine current preferences: check first headline at level <6 and check
# first headline with =. This must be done in hook_makeOutline().
# (Save in VO, similar to reST mode.) Cannot be done during outline operation,
# that is in hook_doBodyAfterOop().
# Defaults: use underline, use closing ='s.

try:
    import vim
    if vim.eval('exists("g:voom_asciidoc_do_blanks")')=='1' and vim.eval("g:voom_asciidoc_do_blanks")=='0':
        DO_BLANKS = False
    else:
        DO_BLANKS = True
except ImportError:
    DO_BLANKS = True

import re
# regex for 1-style headline, assumes there is no trailing whitespace
HEAD_MATCH = re.compile(r'^(=+)(\s+\S.*?)(\s+\1)?$').match

# underline chars
ADS_LEVELS = {'=':1, '-':2, '~':3, '^':4, '+':5}
LEVELS_ADS = {1:'=', 2:'-', 3:'~', 4:'^', 5:'+'}

# DelimitedBlock chars, headines are ignored inside such blocks
BLOCK_CHARS = {'/':0, '+':0, '-':0, '.':0, '*':0, '_':0, '=':0}

# Combine all signficant chars. Need one of these at start of line for a
# headline or DelimitedBlock to occur.
CHARS = {}
for k in ADS_LEVELS:
    CHARS[k] = 0
for k in BLOCK_CHARS:
    CHARS[k] = 0


def hook_makeOutline(VO, blines):
    """Return (tlines, bnodes, levels) for Body lines blines.
    blines is either Vim buffer object (Body) or list of buffer lines.
    """
    ENC = VO.enc
    Z = len(blines)
    tlines, bnodes, levels = [], [], []
    tlines_add, bnodes_add, levels_add = tlines.append, bnodes.append, levels.append

    # trailing whitespace is always removed with rstrip()
    # if headline is precedeed by [AAA] and/or [[AAA]], bnode is set to their lnum
    #
    # 1-style, overides 2-style
    #  [[AAA]]       L3, blines[i-2]
    #  [yyy]         L2, blines[i-1]
    #  == head ==    L1, blines[i]   -- current line, closing = are optional
    #
    # 2-style (underline)
    #  [[AAA]]       L4, blines[i-3]
    #  [yyy]         L3, blines[i-2]
    #  head          L2, blines[i-1] -- title line, many restrictions on the format
    #  ----          L1, blines[i]   -- current line


    # Set this the first time a headline with level 1-5 is encountered.
    # 0 or 1 -- False, use 2-style (default); 2 -- True, use 1-style
    useOne = 0
    # Set this the first time headline in 1-style is encountered.
    # 0 or 1 -- True, use closing ='s (default); 2 -- False, do not use closing ='s
    useOneClose = 0

    gotHead = False
    inBlock = False # True if inside DelimitedBlock, the value is the char
    headI = -2 # idx of the last line that is part of a headline
    blockI = -2 # idx of the last line where a DelimitedBlock ended
    m = None # match object for 1-style regex

    for i in xrange(Z):
        L1 = blines[i].rstrip()
        if not L1 or not L1[0] in CHARS:
            continue
        ch = L1[0]

        if inBlock:
            if inBlock==ch and len(L1)>3 and L1.lstrip(ch)=='':
                inBlock = False
                blockI = i
            continue

        # 1-style headline
        if ch == '=' and L1.strip('='):
            m = HEAD_MATCH(L1)
            if m:
                gotHead = True
                headI_ = headI
                headI = i
                lev = len(m.group(1))
                head = m.group(2).strip()
                bnode = i+1

        # current line is an underline
        # the previous, underlined line (L2) is not a headline if it:
        #   is not exactly the length of underline +/- 2
        #   is already part of in the previous headline
        #   looks like an underline or a delimited block line
        #   is [[AAA]] or [AAA] (BlockID or Attribute List)
        #   starts with . (Block Title, they have no level)
        #   starts with // (comment line)
        #   starts with tab (don't know why, spaces are ok)
        #   is only 1 chars (avoids confusion with --, as in Vim syntax, not as in AsciiDoc)
        if not gotHead and ch in ADS_LEVELS and L1.lstrip(ch)=='' and i > 0:
            L2 = blines[i-1].rstrip()
            z2 = len(L2.decode(ENC,'replace'))
            z1 = len(L1)
            if (L2 and
                  (-3 < z2 - z1 < 3) and z1 > 1 and z2 > 1 and
                  headI != i-1 and
                  not ((L2[0] in CHARS) and L2.lstrip(L2[0])=='') and
                  not (L2.startswith('[') and L2.endswith(']')) and
                  not L2.startswith('.') and
                  not L2.startswith('\t') and
                  not (L2.startswith('//') and not L2.startswith('///'))
                  ):
                gotHead = True
                headI_ = headI
                headI = i
                lev = ADS_LEVELS[ch]
                head = L2.strip()
                bnode = i # lnum of previous line (L2)

        if gotHead and bnode > 1:
            # decrement bnode if preceding lines are [[AAA]] or [AAA] lines
            # that is set bnode to the topmost [[AAA]] or [AAA] line number
            j_ = bnode-2 # idx of line before the title line
            L3 = blines[bnode-2].rstrip()
            while L3.startswith('[') and L3.endswith(']'):
                bnode -= 1
                if bnode > 1:
                    L3 = blines[bnode-2].rstrip()
                else:
                    break

            # headline must be preceded by a blank line unless:
            #   it's line 1 (j == -1)
            #   headline is preceded by [AAA] or [[AAA]] lines (j != j_)
            #   previous line is a headline (headI_ == j)
            #   previous line is the end of a DelimitedBlock (blockI == j)
            j = bnode-2
            if DO_BLANKS and j==j_ and j > -1:
                L3 = blines[j].rstrip()
                if L3 and headI_ != j and blockI != j:
                    # skip over any adjacent comment lines
                    while L3.startswith('//') and not L3.startswith('///'):
                        j -= 1
                        if j > -1:
                            L3 = blines[j].rstrip()
                        else:
                            L3 = ''
                    if L3 and headI_ != j and blockI != j:
                        gotHead = False
                        headI = headI_

        # start of DelimitedBlock
        if not gotHead and ch in BLOCK_CHARS and len(L1)>3 and L1.lstrip(ch)=='':
            inBlock = ch
            continue

        if gotHead:
            gotHead = False
            # save style info for first headline and first 1-style headline
            if not useOne and lev < 6:
                if m:
                    useOne = 2
                else:
                    useOne = 1
            if not useOneClose and m:
                if m.group(3):
                    useOneClose = 1
                else:
                    useOneClose = 2
            # make outline
            tline = '  %s|%s' %('. '*(lev-1), head)
            tlines_add(tline)
            bnodes_add(bnode)
            levels_add(lev)

    # don't clobber these when parsing clipboard during Paste
    # which is the only time blines is not Body
    if blines is VO.Body:
        VO.useOne = useOne == 2
        VO.useOneClose = useOneClose < 2

    return (tlines, bnodes, levels)


def hook_newHeadline(VO, level, blnum, tlnum):
    """Return (tree_head, bodyLines).
    tree_head is new headline string in Tree buffer (text after |).
    bodyLines is list of lines to insert in Body buffer.
    """
    tree_head = 'NewHeadline'
    if level < 6 and not VO.useOne:
        bodyLines = [tree_head, LEVELS_ADS[level]*11, '']
    else:
        lev = '='*level
        if VO.useOneClose:
            bodyLines = ['%s %s %s' %(lev, tree_head, lev), '']
        else:
            bodyLines = ['%s %s' %(lev, tree_head), '']

    # Add blank line when inserting after non-blank Body line.
    if VO.Body[blnum-1].strip():
        bodyLines[0:0] = ['']

    return (tree_head, bodyLines)


#def hook_changeLevBodyHead(VO, h, levDelta):
#    DO NOT CREATE THIS HOOK


def hook_doBodyAfterOop(VO, oop, levDelta, blnum1, tlnum1, blnum2, tlnum2, blnumCut, tlnumCut):
    # this is instead of hook_changeLevBodyHead()

    # Based on Markdown mode function.
    # Inserts blank separator lines if missing.

    #print oop, levDelta, blnum1, tlnum1, blnum2, tlnum2, tlnumCut, blnumCut
    Body = VO.Body
    Z = len(Body)
    bnodes, levels = VO.bnodes, VO.levels
    ENC = VO.enc

    # blnum1 blnum2 is first and last lnums of Body region pasted, inserted
    # during up/down, or promoted/demoted.
    if blnum1:
        assert blnum1 == bnodes[tlnum1-1]
        if tlnum2 < len(bnodes):
            assert blnum2 == bnodes[tlnum2]-1
        else:
            assert blnum2 == Z

    # blnumCut is Body lnum after which a region was removed during 'cut',
    # 'up', 'down'. Need this to check if there is blank line between nodes
    # used to be separated by the cut/moved region.
    if blnumCut:
        if tlnumCut < len(bnodes):
            assert blnumCut == bnodes[tlnumCut]-1
        else:
            assert blnumCut == Z

    # Total number of added lines minus number of deleted lines.
    b_delta = 0

    ### After 'cut' or 'up': insert blank line if there is none
    # between the nodes used to be separated by the cut/moved region.
    if DO_BLANKS and (oop=='cut' or oop=='up') and (0 < blnumCut < Z) and Body[blnumCut-1].strip():
        Body[blnumCut:blnumCut] = ['']
        update_bnodes(VO, tlnumCut+1 ,1)
        b_delta+=1

    if oop=='cut':
        return

    ### Make sure there is blank line after the last node in the region:
    # insert blank line after blnum2 if blnum2 is not blank, that is insert
    # blank line before bnode at tlnum2+1.
    if DO_BLANKS and blnum2 < Z and Body[blnum2-1].strip():
        Body[blnum2:blnum2] = ['']
        update_bnodes(VO, tlnum2+1 ,1)
        b_delta+=1

    ### Change levels and/or formats of headlines in the affected region.
    # Always do this after Paste, even if level is unchanged -- format can
    # be different when pasting from other outlines.
    # Examine each headline, from bottom to top, and change level and/or format.
    # To change from 1-style to 2-style:
    #   strip ='s, strip whitespace;
    #   insert underline.
    # To change from 2-style to 1-style:
    #   delete underline;
    #   insert ='s.
    # Update bnodes after inserting or deleting a line.
    #
    # NOTE: bnode can be [[AAA]] or [AAA] line, we check for that and adjust it
    # to point to the headline text line
    #
    #   1-style          2-style
    #
    #            L0            L0             Body[bln-2]
    #   == head  L1      head  L1   <--bnode  Body[bln-1] (not always the actual bnode)
    #            L2      ----  L2             Body[bln]
    #            L3            L3             Body[bln+1]

    if levDelta or oop=='paste':
        for i in xrange(tlnum2, tlnum1-1, -1):
            # required level (VO.levels has been updated)
            lev = levels[i-1]
            # current level from which to change to lev
            lev_ = lev - levDelta

            # Body headline (bnode) and the next line
            bln = bnodes[i-1]
            L1 = Body[bln-1].rstrip()
            # bnode can point to the tompost [AAA] or [[AAA]] line
            # increment bln until the actual headline (title line) is found
            while L1.startswith('[') and L1.endswith(']'):
                bln += 1
                L1 = Body[bln-1].rstrip()
            # the underline line
            if bln+1 < len(Body):
                L2 = Body[bln].rstrip()
            else:
                L2 = ''

            # get current headline format
            hasOne, hasOneClose = False, VO.useOneClose
            theHead = L1
            if L1.startswith('='):
                m = HEAD_MATCH(L1)
                if m:
                    hasOne = True
                    # headline without ='s but with whitespace around it preserved
                    theHead = m.group(2)
                    theclose = m.group(3)
                    if theclose:
                        hasOneClose = True
                        theHead += theclose.rstrip('=')
                    else:
                        hasOneClose = False

            # get desired headline format
            if oop=='paste':
                if lev > 5:
                    useOne = True
                else:
                    useOne = VO.useOne
                useOneClose = VO.useOneClose
            elif lev < 6 and lev_ < 6:
                useOne = hasOne
                useOneClose = hasOneClose
            elif lev > 5 and lev_ > 5:
                useOne = True
                useOneClose = hasOneClose
            elif lev < 6 and lev_ > 5:
                useOne = VO.useOne
                useOneClose = VO.useOneClose
            elif lev > 5 and lev_ < 6:
                useOne = True
                useOneClose = hasOneClose
            else:
                assert False
            #print useOne, hasOne, ';', useOneClose, hasOneClose

            ### change headline level and/or format
            # 2-style unchanged, only adjust level of underline
            if not useOne and not hasOne:
                if not levDelta: continue
                Body[bln] = LEVELS_ADS[lev]*len(L2)
            # 1-style unchanged, adjust level of ='s and add/remove closing ='s
            elif useOne and hasOne:
                # no format change, there are closing ='s
                if useOneClose and hasOneClose:
                    if not levDelta: continue
                    Body[bln-1] = '%s%s%s' %('='*lev, theHead, '='*lev)
                # no format change, there are no closing ='s
                elif not useOneClose and not hasOneClose:
                    if not levDelta: continue
                    Body[bln-1] = '%s%s' %('='*lev, theHead)
                # add closing ='s
                elif useOneClose and not hasOneClose:
                    Body[bln-1] = '%s%s %s' %('='*lev, theHead.rstrip(), '='*lev)
                # remove closing ='s
                elif not useOneClose and hasOneClose:
                    Body[bln-1] = '%s%s' %('='*lev, theHead.rstrip())
            # insert underline, remove ='s
            elif not useOne and hasOne:
                L1 = theHead.strip()
                Body[bln-1] = L1
                # insert underline
                Body[bln:bln] = [LEVELS_ADS[lev]*len(L1.decode(ENC,'replace'))]
                update_bnodes(VO, i+1, 1)
                b_delta+=1
            # remove underline, insert ='s
            elif useOne and not hasOne:
                if useOneClose:
                    Body[bln-1] = '%s %s %s' %('='*lev, theHead.strip(), '='*lev)
                else:
                    Body[bln-1] = '%s %s' %('='*lev, theHead.strip())
                # delete underline
                Body[bln:bln+1] = []
                update_bnodes(VO, i+1, -1)
                b_delta-=1

    ### Make sure first headline is preceded by a blank line.
    blnum1 = bnodes[tlnum1-1]
    if DO_BLANKS and blnum1 > 1 and Body[blnum1-2].strip():
        Body[blnum1-1:blnum1-1] = ['']
        update_bnodes(VO, tlnum1 ,1)
        b_delta+=1

    ### After 'down' : insert blank line if there is none
    # between the nodes used to be separated by the moved region.
    if DO_BLANKS and oop=='down' and (0 < blnumCut < Z) and Body[blnumCut-1].strip():
        Body[blnumCut:blnumCut] = ['']
        update_bnodes(VO, tlnumCut+1 ,1)
        b_delta+=1

    assert len(Body) == Z + b_delta


def update_bnodes(VO, tlnum, delta):
    """Update VO.bnodes by adding/substracting delta to each bnode
    starting with bnode at tlnum and to the end.
    """
    bnodes = VO.bnodes
    for i in xrange(tlnum, len(bnodes)+1):
        bnodes[i-1] += delta


