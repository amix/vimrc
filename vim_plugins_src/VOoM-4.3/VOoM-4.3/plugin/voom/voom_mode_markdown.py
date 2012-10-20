# voom_mode_markdown.py
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
VOoM markup mode for Markdown headers.
See |voom_mode_markdown|,   ../../doc/voom.txt#*voom_mode_markdown*
"""

### NOTES
# When outline operation changes level, it has to deal with two ambiguities:
#   a) Level 1 and 2 headline can use underline-style or hashes-style.
#   b) Hashes-style can have or not have closing hashes.
# To determine current preferences: check first headline at level <3 and check
# first headline with hashes. This must be done in hook_makeOutline().
# (Save in VO, similar to reST mode.) Cannot be done during outline operation,
# that is in hook_doBodyAfterOop().
# Defaults: use underline, use closing hashes.


levels_ads = {1:'=', 2:'-'}


def hook_makeOutline(VO, blines):
    """Return (tlines, bnodes, levels) for Body lines blines.
    blines is either Vim buffer object (Body) or list of buffer lines.
    """
    Z = len(blines)
    tlines, bnodes, levels = [], [], []
    tlines_add, bnodes_add, levels_add = tlines.append, bnodes.append, levels.append

    # trailing whitespace is always removed with rstrip()
    #
    # hashes-style, overides underline-style
    #  abcde   L2, blines[i-1]
    #  ## head L1, blines[i]   -- current line
    #
    # underline-style
    #   head   L2, blines[i-1] -- title line, not blank, does not start with #
    #  ------  L1, blines[i]   -- current line, any number of = or - only

    L1, L2 = '',''

    # Set this once when headline with level 1 or 2 is encountered.
    # 0 or 1 -- False, use underline-style (default); 2 -- True, use hashes-style
    useHash = 0
    # Set this once when headline with hashes is encountered.
    # 0 or 1 -- True, use closing hashes (default); 2 -- False, do not use closing hashes
    useCloseHash = 0

    gotHead = False
    for i in xrange(Z):
        L2 = L1
        L1 = blines[i].rstrip()

        if L1.startswith('#'):
            gotHead = True
            lev = len(L1) - len(L1.lstrip('#'))
            bnode = i+1
            head = L1.strip('#').strip()
        elif L2 and L1.startswith('=') and L1.lstrip('=')=='':
            gotHead = True
            lev = 1
            head = L2.strip()
            bnode = i
        elif L2 and L1.startswith('-') and L1.lstrip('-')=='':
            gotHead = True
            lev = 2
            head = L2.strip()
            bnode = i
        else:
            continue

        if gotHead:
            gotHead = False
            if not useHash and lev < 3:
                if L1.startswith('#'):
                    useHash = 2
                else:
                    useHash = 1
            if not useCloseHash and L1.startswith('#'):
                if L1.endswith('#'):
                    useCloseHash = 1
                else:
                    useCloseHash = 2
            L1, L2 = '',''

            tline = '  %s|%s' %('. '*(lev-1), head)
            tlines_add(tline)
            bnodes_add(bnode)
            levels_add(lev)

    # don't clobber these when parsing clipboard during Paste
    # which is the only time blines is not Body
    if blines is VO.Body:
        VO.useHash = useHash == 2
        VO.useCloseHash = useCloseHash < 2

    return (tlines, bnodes, levels)


def hook_newHeadline(VO, level, blnum, tlnum):
    """Return (tree_head, bodyLines).
    tree_head is new headline string in Tree buffer (text after |).
    bodyLines is list of lines to insert in Body buffer.
    """
    tree_head = 'NewHeadline'
    if level < 3 and not VO.useHash:
        bodyLines = [tree_head, levels_ads[level]*11, '']
    else:
        lev = '#'*level
        if VO.useCloseHash:
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

    # Based on reST mode function. Insert blank separator lines if missing,
    # even though they are not important for Markdown headlines.

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
    if (oop=='cut' or oop=='up') and (0 < blnumCut < Z) and Body[blnumCut-1].strip():
        Body[blnumCut:blnumCut] = ['']
        update_bnodes(VO, tlnumCut+1 ,1)
        b_delta+=1

    if oop=='cut':
        return

    ### Make sure there is blank line after the last node in the region:
    # insert blank line after blnum2 if blnum2 is not blank, that is insert
    # blank line before bnode at tlnum2+1.
    if blnum2 < Z and Body[blnum2-1].strip():
        Body[blnum2:blnum2] = ['']
        update_bnodes(VO, tlnum2+1 ,1)
        b_delta+=1

    ### Change levels and/or formats of headlines in the affected region.
    # Always do this after Paste, even if level is unchanged -- format can
    # be different when pasting from other outlines.
    # Examine each headline, from bottom to top, and change level and/or format.
    # To change from hashes to underline-style:
    #   strip hashes, strip whitespace;
    #   insert underline.
    # To change from underline to hashes-style:
    #   delete underline;
    #   insert hashes.
    # Update bnodes after inserting or deleting a line.

    #   hash-style       underline-style
    #
    #            L0            L0             Body[bln-2]
    #   ## head  L1      head  L1   <--bnode  Body[bln-1]
    #            L2      ----  L2             Body[bln]
    #            L3            L3             Body[bln+1]

    if levDelta or oop=='paste':
        for i in xrange(tlnum2, tlnum1-1, -1):
            # required level (VO.levels has been updated)
            lev = levels[i-1]
            # current level from which to change to lev
            lev_ = lev - levDelta

            # Body headline (bnode) and next line
            bln = bnodes[i-1]
            L1 = Body[bln-1].rstrip()
            if bln+1 < len(Body):
                L2 = Body[bln].rstrip()
            else:
                L2 = ''

            # get current headline format
            hasHash, hasCloseHash = False, VO.useCloseHash
            if L1.startswith('#'):
                hasHash = True
                if L1.endswith('#'):
                    hasCloseHash = True
                else:
                    hasCloseHash = False

            # get desired headline format
            if oop=='paste':
                if lev > 2:
                    useHash = True
                else:
                    useHash = VO.useHash
                useCloseHash = VO.useCloseHash
            elif lev < 3 and lev_ < 3:
                useHash = hasHash
                useCloseHash = hasCloseHash
            elif lev > 2 and lev_ > 2:
                useHash = True
                useCloseHash = hasCloseHash
            elif lev < 3 and lev_ > 2:
                useHash = VO.useHash
                useCloseHash = VO.useCloseHash
            elif lev > 2 and lev_ < 3:
                useHash = True
                useCloseHash = hasCloseHash
            else:
                assert False
            #print useHash, hasHash, ';', useCloseHash, hasCloseHash

            # change headline level and/or format

            # underline-style unchanged, only adjust level of underline
            if not useHash and not hasHash:
                if not levDelta: continue
                Body[bln] = levels_ads[lev]*len(L2)
            # hashes-style unchanged, adjust level of hashes and add/remove closing hashes
            elif useHash and hasHash:
                # no format change, there are closing hashes
                if useCloseHash and hasCloseHash:
                    if not levDelta: continue
                    Body[bln-1] = '%s%s%s' %('#'*lev, L1.strip('#'), '#'*lev)
                # no format change, there are no closing hashes
                elif not useCloseHash and not hasCloseHash:
                    if not levDelta: continue
                    Body[bln-1] = '%s%s' %('#'*lev, L1.lstrip('#'))
                # add closing hashes
                elif useCloseHash and not hasCloseHash:
                    Body[bln-1] = '%s%s %s' %('#'*lev, L1.strip('#').rstrip(), '#'*lev)
                # remove closing hashes
                elif not useCloseHash and hasCloseHash:
                    Body[bln-1] = '%s%s' %('#'*lev, L1.strip('#').rstrip())
            # insert underline, remove hashes
            elif not useHash and hasHash:
                L1 = L1.strip('#').strip()
                Body[bln-1] = L1
                # insert underline
                Body[bln:bln] = [levels_ads[lev]*len(L1.decode(ENC,'replace'))]
                update_bnodes(VO, i+1, 1)
                b_delta+=1
            # remove underline, insert hashes
            elif useHash and not hasHash:
                if useCloseHash:
                    Body[bln-1] = '%s %s %s' %('#'*lev, L1.strip('#').strip(), '#'*lev)
                else:
                    Body[bln-1] = '%s %s' %('#'*lev, L1.strip('#').strip())
                # delete underline
                Body[bln:bln+1] = []
                update_bnodes(VO, i+1, -1)
                b_delta-=1

    ### Make sure first headline is preceded by a blank line.
    blnum1 = bnodes[tlnum1-1]
    if blnum1 > 1 and Body[blnum1-2].strip():
        Body[blnum1-1:blnum1-1] = ['']
        update_bnodes(VO, tlnum1 ,1)
        b_delta+=1

    ### After 'down' : insert blank line if there is none
    # between the nodes used to be separated by the moved region.
    if oop=='down' and (0 < blnumCut < Z) and Body[blnumCut-1].strip():
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


