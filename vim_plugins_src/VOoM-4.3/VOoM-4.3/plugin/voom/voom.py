# voom.py
# Last Modified: 2012-04-16
# VOoM -- Vim two-pane outliner, plugin for Python-enabled Vim version 7.x
# Version: 4.3
# Website: http://www.vim.org/scripts/script.php?script_id=2657
# Author: Vlad Irnov (vlad DOT irnov AT gmail DOT com)
# License: This program is free software. It comes without any warranty,
#          to the extent permitted by applicable law. You can redistribute it
#          and/or modify it under the terms of the Do What The Fuck You Want To
#          Public License, Version 2, as published by Sam Hocevar.
#          See http://sam.zoy.org/wtfpl/COPYING for more details.

"""This module is meant to be imported by voom.vim ."""

import vim
import sys, os, re
import traceback
import bisect
# lazy imports
shuffle = None # random.shuffle

#Vim = sys.modules['__main__']

# see voom.vim for conventions
# voom_WhatEver() is Python code for Voom_WhatEver() function in voom.vim


#---Constants and Settings--------------------{{{1=

# VO is instance of VoomOutline class, stored in dict VOOMS
# create VOOMS in voom.vim: less disruption if this module is reloaded
#VOOMS = {} # {body: VO, ...}

# {filetype: make_head_<filetype> function, ...}
MAKE_HEAD = {}

# default start fold marker string and regexp
MARKER = '{{{'                            #}}}
MARKER_RE = re.compile(r'{{{(\d+)(x?)')   #}}}

# {'markdown': 'markdown', 'pandoc': 'markdown', ...}
if vim.eval("exists('g:voom_ft_modes')")=='1':
    FT_MODES = vim.eval('g:voom_ft_modes')
else:
    FT_MODES = {}
# default markup mode
if vim.eval("exists('g:voom_default_mode')")=='1':
    MODE = vim.eval('g:voom_default_mode')
else:
    MODE = ''


#---Outline Construction----------------------{{{1o


class VoomOutline: #{{{2
    """Outline data for one Body buffer.
    Instantiated from Body by Voom_Init().
    """
    def __init__(self,body):
        assert body == int(vim.eval("bufnr('')"))


def voom_Init(body): #{{{2
    VO = VoomOutline(body)
    VO.bnodes = [] # Body lnums of headlines
    VO.levels = [] # headline levels
    VO.body = body
    VO.Body = vim.current.buffer
    VO.tree = None # will set later
    VO.Tree = None # will set later
    VO.snLn = 1 # will change later if different
    # first Tree line is Body buffer name and path
    VO.bname = vim.eval('l:firstLine')
    # Body &filetype
    VO.filetype = vim.eval('&filetype')
    VO.enc = get_vim_encoding()

    # start fold marker string and regexp (default and 'fmr' modes)
    marker = vim.eval('&foldmarker').split(',')[0]
    VO.marker = marker
    if marker==MARKER:
        VO.marker_re = MARKER_RE
    else:
        VO.marker_re = re.compile(re.escape(marker) + r'(\d+)(x?)')

    # chars to strip from right side of Tree headlines (default and 'fmr' modes)
    if vim.eval("exists('g:voom_rstrip_chars_{&ft}')")=="1":
        VO.rstrip_chars = vim.eval("g:voom_rstrip_chars_{&ft}")
    else:
        VO.rstrip_chars = vim.eval("&commentstring").split('%s')[0].strip() + " \t"

    ### get markup mode, l:qargs is mode's name ###
    mModule = 0
    qargs = vim.eval('l:qargs').strip() or FT_MODES.get(VO.filetype, MODE)
    if qargs:
        mName = 'voom_mode_%s' %qargs
        try:
            mModule = __import__(mName)
            VO.bname += ', %s' %qargs
            vim.command("call Voom_WarningMsg('VOoM: mode ''%s'' [%s]')" %(qargs.replace("'","''"), os.path.abspath(mModule.__file__).replace("'","''")))
        except ImportError:
            vim.command("call Voom_ErrorMsg('VOoM: cannot import Python module %s')" %mName.replace("'","''"))
            return

    VO.mModule = mModule
    ### define mode-specific methods ###
    # no markup mode, default behavior
    if not mModule:
        VO.mmode = 0
        if VO.filetype in MAKE_HEAD:
            VO.makeOutline = makeOutlineH
        else:
            VO.makeOutline = makeOutline
        VO.newHeadline = newHeadline
        VO.changeLevBodyHead = changeLevBodyHead
        VO.hook_doBodyAfterOop = 0
    # markup mode for fold markers, similar to the default behavior
    elif getattr(mModule,'MODE_FMR',0):
        VO.mmode = 0
        f = getattr(mModule,'hook_makeOutline',0)
        if f:
            VO.makeOutline = f
        elif VO.filetype in MAKE_HEAD:
            VO.makeOutline = makeOutlineH
        else:
            VO.makeOutline = makeOutline
        VO.newHeadline = getattr(mModule,'hook_newHeadline',0) or newHeadline
        VO.changeLevBodyHead = changeLevBodyHead
        VO.hook_doBodyAfterOop = 0
    # markup mode not for fold markers
    else:
        VO.mmode = 1
        VO.makeOutline = getattr(mModule,'hook_makeOutline',0) or makeOutline
        VO.newHeadline = getattr(mModule,'hook_newHeadline',0) or newHeadline
        # These must be False if not defined by the markup mode.
        VO.changeLevBodyHead = getattr(mModule,'hook_changeLevBodyHead',0)
        VO.hook_doBodyAfterOop = getattr(mModule,'hook_doBodyAfterOop',0)

    ### the end ###
    vim.command('let l:mmode=%s' %VO.mmode)
    VOOMS[body] = VO


def voom_TreeCreate(): #{{{2
    """This is part of Voom_TreeCreate(), called from Tree."""
    body = int(vim.eval('a:body'))
    blnr = int(vim.eval('l:blnr')) # Body cursor lnum
    VO = VOOMS[body]

    if VO.mmode:
        computeSnLn(body, blnr)
        # reST, wiki files often have most headlines at level >1
        vim.command('setl fdl=2')
        return

    bnodes = VO.bnodes
    Body = VO.Body
    z = len(bnodes)

    ### compute snLn, create Tree folding

    # find bnode marked with '='
    # find bnodes marked with 'o'
    snLn = 0
    marker_re = VO.marker_re
    marker_re_search = marker_re.search
    oFolds = []
    for i in xrange(1,z):
        bline = Body[bnodes[i]-1]
        # part of Body headline after marker+level+'x'
        bline2 = bline[marker_re_search(bline).end():]
        if not bline2: continue
        if bline2[0]=='=':
            snLn = i+1
        elif bline2[0]=='o':
            oFolds.append(i+1)
            if bline2[1:] and bline2[1]=='=':
                snLn = i+1

    # create Tree folding
    if oFolds:
        cFolds = foldingFlip(VO,2,z,oFolds)
        foldingCreate(2,z,cFolds)

    if snLn:
        vim.command('call Voom_SetSnLn(%s,%s)' %(body,snLn))
        VO.snLn = snLn
        # set blnShow if Body cursor is on or before the first headline
        if z > 1 and blnr <= bnodes[1]:
            vim.command('let l:blnShow=%s' %bnodes[snLn-1])
    else:
        # no Body headline is marked with =
        # select current Body node
        computeSnLn(body, blnr)


def makeOutline(VO, blines): #{{{2
    """Return (tlines, bnodes, levels) for Body lines blines.
    blines is either Vim buffer object (Body) or list of buffer lines.
    """
    # blines is usually Body. It is list of clipboard lines during Paste.
    # This function is slower when blines is Vim buffer object instead of
    # Python list. But overall time to do outline update is the same and memory
    # usage is less because we don't create new list (see v3.0 notes)

    # Optimized for buffers in which most lines don't have fold markers.

    # NOTE: duplicate code with makeOutlineH(), only head construction is different
    marker = VO.marker
    marker_re_search = VO.marker_re.search
    Z = len(blines)
    tlines, bnodes, levels = [], [], []
    tlines_add, bnodes_add, levels_add = tlines.append, bnodes.append, levels.append
    c = VO.rstrip_chars
    for i in xrange(Z):
        if not marker in blines[i]: continue
        bline = blines[i]
        m = marker_re_search(bline)
        if not m: continue
        lev = int(m.group(1))
        head = bline[:m.start()].lstrip().rstrip(c).strip('-=~').strip()
        tline = ' %s%s|%s' %(m.group(2) or ' ', '. '*(lev-1), head)
        tlines_add(tline)
        bnodes_add(i+1)
        levels_add(lev)
    return (tlines, bnodes, levels)


def makeOutlineH(VO, blines): #{{{2
    """Identical to makeOutline(), duplicate code. The only difference is that
    a custom function is used to construct Tree headline text.
    """
    # NOTE: duplicate code with makeOutline(), only head construction is different
    marker = VO.marker
    marker_re_search = VO.marker_re.search
    Z = len(blines)
    tlines, bnodes, levels = [], [], []
    tlines_add, bnodes_add, levels_add = tlines.append, bnodes.append, levels.append
    h = MAKE_HEAD[VO.filetype]
    for i in xrange(Z):
        if not marker in blines[i]: continue
        bline = blines[i]
        m = marker_re_search(bline)
        if not m: continue
        lev = int(m.group(1))
        head = h(bline,m)
        tline = ' %s%s|%s' %(m.group(2) or ' ', '. '*(lev-1), head)
        tlines_add(tline)
        bnodes_add(i+1)
        levels_add(lev)
    return (tlines, bnodes, levels)


#--- make_head functions --- {{{2

def make_head_html(bline,match):
    s = bline[:match.start()].strip().strip('-=~').strip()
    if s.endswith('<!'):
        return s[:-2].strip()
    else:
        return s
MAKE_HEAD['html'] = make_head_html

#def make_head_vim(bline,match):
#    return bline[:match.start()].lstrip().rstrip('" \t').strip('-=~').strip()
#MAKE_HEAD['vim'] = make_head_vim

#def make_head_py(bline,match):
#    return bline[:match.start()].lstrip().rstrip('# \t').strip('-=~').strip()
#for ft in 'python ruby perl tcl'.split():
#    MAKE_HEAD[ft] = make_head_py


def updateTree(body,tree): #{{{2
    """Construct outline for Body body.
    Update lines in Tree buffer if needed.
    This can be run from any buffer as long as Tree is set to ma.
    """
    ### Construct outline.
    VO = VOOMS[body]
    assert VO.tree == tree
    #blines = VO.Body[:] # wasteful, see v3.0 notes
    tlines, bnodes, levels  = VO.makeOutline(VO, VO.Body)
    tlines[0:0], bnodes[0:0], levels[0:0] = [VO.bname], [1], [1]
    VO.bnodes, VO.levels = bnodes, levels

    ### Add the = mark.
    snLn = VO.snLn
    Z = len(bnodes)
    # snLn got larger than the number of nodes because some nodes were
    # deleted while editing the Body
    if snLn > Z:
        snLn = Z
        vim.command('call Voom_SetSnLn(%s,%s)' %(body,snLn))
        VO.snLn = snLn
    tlines[snLn-1] = '=%s' %tlines[snLn-1][1:]

    ### Compare Tree lines, draw as needed.
    # Draw all Tree lines only when needed. This is optimization for large
    # outlines, e.g. >1000 Tree lines. Drawing all lines is slower than
    # comparing all lines and then drawing nothing or just one line.

    Tree = VO.Tree
    #tlines_ = Tree[:]
    if not len(Tree)==len(tlines):
        Tree[:] = tlines
        vim.command('let l:ok=1')
        return

    # If only one line is modified, draw that line only. This ensures that
    # editing (and inserting) a single headline in a large outline is fast.
    # If more than one line is modified, draw all lines from first changed line
    # to the end of buffer.
    draw_one = False
    for i in xrange(len(tlines)):
        if not tlines[i]==Tree[i]:
            if draw_one==False:
                draw_one = True
                diff = i
            else:
                Tree[diff:] = tlines[diff:]
                vim.command('let l:ok=1')
                return
    if draw_one:
        Tree[diff] = tlines[diff]

    vim.command('let l:ok=1')
    # why l:ok is needed:  ../../doc/voom.txt#id_20110213212708


def computeSnLn(body, blnr): #{{{2
    """Compute Tree lnum for node at line blnr in Body body.
    Assign Vim and Python snLn vars.
    """
    # snLn should be 1 if blnr is before the first node, top of Body
    VO = VOOMS[body]
    snLn = bisect.bisect_right(VO.bnodes, blnr)
    vim.command('call Voom_SetSnLn(%s,%s)' %(body,snLn))
    VO.snLn = snLn


def voom_UnVoom(body): #{{{2
    if body in VOOMS: del VOOMS[body]


#---Outline Traversal-------------------------{{{1
# Functions for getting node's parents, children, ancestors, etc.
# Nodes here are Tree buffer lnums.
# All we do is traverse VO.levels.


def nodeHasChildren(VO, lnum): #{{{2
    """Determine if node at Tree line lnum has children."""
    levels = VO.levels
    if lnum==1 or lnum==len(levels): return False
    elif levels[lnum-1] < levels[lnum]: return True
    else: return False


def nodeSubnodes(VO, lnum): #{{{2
    """Number of all subnodes for node at Tree line lnum."""
    levels = VO.levels
    z = len(levels)
    if lnum==1 or lnum==z: return 0
    lev = levels[lnum-1]
    for i in xrange(lnum,z):
        if levels[i]<=lev:
            return i-lnum
    return z-lnum


def nodeParent(VO, lnum): #{{{2
    """Return lnum of closest parent of node at Tree line lnum."""
    levels = VO.levels
    lev = levels[lnum-1]
    if lev==1: return None
    for i in xrange(lnum-2,0,-1):
        if levels[i] < lev: return i+1


def nodeAncestors(VO, lnum): #{{{2
    """Return lnums of ancestors of node at Tree line lnum."""
    levels = VO.levels
    lev = levels[lnum-1]
    if lev==1: return []
    ancestors = []
    for i in xrange(lnum-2,0,-1):
        levi = levels[i]
        if levi < lev:
            lev = levi
            ancestors.append(i+1)
            if lev==1:
                ancestors.reverse()
                return ancestors
    # we get here if there are no nodes at level 1 (wiki mode)
    ancestors.reverse()
    return ancestors


def nodeUNL(VO, lnum): #{{{2
    """Compute UNL of node at Tree line lnum.
    Return list of headlines.
    """
    Tree = VO.Tree
    levels = VO.levels
    if lnum==1: return ['top-of-buffer']
    parents = nodeAncestors(VO,lnum)
    parents.append(lnum)
    heads = [Tree[ln-1].split('|',1)[1] for ln in parents]
    return heads


def nodeSiblings(VO, lnum): #{{{2
    """Return lnums of siblings for node at Tree line lnum.
    These are nodes with the same parent and level as lnum node. Sorted in
    ascending order. lnum itself is included. First node (line 1) is never
    included, that is minimum lnum in results is 2.
    """
    levels = VO.levels
    lev = levels[lnum-1]
    siblings = []
    # scan back
    for i in xrange(lnum-1,0,-1):
        levi = levels[i]
        if levi < lev:
            break
        elif levi==lev:
            siblings[0:0] = [i+1]
    # scan forward
    for i in xrange(lnum,len(levels)):
        levi = levels[i]
        if levi < lev:
            break
        elif levi==lev:
            siblings.append(i+1)
    return siblings


def rangeSiblings(VO, lnum1, lnum2): #{{{2
    """Return lnums of siblings for nodes in Tree range lnum1,lnum2.
    These are nodes with the same parent and level as lnum1 node.
    First node (first Tree line) is never included, that is minimum lnum in results is 2.
    Return None if range is ivalid.
    """
    if lnum1==1: lnum1 = 2
    if lnum1 > lnum2: return None
    levels = VO.levels
    lev = levels[lnum1-1]
    siblings = [lnum1]
    for i in xrange(lnum1,lnum2):
        levi = levels[i]
        # invalid range
        if levi < lev:
            return None
        elif levi==lev:
            siblings.append(i+1)
    return siblings


def getSiblingsGroups(VO, siblings): #{{{2
    """Return list of groups of siblings in the region defined by 'siblings'
    group, which is list of siblings in ascending order (Tree lnums).
    Siblings in each group are nodes with the same parent and level.
    Siblings in each group are in ascending order.
    List of groups is reverse-sorted by level of siblings and by parent lnum:
        from RIGHT TO LEFT and from BOTTOM TO TOP.
    """
    if not siblings: return []
    levels = VO.levels
    lnum1, lnum2 = siblings[0], siblings[-1]
    lnum2 = lnum2 + nodeSubnodes(VO,lnum2)

    # get all parents (nodes with children) in the range
    parents = [i for i in xrange(lnum1,lnum2) if levels[i-1]<levels[i]]
    if not parents:
        return [siblings]

    # get children for each parent
    results_dec = [(levels[lnum1-1], 0, siblings)]
    for p in parents:
        sibs = [p+1]
        lev = levels[p] # level of siblings of this parent
        for i in xrange(p+1, lnum2):
            levi = levels[i]
            if levi==lev:
                sibs.append(i+1)
            elif levi < lev:
                break
        results_dec.append((lev, p, sibs))

    results_dec.sort()
    results_dec.reverse()
    results = [i[2] for i in results_dec]
    assert len(parents)+1 == len(results)
    return results


def nodesBodyRange(VO, ln1, ln2, withSubnodes=False): #{{{2
    """Return Body start and end lnums (bln1, bln2) corresponding to nodes at
    Tree lnums ln1 to ln2. Include ln2's subnodes if withSubnodes."""
    bln1 = VO.bnodes[ln1-1]
    if withSubnodes:
        ln2 += nodeSubnodes(VO,ln2)
    if ln2 < len(VO.bnodes):
        bln2 = VO.bnodes[ln2]-1
    else:
        bln2 = len(VO.Body)
    return (bln1,bln2)
    # (bln1,bln2) can be (1,0), see voom_TreeSelect()
    # this is what we want: getbufline(body,1,0)==[]


#---Outline Navigation------------------------{{{1


def voom_TreeSelect(): #{{{2
    # Get first and last lnums of Body node for Tree line lnum.
    lnum = int(vim.eval('l:lnum'))
    body = int(vim.eval('l:body'))
    VO = VOOMS[body]
    VO.snLn = lnum
    vim.command('let l:blnum1=%s' %(VO.bnodes[lnum-1]))
    if lnum < len(VO.bnodes):
        vim.command('let l:blnum2=%s' %(VO.bnodes[lnum]-1 or 1))
    else:
        vim.command("let l:blnum2=%s" %(len(VO.Body)+1))
    # "or 1" takes care of situation when:
    # lnum is 1 (first Tree line) and first Body line is a headline.
    # In that case VO.bnodes is [1, 1, ...] and (l:blnum1,l:blnum2) is (1,0)


def voom_TreeToStartupNode(): #{{{2
    body = int(vim.eval('l:body'))
    VO = VOOMS[body]
    bnodes = VO.bnodes
    Body = VO.Body
    marker_re = VO.marker_re
    z = len(bnodes)
    # find Body headlines marked with '='
    lnums = []
    for i in xrange(1,z):
        bline = Body[bnodes[i]-1]
        # part of Body headline after marker+level+'x'+'o'
        bline2 = bline[marker_re.search(bline).end():]
        if not bline2: continue
        if bline2[0]=='=':
            lnums.append(i+1)
        elif bline2[0]=='o':
            if bline2[1:] and bline2[1]=='=':
                lnums.append(i+1)
    vim.command('let l:lnums=%s' %repr(lnums))


def voom_EchoUNL(): #{{{2
    bufType = vim.eval('l:bufType')
    body = int(vim.eval('l:body'))
    tree = int(vim.eval('l:tree'))
    lnum = int(vim.eval('l:lnum'))

    VO = VOOMS[body]
    assert VO.tree == tree

    if bufType=='Body':
        lnum = bisect.bisect_right(VO.bnodes, lnum)

    heads = nodeUNL(VO,lnum)
    UNL = ' -> '.join(heads)
    vim.command("let @n='%s'" %UNL.replace("'", "''"))
    for h in heads[:-1]:
        vim.command("echon '%s'" %(h.replace("'", "''")))
        vim.command("echohl TabLineFill")
        vim.command("echon ' -> '")
        vim.command("echohl None")
    vim.command("echon '%s'" %(heads[-1].replace("'", "''")))


def voom_Grep(): #{{{2
    body = int(vim.eval('l:body'))
    tree = int(vim.eval('l:tree'))
    VO = VOOMS[body]
    assert VO.tree == tree
    bnodes = VO.bnodes
    matchesAND, matchesNOT = vim.eval('l:matchesAND'), vim.eval('l:matchesNOT')

    # convert blnums of mathes into tlnums, that is node numbers
    tlnumsAND, tlnumsNOT = [], [] # lists of AND and NOT "tlnums" dicts
    counts = {} # {tlnum: count of all AND matches in this node, ...}
    blnums = {} # {tlnum: first AND match in this node, ...}
    for L in matchesAND:
        tlnums = {} # {tlnum of node with a match:0, ...}
        L.pop()
        for bln in L:
            bln = int(bln)
            tln = bisect.bisect_right(bnodes, bln)
            if not tln in blnums:
                blnums[tln] = bln
            elif blnums[tln] > bln:
                blnums[tln] = bln
            if tln in counts:
                counts[tln]+=1
            else:
                counts[tln] = 1
            tlnums[tln] = 0
        tlnumsAND.append(tlnums)
    for L in matchesNOT:
        tlnums = {} # {tlnum of node with a match:0, ...}
        L.pop()
        for bln in L:
            bln = int(bln)
            tln = bisect.bisect_right(bnodes, bln)
            tlnums[tln] = 0
        tlnumsNOT.append(tlnums)

    # if there are only NOT patterns
    if not matchesAND:
        tlnumsAND = [{}.fromkeys(range(1,len(bnodes)+1))]

    # compute intersection
    results = intersectDicts(tlnumsAND, tlnumsNOT)
    results = results.keys()
    results.sort()
    #print results

    # need this to left-align UNLs in the qflist
    max_size = 0
    for t in results:
        if not matchesAND:
            blnums[t] = bnodes[t-1]
            counts[t] = 0
        size = len('%s%s%s' %(t, counts[t], blnums[t]))
        if size > max_size:
            max_size = size

    # list of dictionaries for setloclist() or setqflist()
    loclist = []
    for t in results:
        size = len('%s%s%s' %(t, counts[t], blnums[t]))
        spaces = ' '*(max_size - size)
        UNL = ' -> '.join(nodeUNL(VO,t)).replace("'", "''")
        text = 'n%s:%s%s|%s' %(t, counts[t], spaces, UNL)
        d = "{'text':'%s', 'lnum':%s, 'bufnr':%s}, " %(text, blnums[t], body)
        loclist .append(d)
    #print '\n'.join(loclist)

    vim.command("call setqflist([%s],'a')" %(''.join(loclist)) )


def intersectDicts(dictsAND, dictsNOT): #{{{2
    """Arguments are two lists of dictionaries. Keys are Tree lnums.
    Return dict: intersection of all dicts in dictsAND and non-itersection with
    all dicts in dictsNOT.
    """
    if not dictsAND: return {}
    D1 = dictsAND[0]
    if len(dictsAND)==1:
        res = D1
    else:
        res = {}
    # get intersection with all other AND dicts
    for D in dictsAND[1:]:
        for item in D1:
            if item in D: res[item] = 0
    # get non-intersection with NOT dicts
    for D in dictsNOT:
        keys = res.keys()
        for key in keys:
            if key in D: del res[key]
    return res


#---Outline Operations------------------------{{{1o
# voom_Oop... functions are called from Voom_Oop... Vim functions.
# They use local Vim vars set by the caller and can create and change Vim vars.
# Most of them set lines in Tree and Body via vim.buffer objects.
# Default l:blnShow is -1.
# Returning before setting l:blnShow means no changes were made.


def changeLevTreeHead(h, levDelta): #{{{2
    """Increase of decrese level of Tree headline by levDelta:
    insert or delete  levDelta*". "  string.
    """
    if levDelta > 0:
        return '%s%s%s' %(h[:2], '. '*levDelta, h[2:])
    elif levDelta < 0:
        return '%s%s' %(h[:2], h[2-2*levDelta:])
    else:
        return h


def changeLevBodyHead(VO, h, levDelta): #{{{2
    """Increase of decrease level number of Body headline by levDelta.
    NOTE: markup modes can replace this function with hook_changeLevBodyHead.
    """
    if levDelta==0: return h
    m = VO.marker_re.search(h)
    level = int(m.group(1))
    return '%s%s%s' %(h[:m.start(1)], level+levDelta, h[m.end(1):])


def newHeadline(VO, level, blnum, ln): #{{{2
    """Return (tree_head, bodyLines).
    tree_head is new headline string in Tree buffer (text after |).
    bodyLines is list of lines to insert in Body buffer.
    """
    tree_head = 'NewHeadline'
    bodyLines = ['---%s--- %s%s' %(tree_head, VO.marker, level), '']
    return (tree_head, bodyLines)


def setClipboard(s): #{{{2
    """Set Vim + register (system clipboard) to string s."""
    # important: use '' for Vim string
    vim.command("let @+='%s'" %s.replace("'", "''"))

    # The above failed once: empty clipboard after copy/delete >5MB outline. Could
    # not reproduce after Windows restart. Probably stale system. Thus the
    # following check. It adds about 0.09 sec for each 1MB in the clipboard.
    # 30-40% increase overall in the time of Copy operation (yy).
    if not vim.eval('len(@+)')=='%s' %len(s):
        vim.command("echoerr 'VOoM: error setting clipboard'")


def voom_OopVerify(): #{{{2
    body, tree = int(vim.eval('a:body')), int(vim.eval('a:tree'))
    VO = VOOMS[body]
    assert VO.tree == tree

    tlines, bnodes, levels  = VO.makeOutline(VO, VO.Body)
    if not len(VO.Tree)==len(tlines)+1:
        vim.command("echoerr 'VOoM: wrong Tree size'")
        return
    tlines[0:0], bnodes[0:0], levels[0:0] = [VO.bname], [1], [1]
    snLn = VO.snLn
    tlines[snLn-1] = '=%s' %tlines[snLn-1][1:]

    ok = True
    if not VO.Tree[:] == tlines:
        vim.command("echoerr 'VOoM: DIFFERENT Tree lines'")
        ok = False
    if not VO.bnodes == bnodes:
        vim.command("echoerr 'VOoM: DIFFERENT bnodes'")
        ok = False
    if not VO.levels == levels:
        vim.command("echoerr 'VOoM: DIFFERENT levels'")
        ok = False
    if ok:
        vim.command("let l:ok=1")


def voom_OopSelEnd(): #{{{2
    """This is part of Voom_Oop() checks.
    Selection in Tree starts at line ln1 and ends at line ln2.
    Selection can have many sibling nodes: nodes with the same level as ln1 node.
    Return lnum of last node in the last sibling node's branch.
    Return 0 if selection is invalid.
    """
    body = int(vim.eval('l:body'))
    ln1, ln2  = int(vim.eval('l:ln1')), int(vim.eval('l:ln2'))
    if ln1==1: return 0
    levels = VOOMS[body].levels
    z, lev0 = len(levels), levels[ln1-1]
    for i in xrange(ln1,z):
        lev = levels[i]
        # invalid selection: there is node with level smaller than that of ln1 node
        if i+1 <= ln2 and lev < lev0: return 0
        # node after the last sibling node's branch
        elif i+1 > ln2 and lev <= lev0: return i
    return z


def voom_OopSelectBodyRange(): # {{{2
    body, tree = int(vim.eval('l:body')), int(vim.eval('l:tree'))
    ln1, ln2 = int(vim.eval('l:ln1')), int(vim.eval('l:ln2'))
    VO = VOOMS[body]
    assert VO.tree == tree
    bln1, bln2 = nodesBodyRange(VO, ln1, ln2)
    vim.command("let [l:bln1,l:bln2]=[%s,%s]" %(bln1,bln2))


def voom_OopInsert(as_child=False): #{{{2
    body, tree = int(vim.eval('l:body')), int(vim.eval('l:tree'))
    ln, ln_status = int(vim.eval('l:ln')), vim.eval('l:ln_status')
    VO = VOOMS[body]
    assert VO.tree == tree
    Body, Tree, levels, snLn = VO.Body, VO.Tree, VO.levels, VO.snLn

    # Compute where to insert and at what level.
    # Insert new headline after node at ln.
    # If node is folded, insert after the end of node's tree.
    # default level
    lev = levels[ln-1]
    # after first Tree line
    if ln==1: lev=1
    # as_child always inserts as first child of current node, even if it's folded
    elif as_child: lev+=1
    # after last Tree line, same level
    elif ln==len(levels): pass
    # node has children, it can be folded
    elif lev < levels[ln]:
        # folded: insert after current node's branch, same level
        if ln_status=='folded': ln += nodeSubnodes(VO,ln)
        # not folded, insert as child
        else: lev+=1

    # remove = mark before modifying Tree
    Tree[snLn-1] = ' ' + Tree[snLn-1][1:]

    # insert headline in Tree and Body
    # bLnum is Body lnum after which to insert new headline
    if ln < len(levels):
        bLnum = VO.bnodes[ln]-1
    else:
        bLnum = len(Body)

    tree_head, bodyLines = VO.newHeadline(VO,lev,bLnum,ln)

    treeLine = '= %s|%s' %('. '*(lev-1), tree_head)
    Tree[ln:ln] = [treeLine]
    Body[bLnum:bLnum] = bodyLines

    vim.command('let l:bLnum=%s' %(bLnum+1))

    # write = mark and set snLn to new headline
    Tree[ln] = '=' + Tree[ln][1:]
    VO.snLn = ln+1
    vim.command('call Voom_SetSnLn(%s,%s)' %(body, ln+1))


def voom_OopCopy(): #{{{2
    body = int(vim.eval('l:body'))
    ln1, ln2 = int(vim.eval('l:ln1')), int(vim.eval('l:ln2'))
    VO = VOOMS[body]
    Body, bnodes = VO.Body, VO.bnodes

    # body lines to copy
    bln1 = bnodes[ln1-1]
    if ln2 < len(bnodes): bln2 = bnodes[ln2]-1
    else: bln2 = len(Body)
    blines = Body[bln1-1:bln2]

    setClipboard('\n'.join(blines))


def voom_OopCut(): #{{{2
    body, tree = int(vim.eval('l:body')), int(vim.eval('l:tree'))
    ln1, ln2 = int(vim.eval('l:ln1')), int(vim.eval('l:ln2'))
    lnUp1 = int(vim.eval('l:lnUp1'))
    VO = VOOMS[body]
    assert VO.tree == tree
    Body, Tree = VO.Body, VO.Tree
    bnodes, levels = VO.bnodes, VO.levels

    # diagram {{{
    # .............. blnUp1-1
    # ============== blnUp1=bnodes[lnUp1-1]
    # ..............
    # ============== bln1=bnodes[ln1-1]
    # range being
    # deleted
    # .............. bln2=bnodes[ln2]-1, or last Body line
    # ==============
    # .............. }}}

    ### copy and delete body lines
    bln1 = bnodes[ln1-1]
    if ln2 < len(bnodes): bln2 = bnodes[ln2]-1
    else: bln2 = len(Body)
    blines = Body[bln1-1:bln2]

    setClipboard('\n'.join(blines))
    Body[bln1-1:bln2] = []

    blnShow = bnodes[lnUp1-1] # does not change

    ### update bnodes
    # decrement lnums after deleted range
    delta = bln2-bln1+1
    for i in xrange(ln2,len(bnodes)):
        bnodes[i]-=delta
    # cut
    bnodes[ln1-1:ln2] = []

    ### delete range in levels (same as in Tree)
    levels[ln1-1:ln2] = []

    if VO.hook_doBodyAfterOop:
        VO.hook_doBodyAfterOop(VO, 'cut', 0,  None, None,  None, None,  bln1-1, ln1-1)

    ### ---go back to Tree---
    vim.command('let l:blnShow=%s' %blnShow)
    vim.command("call Voom_OopFromBody(%s,%s,%s,1)" %(body,tree, blnShow))

    ### remove = mark before modifying Tree
    snLn = VO.snLn
    Tree[snLn-1] = ' ' + Tree[snLn-1][1:]
    ### delete range in Tree (same as in levels))
    Tree[ln1-1:ln2] = []

    ### add snLn mark
    Tree[lnUp1-1] = '=' + Tree[lnUp1-1][1:]
    VO.snLn = lnUp1


def voom_OopPaste(): #{{{2
    body, tree = int(vim.eval('l:body')), int(vim.eval('l:tree'))
    ln, ln_status = int(vim.eval('l:ln')), vim.eval('l:ln_status')
    VO = VOOMS[body]
    assert VO.tree == tree
    Body, Tree = VO.Body, VO.Tree
    levels, bnodes = VO.levels, VO.bnodes

    ### clipboard
    pText = vim.eval('@+')
    if not pText:
        vim.command("call Voom_ErrorMsg('VOoM (paste): clipboard is empty')")
        vim.command("call Voom_OopFromBody(%s,%s,-1,1)" %(body,tree))
        return
    pBlines = pText.split('\n') # Body lines to paste
    pTlines, pBnodes, pLevels = VO.makeOutline(VO, pBlines)

    ### verify that clipboard is a valid outline 
    if pBnodes==[] or pBnodes[0]!=1:
        vim.command("call Voom_ErrorMsg('VOoM (paste): invalid clipboard--first line is not a headline')")
        vim.command("call Voom_OopFromBody(%s,%s,-1,1)" %(body,tree))
        return
    lev_ = pLevels[0]
    for lev in pLevels:
        # there is node with level smaller than that of the first node
        if lev < pLevels[0]:
            vim.command("call Voom_ErrorMsg('VOoM (paste): invalid clipboard--root level error')")
            vim.command("call Voom_OopFromBody(%s,%s,-1,1)" %(body,tree))
            return
        # level incremented by 2 or more
        elif lev-lev_ > 1:
            vim.command("call Voom_WarningMsg('VOoM (paste): inconsistent levels in clipboard--level incremented by >1', ' ')")
        lev_ = lev

    ### compute where to insert and at what level
    # insert nodes after node at ln at level lev
    # if node is folded, insert after the end of node's tree
    lev = levels[ln-1] # default level
    # after first Tree line: use level of next node in case min level is not 1 (wiki mode)
    if ln==1:
        if len(levels)>1: lev = levels[1]
        else: lev=1
    # after last Tree line, same level
    elif ln==len(levels): pass
    # node has children, it can be folded
    elif lev < levels[ln]:
        # folded: insert after current node's branch, same level
        if ln_status=='folded': ln += nodeSubnodes(VO,ln)
        # not folded, insert as child
        else: lev+=1

    ### adjust levels of nodes being inserted
    levDelta = lev - pLevels[0]
    if levDelta:
        pTlines = [changeLevTreeHead(h, levDelta) for h in pTlines]
        pLevels = [(lev+levDelta) for lev in pLevels]
        f = VO.changeLevBodyHead
        if f:
            for bl in pBnodes:
                pBlines[bl-1] = f(VO, pBlines[bl-1], levDelta)

    ### insert body lines in Body
    # bln is Body lnum after which to insert
    if ln < len(bnodes): bln = bnodes[ln]-1
    else: bln = len(Body)
    Body[bln:bln] = pBlines
    blnShow = bln+1

    ### update bnodes
    # increment bnodes being pasted
    for i in xrange(0,len(pBnodes)):
        pBnodes[i]+=bln
    # increment bnodes after pasted region
    delta = len(pBlines)
    for i in xrange(ln,len(bnodes)):
        bnodes[i]+=delta
    # insert pBnodes after ln
    bnodes[ln:ln] = pBnodes

    ### insert new levels in levels (same as in Tree)
    levels[ln:ln] = pLevels

    ### start and end lnums of inserted region
    ln1 = ln+1
    ln2 = ln+len(pBnodes)

    if VO.hook_doBodyAfterOop:
        VO.hook_doBodyAfterOop(VO, 'paste', levDelta,
                    blnShow, ln1,
                    blnShow+len(pBlines)-1, ln2,
                    None, None)

    ### ---go back to Tree---
    vim.command("call Voom_OopFromBody(%s,%s,%s,1)" %(body,tree, blnShow))

    # remove = mark before modifying Tree
    snLn = VO.snLn
    Tree[snLn-1] = ' ' + Tree[snLn-1][1:]
    ### insert new headlines in Tree (same as in levels)
    Tree[ln:ln] = pTlines

    ### start and end lnums of inserted region
    vim.command('let l:ln1=%s' %ln1)
    vim.command('let l:ln2=%s' %ln2)
    # set snLn to first headline of inserted nodes
    Tree[ln1-1] = '=' + Tree[ln1-1][1:]
    VO.snLn = ln1

    # we don't get here if previous code fails
    vim.command('let l:blnShow=%s' %blnShow)


def voom_OopUp(): #{{{2
    body, tree = int(vim.eval('l:body')), int(vim.eval('l:tree'))
    ln1, ln2 = int(vim.eval('l:ln1')), int(vim.eval('l:ln2'))
    lnUp1, lnUp2 = int(vim.eval('l:lnUp1')), int(vim.eval('l:lnUp2'))
    VO = VOOMS[body]
    assert VO.tree == tree
    Body, Tree = VO.Body, VO.Tree
    bnodes, levels = VO.bnodes, VO.levels

    # diagram {{{
    # .............. blnUp1-1
    # ============== blnUp1=bnodes[lnUp1-1]
    # range before
    # which to move
    # ..............
    # ============== bln1=bnodes[ln1-1]
    # range being
    # moved
    # .............. bln2=bnodes[ln2]-1, or last Body line
    # ==============
    # .............. }}}

    ### compute change in level
    # current level of root nodes in selection
    levOld = levels[ln1-1]
    # new level of root nodes in selection
    # lnUp1 is fist child of lnUp2, insert also as first child
    if levels[lnUp2-1] + 1 == levels[lnUp1-1]:
        levNew = levels[lnUp1-1]
    # all other cases, includes insertion after folded node
    else:
        levNew = levels[lnUp2-1]
    levDelta = levNew-levOld

    ### body lines to move
    bln1 = bnodes[ln1-1]
    if ln2 < len(bnodes): bln2 = bnodes[ln2]-1
    else: bln2 = len(Body)
    blines = Body[bln1-1:bln2]
    if levDelta:
        f = VO.changeLevBodyHead
        if f:
            for bl in bnodes[ln1-1:ln2]:
                blines[bl-bln1] = f(VO, blines[bl-bln1], levDelta)

    ### move body lines: cut, then insert
    # insert before line blnUp1, it will not change after bnodes update
    blnUp1 = bnodes[lnUp1-1]
    blnShow = blnUp1
    Body[bln1-1:bln2] = []
    Body[blnUp1-1:blnUp1-1] = blines

    ###update bnodes
    # increment lnums in the range before which the move is made
    delta = bln2-bln1+1
    for i in xrange(lnUp1-1,ln1-1):
        bnodes[i]+=delta
    # decrement lnums in the range which is being moved
    delta = bln1-blnUp1
    for i in xrange(ln1-1,ln2):
        bnodes[i]-=delta
    # cut, insert
    nLines = bnodes[ln1-1:ln2]
    bnodes[ln1-1:ln2] = []
    bnodes[lnUp1-1:lnUp1-1] = nLines

    ### update levels (same as for Tree)
    nLevels = levels[ln1-1:ln2]
    if levDelta:
        nLevels = [(lev+levDelta) for lev in nLevels]
    # cut, then insert
    levels[ln1-1:ln2] = []
    levels[lnUp1-1:lnUp1-1] = nLevels

    if VO.hook_doBodyAfterOop:
        VO.hook_doBodyAfterOop(VO, 'up', levDelta,
                    blnShow, lnUp1,
                    blnShow+len(blines)-1, lnUp1+len(nLevels)-1,
                    bln1-1+len(blines), ln1-1+len(nLevels))

    ### ---go back to Tree---
    vim.command("call Voom_OopFromBody(%s,%s,%s,1)" %(body,tree, blnShow))

    ### remove snLn mark before modifying Tree
    snLn = VO.snLn
    Tree[snLn-1] = ' ' + Tree[snLn-1][1:]

    ### update Tree (same as for levels)
    tlines = Tree[ln1-1:ln2]
    if levDelta:
        tlines = [changeLevTreeHead(h, levDelta) for h in tlines]
    # cut, then insert
    Tree[ln1-1:ln2] = []
    Tree[lnUp1-1:lnUp1-1] = tlines

    ### add snLn mark
    Tree[lnUp1-1] = '=' + Tree[lnUp1-1][1:]
    VO.snLn = lnUp1

    # we don't get here only if previous code fails
    vim.command('let l:blnShow=%s' %blnShow)


def voom_OopDown(): #{{{2
    body, tree = int(vim.eval('l:body')), int(vim.eval('l:tree'))
    ln1, ln2 = int(vim.eval('l:ln1')), int(vim.eval('l:ln2'))
    lnDn1, lnDn1_status = int(vim.eval('l:lnDn1')), vim.eval('l:lnDn1_status')
    # note: lnDn1 == ln2+1
    VO = VOOMS[body]
    assert VO.tree == tree
    Body, Tree = VO.Body, VO.Tree
    bnodes, levels = VO.bnodes, VO.levels

    # diagram {{{
    # ..............
    # ============== bln1=bnodes[ln1-1]
    # range being
    # moved
    # .............. bln2=bnodes[ln2]-1
    # ============== blnDn1=bnodes[lnDn1-1]
    # range after
    # which to move
    # .............. blnIns=bnodes[lnIns]-1, or last Body line
    # ==============
    # .............. }}}

    ### compute change in level, and line after which to insert
    # current level
    levOld = levels[ln1-1]
    # new level is either that of lnDn1 or +1
    levNew = levels[lnDn1-1]
    # line afer which to insert
    lnIns = lnDn1
    if lnDn1==len(levels): # end of Tree
        pass
    # lnDn1 has children; insert as child unless it's folded
    elif levels[lnDn1-1] < levels[lnDn1]:
        if lnDn1_status=='folded':
            lnIns += nodeSubnodes(VO,lnDn1)
        else:
            levNew+=1
    levDelta = levNew-levOld

    ### body lines to move
    bln1 = bnodes[ln1-1]
    bln2 = bnodes[ln2]-1
    blines = Body[bln1-1:bln2]
    if levDelta:
        f = VO.changeLevBodyHead
        if f:
            for bl in bnodes[ln1-1:ln2]:
                blines[bl-bln1] = f(VO, blines[bl-bln1], levDelta)

    ### move body lines: insert, then cut
    if lnIns < len(bnodes): blnIns = bnodes[lnIns]-1
    else: blnIns = len(Body)
    Body[blnIns:blnIns] = blines
    Body[bln1-1:bln2] = []

    ### update bnodes
    # increment lnums in the range which is being moved
    delta = blnIns-bln2
    for i in xrange(ln1-1,ln2):
        bnodes[i]+=delta
    # decrement lnums in the range after which the move is made
    delta = bln2-bln1+1
    for i in xrange(ln2,lnIns):
        bnodes[i]-=delta
    # insert, cut
    nLines = bnodes[ln1-1:ln2]
    bnodes[lnIns:lnIns] = nLines
    bnodes[ln1-1:ln2] = []

    ### compute and set new snLn, blnShow
    snLn_ = VO.snLn
    snLn = lnIns+1-(ln2-ln1+1)
    VO.snLn = snLn
    vim.command('let snLn=%s' %snLn)

    blnShow = bnodes[snLn-1] # must compute after bnodes update

    ### update levels (same as for Tree)
    nLevels = levels[ln1-1:ln2]
    if levDelta:
        nLevels = [(lev+levDelta) for lev in nLevels]
    # insert, then cut
    levels[lnIns:lnIns] = nLevels
    levels[ln1-1:ln2] = []

    if VO.hook_doBodyAfterOop:
        VO.hook_doBodyAfterOop(VO, 'down', levDelta,
                    blnShow, snLn,
                    blnShow+len(blines)-1, snLn+len(nLevels)-1,
                    bln1-1, ln1-1)

    ### ---go back to Tree---
    vim.command("call Voom_OopFromBody(%s,%s,%s,1)" %(body,tree, blnShow))

    ### remove snLn mark before modifying Tree
    Tree[snLn_-1] = ' ' + Tree[snLn_-1][1:]

    ### update Tree (same as for levels)
    tlines = Tree[ln1-1:ln2]
    if levDelta:
        tlines = [changeLevTreeHead(h, levDelta) for h in tlines]
    # insert, then cut
    Tree[lnIns:lnIns] = tlines
    Tree[ln1-1:ln2] = []

    ### add snLn mark
    Tree[snLn-1] = '=' + Tree[snLn-1][1:]

    # we don't get here only if previous code fails
    vim.command('let l:blnShow=%s' %blnShow)


def voom_OopRight(): #{{{2
    body, tree = int(vim.eval('l:body')), int(vim.eval('l:tree'))
    ln1, ln2 = int(vim.eval('l:ln1')), int(vim.eval('l:ln2'))
    VO = VOOMS[body]
    assert VO.tree == tree
    Body, Tree = VO.Body, VO.Tree
    bnodes, levels = VO.bnodes, VO.levels

    ### Move right means increment level by 1 for all nodes in the range.

    # can't move right if ln1 node is child of previous node
    if levels[ln1-1] > levels[ln1-2]:
        vim.command("call Voom_OopFromBody(%s,%s,-1,1)" %(body,tree))
        return

    ### change levels of Body headlines
    f = VO.changeLevBodyHead
    if f:
        for bln in bnodes[ln1-1:ln2]:
            Body[bln-1] = f(VO, Body[bln-1], 1)

    # new snLn will be set to ln1
    blnShow = bnodes[ln1-1]

    ### change levels of VO.levels (same as for Tree)
    nLevels = levels[ln1-1:ln2]
    nLevels = [(lev+1) for lev in nLevels]
    levels[ln1-1:ln2] = nLevels

    if VO.hook_doBodyAfterOop:
        if ln2 < len(bnodes): blnum2 = bnodes[ln2]-1
        else: blnum2 = len(Body)
        VO.hook_doBodyAfterOop(VO, 'right', 1, blnShow, ln1, blnum2, ln2, None, None)

    ### ---go back to Tree---
    vim.command("let &fdm=fdm_b")
    vim.command("call Voom_OopFromBody(%s,%s,%s,1)" %(body,tree, blnShow))

    ### change levels of Tree lines (same as for VO.levels)
    tlines = Tree[ln1-1:ln2]
    tlines = [changeLevTreeHead(h, 1) for h in tlines]
    Tree[ln1-1:ln2] = tlines

    ### set snLn to ln1
    snLn = VO.snLn
    if not snLn==ln1:
        Tree[snLn-1] = ' ' + Tree[snLn-1][1:]
        snLn = ln1
        Tree[snLn-1] = '=' + Tree[snLn-1][1:]
        VO.snLn = snLn

    # we don't get here if previous code fails
    vim.command('let l:blnShow=%s' %blnShow)


def voom_OopLeft(): #{{{2
    body, tree = int(vim.eval('l:body')), int(vim.eval('l:tree'))
    ln1, ln2 = int(vim.eval('l:ln1')), int(vim.eval('l:ln2'))
    VO = VOOMS[body]
    assert VO.tree == tree
    Body, Tree = VO.Body, VO.Tree
    bnodes, levels = VO.bnodes, VO.levels

    ### Move left means decrement level by 1 for all nodes in the range.

    # can't move left if at top level 1
    if levels[ln1-1]==1:
        vim.command("call Voom_OopFromBody(%s,%s,-1,1)" %(body,tree))
        return
    # don't move left if the range is not at the end of subtree
    if ln2 < len(levels) and levels[ln2]==levels[ln1-1]:
        vim.command("call Voom_OopFromBody(%s,%s,-1,1)" %(body,tree))
        return

    ### change levels of Body headlines
    f = VO.changeLevBodyHead
    if f:
        for bln in bnodes[ln1-1:ln2]:
            Body[bln-1] = f(VO, Body[bln-1], -1)

    # new snLn will be set to ln1
    blnShow = bnodes[ln1-1]

    ### change levels of VO.levels (same as for Tree)
    nLevels = levels[ln1-1:ln2]
    nLevels = [(lev-1) for lev in nLevels]
    levels[ln1-1:ln2] = nLevels

    if VO.hook_doBodyAfterOop:
        if ln2 < len(bnodes): blnum2 = bnodes[ln2]-1
        else: blnum2 = len(Body)
        VO.hook_doBodyAfterOop(VO, 'left', -1, blnShow, ln1, blnum2, ln2, None, None)

    ### ---go back to Tree---
    vim.command("let &fdm=fdm_b")
    vim.command("call Voom_OopFromBody(%s,%s,%s,1)" %(body,tree, blnShow))

    ### change levels of Tree lines (same as for VO.levels)
    tlines = Tree[ln1-1:ln2]
    tlines = [changeLevTreeHead(h, -1) for h in tlines]
    Tree[ln1-1:ln2] = tlines

    ### set snLn to ln1
    snLn = VO.snLn
    if not snLn==ln1:
        Tree[snLn-1] = ' ' + Tree[snLn-1][1:]
        snLn = ln1
        Tree[snLn-1] = '=' + Tree[snLn-1][1:]
        VO.snLn = snLn

    # we don't get here if previous code fails
    vim.command('let l:blnShow=%s' %blnShow)


def voom_OopMark(): # {{{2
    body, tree = int(vim.eval('l:body')), int(vim.eval('l:tree'))
    ln1, ln2 = int(vim.eval('l:ln1')), int(vim.eval('l:ln2'))
    VO = VOOMS[body]
    assert VO.tree == tree
    Body, Tree = VO.Body, VO.Tree
    bnodes, levels = VO.bnodes, VO.levels
    marker_re = VO.marker_re

    for i in xrange(ln1-1,ln2):
        # insert 'x' in Tree line
        tline = Tree[i]
        if tline[1]!='x':
            Tree[i] = '%sx%s' %(tline[0], tline[2:])
            # insert 'x' in Body headline
            bln = bnodes[i]
            bline = Body[bln-1]
            end = marker_re.search(bline).end(1)
            Body[bln-1] = '%sx%s' %(bline[:end], bline[end:])


def voom_OopUnmark(): # {{{2
    body, tree = int(vim.eval('l:body')), int(vim.eval('l:tree'))
    ln1, ln2 = int(vim.eval('l:ln1')), int(vim.eval('l:ln2'))
    VO = VOOMS[body]
    assert VO.tree == tree
    Body, Tree = VO.Body, VO.Tree
    bnodes, levels = VO.bnodes, VO.levels
    marker_re = VO.marker_re

    for i in xrange(ln1-1,ln2):
        # remove 'x' from Tree line
        tline = Tree[i]
        if tline[1]=='x':
            Tree[i] = '%s %s' %(tline[0], tline[2:])
            # remove 'x' from Body headline
            bln = bnodes[i]
            bline = Body[bln-1]
            end = marker_re.search(bline).end(1)
            # remove one 'x', not enough
            #Body[bln-1] = '%s%s' %(bline[:end], bline[end+1:])
            # remove all consecutive 'x' chars
            Body[bln-1] = '%s%s' %(bline[:end], bline[end:].lstrip('x'))


def voom_OopMarkStartup(): # {{{2
    body, tree = int(vim.eval('l:body')), int(vim.eval('l:tree'))
    ln = int(vim.eval('l:ln'))
    VO = VOOMS[body]
    assert VO.tree == tree
    Body, Tree = VO.Body, VO.Tree
    bnodes, levels = VO.bnodes, VO.levels
    marker_re = VO.marker_re

    if ln==1:
        bln_selected = 0
    else:
        bln_selected = bnodes[ln-1]
    # remove '=' from all other Body headlines
    # also, strip 'x' and 'o' after removed '='
    for bln in bnodes[1:]:
        if bln==bln_selected: continue
        bline = Body[bln-1]
        end = marker_re.search(bline).end()
        bline2 = bline[end:]
        if not bline2: continue
        if bline2[0]=='=':
            Body[bln-1] = '%s%s' %(bline[:end], bline[end:].lstrip('=xo'))
        elif bline2[0]=='o' and bline2[1:] and bline2[1]=='=':
            Body[bln-1] = '%s%s' %(bline[:end+1], bline[end+1:].lstrip('=xo'))

    if ln==1: return

    # insert '=' in current Body headline, but only if it's not there already
    bline = Body[bln_selected-1]
    end = marker_re.search(bline).end()
    bline2 = bline[end:]
    if not bline2:
        Body[bln_selected-1] = '%s=' %bline
        return
    if bline2[0]=='=':
        return
    elif bline2[0]=='o' and bline2[1:] and bline2[1]=='=':
        return
    elif bline2[0]=='o':
        end+=1
    Body[bln_selected-1] = '%s=%s' %(bline[:end], bline[end:])


#--- Tree Folding Operations --- {{{2
# Opened/Closed Tree buffer folds are equivalent to Expanded/Contracted nodes.
# By default, folds are closed.
# Opened folds are marked by 'o' in Body headlines (after 'x', before '=').
#
# To determine which folds are currently closed/opened, we open all closed
# folds one by one, from top to bottom, starting from top level visible folds.
# This produces list of closed folds.
#
# To restore folding according to a list of closed folds:
#   open all folds;
#   close folds from bottom to top.
#
# Conventions:
#   cFolds --lnums of closed folds
#   oFolds --lnums of opened folds
#   ln, ln1, ln2  --Tree line number
#
# NOTE: Cursor position and window view are not restored here.
# See also:
#   ../../doc/voom.txt#id_20110120011733


def voom_OopFolding(action): #{{{3
    body, tree = int(vim.eval('l:body')), int(vim.eval('l:tree'))
    VO = VOOMS[body]
    assert VO.tree == tree
    # check and adjust range lnums
    # don't worry about invalid range lnums: Vim checks that
    if not action=='cleanup':
        ln1, ln2 = int(vim.eval('a:ln1')), int(vim.eval('a:ln2'))
        if ln2<ln1: ln1,ln2=ln2,ln1 # probably redundant
        if ln2==1: return
        #if ln1==1: ln1=2
        if ln1==ln2:
            ln2 = ln2 + nodeSubnodes(VO, ln2)
            if ln1==ln2: return

    if action=='save':
        cFolds = foldingGet(ln1, ln2)
        foldingWrite(VO, ln1, ln2, cFolds)
    elif action=='restore':
        cFolds = foldingRead(VO, ln1, ln2)
        foldingCreate(ln1, ln2, cFolds)
    elif action=='cleanup':
        foldingCleanup(VO)


def foldingGet(ln1, ln2): #{{{3
    """Get all closed folds in line range ln1-ln2, including subfolds.
    If line ln2 is visible and is folded, its subfolds are included.
    Executed in Tree buffer.
    """
    cFolds = []
    lnum = ln1
    # go through top level folded lines (visible closed folds)
    while lnum < ln2+1:
        # line lnum is first line of a closed fold
        if int(vim.eval('foldclosed(%s)' %lnum))==lnum:
            cFolds.append(lnum)
            # line after this fold and subfolds
            foldend = int(vim.eval('foldclosedend(%s)' %lnum))+1
            lnum0 = lnum
            lnum = foldend
            vim.command('keepj normal! %sGzo' %lnum0)
            # open every folded line in this fold
            for ln in xrange(lnum0+1, foldend):
                # line ln is first line of a closed fold
                if int(vim.eval('foldclosed(%s)' %ln))==ln:
                    cFolds.append(ln)
                    vim.command('keepj normal! %sGzo' %ln)
        else:
            lnum+=1

    cFolds.reverse()
    # close back opened folds
    for ln in cFolds:
        vim.command('keepj normal! %sGzc' %ln)
    return cFolds


def foldingCreate(ln1, ln2, cFolds): #{{{3
    """Create folds in range ln1-ln2 from a list of closed folds in that range.
    The list must be reverse sorted. Must not contain nodes without children.
    Executed in Tree buffer.
    """
    #cFolds.sort()
    #cFolds.reverse()
    #vim.command('%s,%sfoldopen!' %(ln1,ln2))
    # see  ../../doc/voom.txt#id_20110120011733
    vim.command(r'try | %s,%sfoldopen! | catch /^Vim\%%((\a\+)\)\=:E490/ | endtry'
            %(ln1,ln2))
    for ln in cFolds:
        vim.command('keepj normal! %sGzc' %ln)


def foldingFlip(VO, ln1, ln2, folds): #{{{3
    """Convert list of opened/closed folds in range ln1-ln2 into list of
    closed/opened folds.
    """
    # Important: this also eliminates lnums of nodes without children,
    # so we don't get Vim E490 (no fold found) error on :foldclose.
    folds = {}.fromkeys(folds)
    folds_flipped = []
    for ln in xrange(ln1,ln2+1):
        if nodeHasChildren(VO, ln) and not ln in folds:
            folds_flipped.append(ln)
    folds_flipped.reverse()
    return folds_flipped


def foldingRead(VO, ln1, ln2): #{{{3
    """Read "o" marks in Body headlines."""
    cFolds = []
    marker_re = VO.marker_re
    bnodes = VO.bnodes
    Body = VO.Body

    for ln in xrange(ln1,ln2+1):
        if not nodeHasChildren(VO, ln):
            continue
        bline = Body[bnodes[ln-1]-1]
        end = marker_re.search(bline).end()
        if end<len(bline) and bline[end]=='o':
            continue
        else:
            cFolds.append(ln)

    cFolds.reverse()
    return cFolds


def foldingWrite(VO, ln1, ln2, cFolds): #{{{3
    """Write "o" marks in Body headlines."""
    cFolds = {}.fromkeys(cFolds)
    marker_re = VO.marker_re
    bnodes = VO.bnodes
    Body = VO.Body

    for ln in xrange(ln1,ln2+1):
        if not nodeHasChildren(VO, ln):
            continue
        bln = bnodes[ln-1]
        bline = Body[bln-1]
        end = marker_re.search(bline).end()
        isClosed = ln in cFolds
        # headline is marked with 'o'
        if end<len(bline) and bline[end]=='o':
            # remove 'o' mark
            if isClosed:
                Body[bln-1] = '%s%s' %(bline[:end], bline[end:].lstrip('ox'))
        # headline is not marked with 'o'
        else:
            # add 'o' mark
            if not isClosed:
                if end==len(bline):
                    Body[bln-1] = '%so' %bline
                elif bline[end] != 'o':
                    Body[bln-1] = '%so%s' %(bline[:end], bline[end:])


def foldingCleanup(VO): #{{{3
    """Remove "o" marks from  from nodes without children."""
    marker_re = VO.marker_re
    bnodes = VO.bnodes
    Body = VO.Body

    for ln in xrange(2,len(bnodes)+1):
        if nodeHasChildren(VO, ln): continue
        bln = bnodes[ln-1]
        bline = Body[bln-1]
        end = marker_re.search(bline).end()
        if end<len(bline) and bline[end]=='o':
            Body[bln-1] = '%s%s' %(bline[:end], bline[end:].lstrip('ox'))


#--- Sort Operations --- {{{2
# 1) Sort siblings of the current node.
# - Get list of siblings of the current node (as Tree lnums).
#   Two nodes are siblings if they have the same parent and the same level.
# - Construct list of corresponding Tree headlines. Decorate with indexes and
#   Tree lnums. Sort by headline text.
# - Construct new Body region from nodes in sorted order. Replace the region.
#   IMPORTANT: this does not change outline data (Tree, VO.levels, VO.bnodes)
#   for nodes with smaller levels or for nodes outside of the siblings region.
#   Thus, recursive sort is possible.
#
# 2) Deep (recursive) sort: sort siblings of the current node and siblings in
# all subnodes. Sort as above for all groups of siblings in the affected
# region, starting from the most deeply nested.
# - Construct list of groups of all siblings: top to bottom, decorate each
#   siblings group with level and parent lnum.
# - Reverse sort the list by levels.
# - Do sort for each group of siblings in the list: from right to left and from
#   bottom to top.
#
# 3) We modify only the Body buffer. We then do global outline update to redraw
# the Tree and to update outline data. Performing targeted update as in other
# outline operations is too tedious.


def voom_OopSort(): #{{{3
    # Returning before setting l:blnShow means no changes were made.
    ### parse options {{{
    oDeep = False
    D = {'oIgnorecase':0, 'oUnicode':0, 'oEnc':0, 'oReverse':0, 'oFlip':0, 'oShuffle':0}
    options = vim.eval('a:qargs')
    options = options.strip().split()
    for o in options:
        if o=='deep': oDeep = True
        elif o=='i':       D['oIgnorecase'] = 1
        elif o=='u':       D['oUnicode']    = 1
        elif o=='r':       D['oReverse']    = 1 # sort in reverse order
        elif o=='flip':    D['oFlip']       = 1 # reverse without sorting
        elif o=='shuffle': D['oShuffle']    = 1
        else:
            vim.command("call Voom_ErrorMsg('VOoM (sort): invalid option: %s')" %o.replace("'","''"))
            vim.command("call Voom_WarningMsg('VOoM (sort): valid options are: deep, i (ignore-case), u (unicode), r (reverse-sort), flip, shuffle')")
            return

    if (D['oReverse'] + D['oFlip'] + D['oShuffle']) > 1:
        vim.command("call Voom_ErrorMsg('VOoM (sort): these options cannot be combined: r, flip, shuffle')")
        return

    if D['oShuffle']:
        global shuffle
        if shuffle is None: from random import shuffle

    if D['oUnicode']:
        D['oEnc'] = get_vim_encoding()
    ###### }}}

    ### get other Vim data, compute 'siblings' {{{
    body, tree = int(vim.eval('l:body')), int(vim.eval('l:tree'))
    ln1, ln2 = int(vim.eval('a:ln1')), int(vim.eval('a:ln2'))
    if ln2<ln1: ln1,ln2=ln2,ln1 # probably redundant
    VO = VOOMS[body]
    assert VO.tree == tree
    Body, Tree = VO.Body, VO.Tree
    bnodes, levels = VO.bnodes, VO.levels

    if ln1==ln2:
        # Tree lnums of all siblings of the current node
        siblings = nodeSiblings(VO,ln1)
    else:
        # Tree lnums of all siblings in the range
        siblings = rangeSiblings(VO,ln1,ln2)
        if not siblings:
            vim.command("call Voom_ErrorMsg('VOoM (sort): invalid Tree selection')")
            return
    ###### }}}
    #print ln1, ln2, siblings

    ### do sorting
    # progress flags: (got >1 siblings, order changed after sort)
    flag1,flag2 = 0,0
    if not oDeep:
        flag1,flag2 = sortSiblings(VO, siblings, **D)
    else:
        siblings_groups = getSiblingsGroups(VO,siblings)
        for group in siblings_groups:
            m, n = sortSiblings(VO, group, **D)
            flag1+=m; flag2+=n

    if flag1==0:
        vim.command("call Voom_WarningMsg('VOoM (sort): nothing to sort')")
        return
    elif flag2==0:
        vim.command("call Voom_WarningMsg('VOoM (sort): already sorted')")
        return

    # Show first sibling. Tracking the current node and bnode is too hard.
    lnum1 = siblings[0]
    lnum2 = siblings[-1] + nodeSubnodes(VO,siblings[-1])
    blnShow = bnodes[lnum1-1]
    vim.command('let [l:blnShow,l:lnum1,l:lnum2]=[%s,%s,%s]' %(blnShow,lnum1,lnum2))


def sortSiblings(VO, siblings, oIgnorecase, oUnicode, oEnc, oReverse, oFlip, oShuffle): #{{{3
    """Sort sibling nodes. 'siblings' is list of Tree lnums in ascending order.
    This only modifies Body buffer. Outline data are not updated.
    Return progress flags (flag1,flag2), see voom_OopSort().
    """
    sibs = siblings
    if len(sibs) < 2:
        return (0,0)
    Body, Tree = VO.Body, VO.Tree
    bnodes, levels = VO.bnodes, VO.levels
    z, Z = len(sibs), len(bnodes)

    ### decorate siblings for sorting
    # [(Tree headline text, index, lnum), ...]
    sibs_dec = []
    for i in xrange(z):
        sib = sibs[i]
        head = Tree[sib-1].split('|',1)[1]
        if oUnicode and oEnc:
            head = unicode(head, oEnc, 'replace')
        if oIgnorecase:
            head = head.lower()
        sibs_dec.append((head, i, sib))

    ### sort
    if oReverse:
        sibs_dec.sort(key=lambda x: x[0], reverse=True)
    elif oFlip:
        sibs_dec.reverse()
    elif oShuffle:
        shuffle(sibs_dec)
    else:
        sibs_dec.sort()

    sibs_sorted = [i[2] for i in sibs_dec]
    #print sibs_dec; print sibs_sorted
    if sibs==sibs_sorted:
        return (1,0)

    ### blnum1, blnum2: first and last Body lnums of the affected region
    blnum1 = bnodes[sibs[0]-1]
    n = sibs[-1] + nodeSubnodes(VO,sibs[-1])
    if n < Z:
        blnum2 = bnodes[n]-1
    else:
        blnum2 = len(Body)

    ### construct new Body region
    blines = []
    for i in xrange(z):
        sib = sibs[i]
        j = sibs_dec[i][1] # index into sibs that points to new sib
        sib_new = sibs[j]

        # get Body region for sib_new branch
        bln1 = bnodes[sib_new-1]
        if j+1 < z:
            sib_next = sibs[j+1]
            bln2 = bnodes[sib_next-1]-1
        else:
            node_last = sib_new + nodeSubnodes(VO,sib_new)
            if node_last < Z:
                bln2 = bnodes[node_last]-1
            else:
                bln2 = len(Body)

        blines.extend(Body[bln1-1:bln2])

    ### replace Body region with the new, sorted region
    body_len = len(Body)
    Body[blnum1-1:blnum2] = blines
    assert body_len == len(Body)

    return (1,1)


#---EXECUTE SCRIPT----------------------------{{{1
#

def voom_GetVoomRange(withSubnodes=0): #{{{2
    body = int(vim.eval('l:body'))
    VO = VOOMS[body]
    lnum = int(vim.eval('a:lnum'))
    if vim.eval('l:bufType')=='Body':
        lnum = bisect.bisect_right(VO.bnodes, lnum)
    bln1, bln2 = nodesBodyRange(VO, lnum, lnum, withSubnodes)
    vim.command("let [l:bln1,l:bln2]=[%s,%s]" %(bln1,bln2))


def voom_GetBuffRange(): #{{{2
    body = int(vim.eval('l:body'))
    ln1, ln2 = int(vim.eval('a:ln1')), int(vim.eval('a:ln2'))
    VO = VOOMS[body]
    bln1, bln2 = nodesBodyRange(VO, ln1, ln2)
    vim.command("let [l:bln1,l:bln2]=[%s,%s]" %(bln1,bln2))


def voom_Exec(): #{{{2
    if vim.eval('l:bufType')=='Tree':
        Buf = VOOMS[int(vim.eval('l:body'))].Body
    else:
        Buf = vim.current.buffer
    bln1, bln2 = int(vim.eval('l:bln1')), int(vim.eval('l:bln2'))
    blines = Buf[bln1-1:bln2]
    # specifiy script encoding (Vim internal encoding) on the first line
    enc = '# -*- coding: %s -*-' %get_vim_encoding()
    # prepend extra \n's to make traceback lnums match buffer lnums
    # TODO: find less silly way to adjust traceback lnums
    script = '%s\n%s%s\n' %(enc, '\n'*(bln1-2), '\n'.join(blines))
    d = {'vim':vim, 'VOOMS':VOOMS, 'voom':sys.modules['voom']}
    try:
        exec script in d
    #except Exception: # does not catch vim.error
    except:
        #traceback.print_exc()  # writes to sys.stderr
        printTraceback(bln1,bln2)

    print '---end of Python script (%s-%s)---' %(bln1,bln2)

# id_20101214100357
# NOTES on printing Python tracebacks and Vim errors.
#
# When there is no PyLog, we want Python traceback echoed as Vim error message.
# Writing to sys.stderr accomplishes that:
#   :py sys.stderr.write('oopsy-doopsy')
# Drawback: writing to default sys.stderr (no PyLog) triggers Vim error.
# Thus, without PyLog there are two useless lines on top with Vim error:
#   Error detected while processing function Voom_Exec:
#   line 63:
#
# Vim code:
#
# 1) PyLog is enabled. Must execute this inside try/catch/entry.
# Otherwise, something weird happens when Vim error occurs, most likely
# Vim error echoing interferes with PyLog scrolling.
# The only downside is that only v:exception is printed, no details
# about Vim error location (v:throwpoint is useless).
#
# 2) PyLog is not enabled. Do not execute this inside try/catch/endtry.
# Python traceback is not printed if we do.
#


def printTraceback(bln1,bln2): #{{{2
    """Print traceback from exception caught during Voomexec."""
    out = None
    # like traceback.format_exc(), traceback.print_exc()
    try:
        etype, value, tb = sys.exc_info()
        out = traceback.format_exception(etype, value, tb)
        #out = traceback.format_exception(etype, value, tb.tb_next)
    finally:
        etype = value = tb = None
    if not out:
        sys.stderr.write('ERROR: Voomexec failed to format Python traceback')
        return
    info = '  ...exception executing script (%s-%s)...\n' %(bln1,bln2)
    if bln1==1:
        info += '  ...subtract 1 from traceback lnums to get buffer lnums...\n'
    out[1:2] = [info]
    #out[1:1] = [info]
    sys.stderr.write(''.join(out))


#---LOG BUFFER--------------------------------{{{1
#
class LogBufferClass: #{{{2
    """A file-like object for replacing sys.stdout and sys.stdin with a Vim buffer."""
    def __init__(self): #{{{3
        self.buffer = vim.current.buffer
        self.logbnr = vim.eval('bufnr("")')
        self.buffer[0] = 'Python Log buffer ...'
        #self.encoding = vim.eval('&enc')
        self.encoding = get_vim_encoding()
        self.join = False

    def write(self,s): #{{{3
        """Append string to buffer, scroll Log windows in all tabs."""
        # Messages are terminated by sending '\n' (null string? ^@).
        # Thus "print '\n'" sends '\n' twice.
        # The message itself can contain '\n's.
        # One line can be sent in many strings which don't always end with \n.
        # This is certainly true for Python errors and for 'print a, b, ...' .

        # Can't append unicode strings. This produces an error:
        #  :py vim.current.buffer.append(u'test')

        # Can't have '\n' in appended list items, so always use splitlines().
        # A trailing \n is lost after splitlines(), but not for '\n\n' etc.
        #print self.buffer.name

        if not s: return
        # Nasty things happen when printing to unloaded PyLog buffer.
        # This also catches printing to noexisting buffer, as in pydoc help() glitch.
        if vim.eval("bufloaded(%s)" %self.logbnr)=='0':
            vim.command("echoerr 'VOoM (PyLog): PyLog buffer %s is unloaded or doesn''t exist'" %self.logbnr)
            vim.command("echoerr 'VOoM (PyLog): unable to write string:'")
            vim.command("echom '%s'" %(repr(s).replace("'", "''")) )
            vim.command("echoerr 'VOoM (PyLog): please try executing command Voomlog to fix'")
            return

        try:
            if type(s) == type(u" "):
                s = s.encode(self.encoding)

            # Join with previous message if it had no ending newline.
            if self.join==True:
                s = self.buffer[-1] + s
                del self.buffer[-1]

            if s[-1]=='\n':
                self.join = False
            else:
                self.join = True

            self.buffer.append(s.splitlines())
        except:
            # list of all exception lines, no newlines in items
            exc_lines = traceback.format_exc().splitlines()
            self.buffer.append('')
            self.buffer.append('VOoM: exception writing to PyLog buffer:')
            self.buffer.append(repr(s))
            self.buffer.append(exc_lines)
            self.buffer.append('')

        vim.command('call Voom_LogScroll()')


#---misc--------------------------------------{{{1

def get_vim_encoding(): #{{{2
    """Return Vim internal encoding."""
    # When &enc is any Unicode Vim allegedly uses utf-8 internally.
    # See |encoding|, mbyte.c, values are from |encoding-values|
    enc = vim.eval('&enc')
    if enc in ('utf-8','ucs-2','ucs-2le','utf-16','utf-16le','ucs-4','ucs-4le'):
        return 'utf-8'
    return enc


# modelines {{{1
# vim:fdm=marker:fdl=0:
# vim:foldtext=getline(v\:foldstart).'...'.(v\:foldend-v\:foldstart):
