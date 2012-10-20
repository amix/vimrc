" This is a sample VOoM add-on.
" It creates global command :VoomInfo which prints various outline information
" about the current buffer if it's a VOoM buffer (Tree or Body)

" This file can be sourced at any time like a regular Vim script. E.g., it can
" be dropped in folder ~/.vim/plugin/ . Of course, VOoM has to be installed for
" the command :VoomInfo to work.
" This works because the name of command function starts with 'Voom_'


com! VoomInfo call Voom_Info()

func! Voom_Info()
    """"""" standard code for every VOoM add-on command
    " Determine if the current buffer is a VOoM Tree buffer, Body buffer, or neither.
    let [bufType,body,tree] = Voom_GetBufInfo()
    " Error, outline is not available (Body is unloaded, outline update failed).
    if body==-1 | return | endif
    """ Do different things depending on the type of the current buffer.
    " Current buffer is not a VOoM buffer (neither Tree nor Body).
    " The error message is printed automatically. It can be suppressed by
    " providing an optional argument: Voom_GetBufInfo(1)
    if bufType==#'None'
        "call Voom_ErrorMsg("VOoM: current buffer is not a VOoM buffer")
        return
    " Current buffer is a VOoM Body. Outline is updated automatically if needed.
    elseif bufType==#'Body'
        call Voom_WarningMsg("in VOoM Body buffer")
    " Current buffer is a VOoM Tree.
    elseif bufType==#'Tree'
        call Voom_WarningMsg("in VOoM Tree buffer")
    endif
    " Get Vim-side outline data. NOTE: Do not modify these dictionaries!
    let [voom_bodies, voom_trees] = Voom_GetData()


    """"""" script-specific code
    " Get Python-side data. This creates Vim local variables.
    py voom_Info()

    echo 'VOoM version:' Voom_GetVar('s:voom_did_quickload')
    echo '__PyLog__ buffer number:' Voom_GetVar('s:voom_logbnr')
    " print outline information
    echo 'VOoM outline for:' getbufline(tree,1)[0][1:]
    echo 'Current buffer is:' bufType
    echo 'Body buffer number:' body
    echo 'Tree buffer number:' tree
    echo 'number of nodes:' l:nodesNumber
    echo 'nodes with/without children:' l:nodesWithChildren '/' l:nodesWithoutChildren
    echo 'max level:' l:maxLevel
    echo 'selected node number:' voom_bodies[body].snLn
    echo 'selected node headline text:' l:selectedHeadline
    echo 'selected node level:' l:selectedNodeLevel
endfunc

python << EOF
def voom_Info():
    body, tree = int(vim.eval('l:body')), int(vim.eval('l:tree'))
    VO = voom.VOOMS[body]
    bnodes, levels = VO.bnodes, VO.levels
    vim.command("let l:maxLevel=%s" %(max(levels)))
    vim.command("let l:nodesNumber=%s" %(len(bnodes)))
    nodesWithChildren = len([i for i in xrange(1,len(bnodes)+1) if voom.nodeHasChildren(VO,i)])
    vim.command("let l:nodesWithChildren=%s" %nodesWithChildren)
    nodesWithoutChildren = len([i for i in xrange(1,len(bnodes)+1) if not voom.nodeHasChildren(VO,i)])
    vim.command("let l:nodesWithoutChildren=%s" %nodesWithoutChildren)
    snLn = VO.snLn
    treeline = VO.Tree[snLn-1]
    if snLn>1:
        selectedHeadline = treeline[treeline.find('|')+1:]
    else:
        selectedHeadline = "top-of-buffer"
    vim.command("let [l:selectedNode,l:selectedHeadline]=[%s,'%s']" %(snLn, selectedHeadline.replace("'","''")))
    vim.command("let l:selectedNodeLevel=%s" %levels[snLn-1])
EOF

