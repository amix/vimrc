" voom.vim
" Last Modified: 2012-05-05
" VOoM -- Vim two-pane outliner, plugin for Python-enabled Vim version 7.x
" Version: 4.3
" Website: http://www.vim.org/scripts/script.php?script_id=2657
" Author: Vlad Irnov (vlad DOT irnov AT gmail DOT com)
" License: This program is free software. It comes without any warranty,
"          to the extent permitted by applicable law. You can redistribute it
"          and/or modify it under the terms of the Do What The Fuck You Want To
"          Public License, Version 2, as published by Sam Hocevar.
"          See http://sam.zoy.org/wtfpl/COPYING for more details.


"---Conventions-------------------------------{{{1
" Tree      --Tree buffer
" Body      --Body buffer
" tree      --Tree buffer number
" body      --Body buffer number
" headline  --Body line with a matching fold marker, also a Tree line
" node      --Body region between two headlines, usually also a fold.
"             A node is identified by Tree lnum (nodes) or Body lnum (bnodes).
" nodes     --list of Tree lnums
" bnodes    --list of Body lnums, line numbers of Body headlines
" bnr       --buffer number
" wnr, tnr  --window number, tab number
" lnum, ln, lnr      --line number, usually Tree
" blnum, bln, blnr   --Body line number
" tline(s)  --Tree line(s)
" bline(s)  --Body line(s)
" snLn      --selected node line number, a Tree line number
" var_      --previous value of var
" l:var     --this var is set by Python code (l:blnShow)
" z, Z      --list siZe, usually len(bnodes)


"---Quickload---------------------------------{{{1
if !exists('s:voom_did_quickload')
    com! -complete=custom,Voom_Complete -nargs=? Voom call Voom_Init(<q-args>)
    com! -complete=custom,Voom_Complete -nargs=? VoomToggle call Voom_Init(<q-args>,1)
    com! Voomhelp call Voom_Help()
    com! Voomlog  call Voom_LogInit()
    com! -nargs=? Voomexec call Voom_Exec(<q-args>)
    exe "au FuncUndefined Voom_* source " . expand("<sfile>:p")
    " support for Vim sessions (:mksession)
    au BufFilePost __PyLog__ call Voom_LogSessionLoad()
    au BufFilePost *_VOOM\d\+ call Voom_TreeSessionLoad()
    let s:voom_did_quickload = 'v4.3'
    finish
endif


"---Initialize--------------------------------{{{1
if !exists('s:voom_did_init')
    let s:script_path = substitute(expand("<sfile>:p"),'\','/','g')
    let s:script_dir = substitute(expand("<sfile>:p:h"),'\','/','g')
    let s:voom_dir = s:script_dir.'/voom'

    let s:voom_logbnr = 0

    " {tree : associated body,  ...}
    let s:voom_trees = {}
    " {body : {'tree' : associated tree,
    "          'blnr' : Body cursor lnum, set when selecting node from Body,
    "          'snLn' : selected node Tree lnum,
    "          'mmode' : 0 (no mode or fmr mode) 1 (markup mode),
    "          'tick' : b:changedtick of Body on Body BufLeave,
    "          'tick_' : b:changedtick of Body on last Tree update}, {...}, ... }
    let s:voom_bodies = {}

python << EOF
import sys, vim
if not vim.eval("s:voom_dir") in sys.path:
    sys.path.append(vim.eval("s:voom_dir"))
import voom
sys.modules['voom'].VOOMS = {}
EOF
    au! FuncUndefined Voom_*
    let s:voom_did_init = 1
endif


"---User Options------------------------------{{{1
" These can be defined in .vimrc .

" Where Tree window is created: 'left', 'right', 'top', 'bottom'
" This is relative to the current window.
if !exists('g:voom_tree_placement')
    let g:voom_tree_placement = 'left'
endif
" Initial Tree window width.
if !exists('g:voom_tree_width')
    let g:voom_tree_width = 30
endif
" Initial Tree window height.
if !exists('g:voom_tree_height')
    let g:voom_tree_height = 12
endif

" Where Log window is created: 'left', 'right', 'top', 'bottom'
" This is far left/right/top/bottom.
if !exists('g:voom_log_placement')
    let g:voom_log_placement = 'bottom'
endif
" Initial Log window width.
if !exists('g:voom_log_width')
    let g:voom_log_width = 30
endif
" Initial Log window height.
if !exists('g:voom_log_height')
    let g:voom_log_height = 12
endif

" Verify outline after outline operations.
if !exists('g:voom_verify_oop')
    let g:voom_verify_oop = 1
endif

" Which key to map to Select-Node-and-Shuttle-between-Body/Tree
if !exists('g:voom_return_key')
    let g:voom_return_key = '<Return>'
endif

" Which key to map to Shuttle-between-Body/Tree
if !exists('g:voom_tab_key')
    let g:voom_tab_key = '<Tab>'
endif

" g:voom_rstrip_chars_{filetype} -- string with chars to strip from right side
" of Tree headlines for Body 'filetype' {filetype}.
" If defined, these will be used instead of 'commentstring' chars.
if !exists('g:voom_rstrip_chars_vim')
    let g:voom_rstrip_chars_vim = "\"# \t"
endif
if !exists('g:voom_rstrip_chars_text')
    let g:voom_rstrip_chars_text = " \t"
endif
if !exists('g:voom_rstrip_chars_help')
    let g:voom_rstrip_chars_help = " \t"
endif


"---Commands----------------------------------{{{1
" Main commands are defined in Quickload section.
" Naming convention: Voomdoit will not modify Body, VoomDoit can modify Body.

com! Voomunl call Voom_EchoUNL()
com! -nargs=? Voomgrep call Voom_Grep(<q-args>)
com! -range -nargs=? VoomSort call Voom_OopSort(<line1>,<line2>, <q-args>)

com! -range VoomFoldingSave    call Voom_OopFolding(<line1>,<line2>, 'save')
com! -range VoomFoldingRestore call Voom_OopFolding(<line1>,<line2>, 'restore')
com! -range VoomFoldingCleanup call Voom_OopFolding(<line1>,<line2>, 'cleanup')

com! Voomtoggle call Voom_ToggleTreeWindow()
com! Voomquit call Voom_DeleteOutline()
com! VoomQuitAll call Voom_DeleteOutlines()

""" development helpers
if exists('g:voom_create_devel_commands')
    " print Vim-side data
    com! VoomPrintData  call Voom_PrintData()
    " reload voom.vim (outlines are preserved)
    com! VoomReloadVim exe 'so '.s:script_path
    " wipe out Trees, PyLog, delete Python modules, reload voom.vim and voom.py
    " Note: simply reloading Python modules is pointless since v4.2
    com! VoomReloadAll call Voom_ReloadAllPre() | exe 'so '.s:script_path
endif


"---Voom_Init(), various commands, helpers----{{{1

func! Voom_Init(qargs,...) "{{{2
" Commands :Voom, :VoomToggle.
    let bnr = bufnr('')
    " Current buffer is Tree.
    if has_key(s:voom_trees, bnr)
        let body = s:voom_trees[bnr]
        if a:0 && a:1
            call Voom_UnVoom(body, bnr)
            return
        endif
        if !hasmapto('Voom_ToTreeOrBodyWin','n')
            echoerr "VOoM: Tree lost mappings. Reconfiguring..."
            call Voom_TreeConfig(body)
        endif
        call Voom_ToBody(body)
        return
    " Current buffer is Body.
    elseif has_key(s:voom_bodies, bnr)
        let tree = s:voom_bodies[bnr].tree
        if a:0 && a:1
            call Voom_UnVoom(bnr, tree)
            return
        endif
        if !hasmapto('Voom_ToTreeOrBodyWin','n')
            echoerr "VOoM: Body lost mappings. Reconfiguring..."
            call Voom_BodyConfig()
        endif
        call Voom_ToTree(tree)
        return
    endif
    " Current buffer is not a VOoM buffer. Create Tree for it. Current buffer
    " becomes a Body buffer.
    let body = bnr
    let s:voom_bodies[body] = {}
    let s:voom_bodies[body].blnr = line('.')
    let [b_name, b_dir] = [expand('%:p:t'), expand('%:p:h')]
    if b_name=='' | let b_name='No Name' | endif
    let l:firstLine = ' '.b_name.' ['.b_dir.'], b'.body
    let [l:mmode, l:qargs] = [-1, a:qargs]
    python voom.voom_Init(int(vim.eval('l:body')))
    if l:mmode < 0 | unlet s:voom_bodies[body] | return | endif
    let s:voom_bodies[body].mmode = l:mmode
    call Voom_BodyConfig()
    call Voom_ToTreeWin()
    call Voom_TreeCreate(body)
    if a:0 && a:1
        call Voom_ToBody(body)
        return
    endif
endfunc


func! Voom_Complete(A,L,P) "{{{2
" Argument completion for command :Voom. Return string "wiki\nvimwiki\nviki..."
" constructed from file names ../plugin/voom/voom_mode_{whatever}.py .
    let thefiles = split(glob(s:voom_dir.'/voom_mode_?*.py'), "\n")
    let themodes = []
    for the in thefiles
        let themode = substitute(fnamemodify(the,':t'), '\c^voom_mode_\(.*\)\.py$', '\1', '')
        call add(themodes, themode)
    endfor
    return join(themodes, "\n")
endfunc


func! Voom_Help() "{{{2
" Open voom.txt as outline in a new tabpage.
    let help_path = fnamemodify(s:script_dir.'/../doc/voom.txt', ":p")
    if !filereadable(help_path)
        echoerr "VOoM: can't read help file:" help_path
        return
    endif

    """ voom.txt exists and is shown in some window in some tab -- go there
    let help_bufnr =  bufnr('^'.help_path.'$')
    if help_bufnr > 0
        let alltabs = range(tabpagenr(),tabpagenr('$')) + range(1,tabpagenr()-1)
        for tnr in alltabs
            if index(tabpagebuflist(tnr), help_bufnr) > -1
                exe 'tabnext '.tnr
                exe bufwinnr(help_bufnr).'wincmd w'
                " make sure critical settings are correct
                if &ft!=#'help'
                    set ft=help
                endif
                if &fmr!=#'[[[,]]]' || &fdm!=#'marker'
                    setl fmr=[[[,]]] fdm=marker
                endif
                " make sure outline is present
                call Voom_Init('')
                return
            endif
        endfor
    endif

    """ try 'tab help' command
    let help_installed = 1
    let [tnr_, tnrM_] = [tabpagenr(), tabpagenr('$')]
    try
        silent tab help voom.txt
    catch /^Vim\%((\a\+)\)\=:E149/ " no help for voom.txt
        let help_installed = 0
    catch /^Vim\%((\a\+)\)\=:E429/ " help file not found--removed after installing
        let help_installed = 0
    endtry
    if help_installed==1
        if fnamemodify(bufname(""), ":t")!=#'voom.txt'
            echoerr "VOoM: internal error"
            return
        endif
        if &fmr!=#'[[[,]]]' || &fdm!=#'marker'
            setl fmr=[[[,]]] fdm=marker
        endif
        call Voom_Init('')
        return
    " 'tab help' failed, we are on new empty tabpage -- kill it
    elseif tabpagenr()!=tnr_ && tabpagenr('$')==tnrM_+1 && bufname('')=='' && winnr('$')==1
        bwipeout
        exe 'tabnext '.tnr_
    endif

    """ open voom.txt as regular file
    exe 'tabnew '.fnameescape(help_path)
    if fnamemodify(bufname(""), ":t")!=#'voom.txt'
        echoerr "VOoM: internal error"
        return
    endif
    if &ft!=#'help'
        setl ft=help
    endif
    if &fmr!=#'[[[,]]]' || &fdm!=#'marker'
        setl fmr=[[[,]]] fdm=marker
    endif
    call Voom_Init('')
endfunc


func! Voom_DeleteOutline(...) "{{{2
" Delete current outline, execute Ex command if in Body or non-VOoM buffer.
    let bnr = bufnr('')
    " current buffer is Tree
    if has_key(s:voom_trees, bnr)
        call Voom_UnVoom(s:voom_trees[bnr], bnr)
        return
    " current buffer is Body
    elseif has_key(s:voom_bodies, bnr)
        call Voom_UnVoom(bnr, s:voom_bodies[bnr].tree)
    endif
    " current buffer is Body or non-VOoM buffer
    if a:0
        execute a:1
    endif
endfunc


func! Voom_DeleteOutlines() "{{{2
" Delete all VOoM outlines.
    for bnr in keys(s:voom_trees)
        let tree = str2nr(bnr)
        call Voom_UnVoom(s:voom_trees[tree], tree)
    endfor
endfunc


func! Voom_UnVoom(body,tree) "{{{2
" Remove VOoM data for Body body and its Tree tree.
" Wipeout Tree, delete Body au, etc.
" Can be called from any buffer.
" Note: when called from Tree BufUnload au, tree doesn't exist.
    if has_key(s:voom_bodies, a:body) && has_key(s:voom_trees, a:tree)
        unlet s:voom_bodies[a:body]
        unlet s:voom_trees[a:tree]
    else
        echoerr 'VOoM: internal error'
        return
    endif
    python voom.voom_UnVoom(int(vim.eval('a:body')))
    exe 'au! VoomBody * <buffer='.a:body.'>'
    if bufexists(a:tree)
        "exe 'noautocmd bwipeout '.a:tree
        exe 'au! VoomTree * <buffer='.a:tree.'>'
        exe 'bwipeout '.a:tree
    endif
    if bufnr('')==a:body
        call Voom_BodyUnMap()
    endif
endfunc


func! Voom_FoldStatus(lnum) "{{{2
    " there is no fold
    if foldlevel(a:lnum)==0
        return 'nofold'
    endif
    let fc = foldclosed(a:lnum)
    " line is hidden in fold, cannot determine it's status
    if fc < a:lnum && fc > 0
        return 'hidden'
    " line is first line of a closed fold
    elseif fc==a:lnum
        return 'folded'
    " line is in an opened fold
    else
        return 'notfolded'
    endif
" Helper for dealing with folds. Determine if line lnum is:
"  not in a fold;
"  hidden in a closed fold;
"  not hidden and is a closed fold;
"  not hidden and is in an open fold.
endfunc


func! Voom_WarningMsg(...) "{{{2
    echohl WarningMsg
    for line in a:000
        echo line
    endfor
    echohl None
endfunc


func! Voom_ErrorMsg(...) "{{{2
    echohl ErrorMsg
    for line in a:000
        echom line
    endfor
    echohl None
endfunc


func! Voom_BufLoaded(body) "{{{2
    if !bufloaded(a:body)
        if bufexists(a:body)
            let bname = fnamemodify(bufname(a:body),":t")
            call Voom_ErrorMsg('VOoM: Body buffer '.a:body.' ('.bname.') is not loaded')
        else
            call Voom_ErrorMsg('VOoM: Body buffer '.a:body.' does not exist')
        endif
        return -1
    endif
endfunc


func! Voom_BufEditable(body) "{{{2
" Check if Body is 'noma' or 'ro' before outline operation.
" Also catches if buffer doesn't exist.
    if getbufvar(a:body, "&ma")==0 || getbufvar(a:body, "&ro")==1
        let bname = fnamemodify(bufname(a:body),":t")
        call Voom_ErrorMsg("VOoM: Body buffer ".a:body." (".bname.") is 'nomodifiable' or 'readonly'")
        return -1
    endif
endfunc


func! Voom_SetSnLn(body, snLn) "{{{2
" Set snLn. Used by Python code.
    let s:voom_bodies[a:body].snLn= a:snLn
endfunc


func! Voom_ToggleTreeWindow() "{{{2
" Mimimize/restore Tree window.
    let bnr = bufnr('')
    if has_key(s:voom_bodies, bnr)
        let [body, tree, inBody] = [bnr, s:voom_bodies[bnr].tree, 1]
    elseif has_key(s:voom_trees, bnr)
        let [body, tree, inBody] = [s:voom_trees[bnr], bnr, 0]
    else
        call Voom_ErrorMsg("VOoM: current buffer is not a VOoM buffer")
        return
    endif

    if inBody
        if Voom_ToTree(tree)!=0 | return | endif
    endif

    " current window width (w) and height (h)
    let [winw, winh] = [winwidth(0), winheight(0)]
    " maximum possible w and h (-2 for statusline and tabline)
    let [maxw, maxh] = [&columns, &lines-&cmdheight-2]
    " minimize w, h, or both
    if winw > 1 && winh > 1
        let w:voom_winsave = winsaveview()
        if winw < maxw
            let w:voom_w = winw
            vertical resize 1
        endif
        if winh < maxh
            let w:voom_h = winh
            resize 1
        endif
    " restore w, h, or both
    else
        if winw <= 1
            let w = exists('w:voom_w') ? w:voom_w : g:voom_tree_width
            exe 'vertical resize '.w
        endif
        if winh <= 1
            let h = exists('w:voom_h') ? w:voom_h : g:voom_tree_height
            exe 'resize '.h
        endif
        if exists('w:voom_winsave')
            call winrestview(w:voom_winsave)
        endif
    endif

    if inBody | call Voom_ToBody(body) | endif
endfunc


"--- for external scripts --- {{{2

func! Voom_GetVar(var) "{{{2
    return {a:var}
endfunc


func! Voom_GetData() "{{{2
    return [s:voom_bodies, s:voom_trees]
endfunc


func! Voom_GetBufInfo(...) "{{{2
    let bnr = bufnr('')
    if has_key(s:voom_trees, bnr)
        let [bufType, body, tree] = ['Tree', s:voom_trees[bnr], bnr]
        if Voom_BufLoaded(body) < 0 | return ['Tree',-1,-1] | endif
    elseif has_key(s:voom_bodies, bnr)
        let [bufType, body, tree] = ['Body', bnr, s:voom_bodies[bnr].tree]
        if Voom_BodyUpdateTree() < 0 | return ['Body',-1,-1] | endif
    else
        if !(a:0 && a:1)
            call Voom_ErrorMsg("VOoM: current buffer is not a VOoM buffer")
        endif
        return ['None',0,0]
    endif
    return [bufType, body, tree]
" Helper for external scripts and add-ons.
" Return ['Body'/'Tree', body, tree] for the current buffer.
" Return ['None',0,0] if current buffer is neither Body nor Tree and print
"   error message. To supress the error message: Voom_GetBufInfo(1)
" Return ['Body'/'Tree',-1,-1] if outline is not available.
" Update outline if current buffer is Body.
endfunc


func! Voom_PrintData() "{{{2
" Print Vim-side VOoM data.
    redir => voomData
    silent echo repeat('-', 60)
    for v in ['s:voom_did_quickload', 's:voom_did_init', 's:voom_logbnr', 's:script_dir', 's:script_path', 's:voom_dir', 'g:voom_verify_oop', 's:voom_trees', 's:voom_bodies']
        silent echo v '--' {v}
    endfor
    redir END
    echo ' '
    python print vim.eval('l:voomData')
endfunc


func! Voom_ReloadAllPre() "{{{2
" Helper for reloading the entire plugin and all modes.
" Wipe out all Tree buffers and PyLog buffer. Delete Python voom modules.
    call Voom_DeleteOutlines()
    if s:voom_logbnr && bufexists(s:voom_logbnr)
        exe 'bwipeout '.s:voom_logbnr
    endif
python << EOF
sys.exc_clear()
del sys.modules['voom']
for k in sys.modules.keys():
    if k.startswith('voom_mode_'):
        del sys.modules[k]
del k
EOF
    unlet s:voom_did_init
endfunc


"---Windows Navigation and Creation-----------{{{1
" These deal only with the current tab page.

func! Voom_ToTreeOrBodyWin() "{{{2
" If in Tree window, move to Body window.
" If in Body window, move to Tree window.
" If possible, use previous window.
    let bnr = bufnr('')
    " current buffer is Tree
    if has_key(s:voom_trees, bnr)
        let target = s:voom_trees[bnr]
    " current buffer is Body
    else
        " This happens after Tree is wiped out.
        if !has_key(s:voom_bodies, bnr)
            call Voom_BodyUnMap()
            return
        endif
        let target = s:voom_bodies[bnr].tree
    endif
    " Try previous window. It's the most common case.
    let wnr = winnr('#')
    if winbufnr(wnr)==target
        exe wnr.'wincmd w'
        return
    endif
    " Use any other window.
    if bufwinnr(target) > 0
        exe bufwinnr(target).'wincmd w'
        return
    endif
endfunc


func! Voom_ToTreeWin() "{{{2
" Move to window or create a new one where a Tree will be loaded.
    " Already in a Tree buffer.
    if has_key(s:voom_trees, bufnr('')) | return | endif

    " Use previous window if it shows Tree.
    let wnr = winnr('#')
    if has_key(s:voom_trees, winbufnr(wnr))
        exe wnr.'wincmd w'
        call Voom_SplitIfUnique()
        return
    endif

    " Use any window with a Tree buffer.
    for bnr in tabpagebuflist()
        if has_key(s:voom_trees, bnr)
            exe bufwinnr(bnr).'wincmd w'
            call Voom_SplitIfUnique()
            return
        endif
    endfor

    " Create new window.
    if g:voom_tree_placement=='top'
        exe 'leftabove '.g:voom_tree_height.'split'
    elseif g:voom_tree_placement=='bottom'
        exe 'rightbelow '.g:voom_tree_height.'split'
    elseif g:voom_tree_placement=='left'
        exe 'leftabove '.g:voom_tree_width.'vsplit'
    elseif g:voom_tree_placement=='right'
        exe 'rightbelow '.g:voom_tree_width.'vsplit'
    endif
endfunc


func! Voom_SplitIfUnique() "{{{2
" Split current window if current buffer is not displayed in any other window
" in current tabpage.
    let bnr = bufnr('')
    let wnr = winnr()
    for i in range(1,winnr('$'))
        if winbufnr(i)==bnr && i!=wnr
            return
        endif
    endfor
    if winheight(0) * 2 >= winwidth(0)
        leftabove split
    else
        leftabove vsplit
    endif
endfunc


func! Voom_ToTree(tree) abort "{{{2
" Move cursor to window with Tree buffer tree.
" If there is no such window, load buffer in a new window.
    " Already there.
    if bufnr('')==a:tree | return | endif

    " Try previous window.
    let wnr = winnr('#')
    if winbufnr(wnr)==a:tree
        exe wnr.'wincmd w'
        return
    endif

    " There is window with buffer a:tree.
    if bufwinnr(a:tree) > 0
        exe bufwinnr(a:tree).'wincmd w'
        return
    endif

    " Bail out if Tree is unloaded or doesn't exist.
    " Because of au, this should never happen.
    if !bufloaded(a:tree)
        let body = s:voom_trees[a:tree]
        call Voom_UnVoom(body,a:tree)
        echoerr "VOoM: Tree buffer" a:tree "is not loaded or does not exist. Cleanup has been performed."
        return -1
    endif

    " Load Tree in appropriate window.
    call Voom_ToTreeWin()
    silent exe 'b '.a:tree
    " window-local options will be set on BufEnter
    return 1
endfunc


func! Voom_ToBodyWin() "{{{2
" Split current Tree window to create window where Body will be loaded
    if g:voom_tree_placement=='top'
        exe 'leftabove '.g:voom_tree_height.'split'
        wincmd p
    elseif g:voom_tree_placement=='bottom'
        exe 'rightbelow '.g:voom_tree_height.'split'
        wincmd p
    elseif g:voom_tree_placement=='left'
        exe 'leftabove '.g:voom_tree_width.'vsplit'
        wincmd p
    elseif g:voom_tree_placement=='right'
        exe 'rightbelow '.g:voom_tree_width.'vsplit'
        wincmd p
    endif
endfunc


func! Voom_ToBody(body) abort "{{{2
" Move to window with Body a:body or load it in a new window.
    " Already there.
    if bufnr('')==a:body | return | endif

    " Try previous window.
    let wnr = winnr('#')
    if winbufnr(wnr)==a:body
        exe wnr.'wincmd w'
        return
    endif

    " There is a window with buffer a:body .
    if bufwinnr(a:body) > 0
        exe bufwinnr(a:body).'wincmd w'
        return
    endif

    if !bufloaded(a:body)
        " Body is unloaded. Load it and force outline update.
        if bufexists(a:body)
            call Voom_ToBodyWin()
            exe 'b '.a:body
            call Voom_BodyUpdateTree()
            call Voom_WarningMsg('VOoM: loaded Body buffer and updated outline')
        " Body doesn't exist. Bail out.
        else
            let tree = s:voom_bodies[a:body].tree
            if !has_key(s:voom_trees, tree) || s:voom_trees[tree]!=a:body
                echoerr "VOoM: internal error"
                return -1
            endif
            call Voom_UnVoom(a:body,tree)
            call Voom_ErrorMsg("VOoM: Body ".a:body." does not exist. Cleanup has been performed.")
        endif
        return -1
    endif

    " Create new window and load there.
    call Voom_ToBodyWin()
    exe 'b '.a:body
    return 1
endfunc


func! Voom_ToLogWin() "{{{2
" Create new window where PyLog will be loaded.
    if g:voom_log_placement=='top'
        exe 'topleft '.g:voom_log_height.'split'
    elseif g:voom_log_placement=='bottom'
        exe 'botright '.g:voom_log_height.'split'
    elseif g:voom_log_placement=='left'
        exe 'topleft '.g:voom_log_width.'vsplit'
    elseif g:voom_log_placement=='right'
        exe 'botright '.g:voom_log_width.'vsplit'
    endif
endfunc


"---TREE BUFFERS------------------------------{{{1

func! Voom_TreeCreate(body) "{{{2
" Create new Tree buffer for Body body in the current window.
    let b_name = fnamemodify(bufname(a:body),":t")
    if b_name=='' | let b_name='NoName' | endif
    silent exe 'edit '.fnameescape(b_name).'_VOOM'.a:body
    let tree = bufnr('')
    let blnr = s:voom_bodies[a:body].blnr

    """ Finish initializing VOoM data for this Body.
    let s:voom_bodies[a:body].tree = tree
    let s:voom_trees[tree] = a:body
    let s:voom_bodies[a:body].tick_ = 0
    python voom.VOOMS[int(vim.eval('a:body'))].tree = int(vim.eval('l:tree'))
    python voom.VOOMS[int(vim.eval('a:body'))].Tree = vim.current.buffer

    call Voom_TreeConfig(a:body)

    let l:blnShow = -1
    """ Create outline and draw Tree lines.
    let lz_ = &lz | set lz
    setl ma
    let ul_=&ul | setl ul=-1
    try
        let l:ok = 0
        keepj python voom.updateTree(int(vim.eval('a:body')), int(vim.eval('l:tree')))
        " Draw = mark. Create folding from o marks.
        " This must be done afer creating outline.
        " this assigns s:voom_bodies[body].snLn
        if l:ok
            python voom.voom_TreeCreate()
            let snLn = s:voom_bodies[a:body].snLn
            " Initial draw puts = on first line.
            if snLn > 1
                keepj call setline(snLn, '='.getline(snLn)[1:])
                keepj call setline(1, ' '.getline(1)[1:])
            endif
            let s:voom_bodies[a:body].tick_ = s:voom_bodies[a:body].tick
        endif
    finally
        let &ul=ul_
        setl noma
        let &lz=lz_
    endtry

    """ Position cursor on snLn line. ../doc/voom.txt#id_20110125210844
    keepj normal! gg
    if snLn > 1
        exe "normal! ".snLn."G0f|m'"
        call Voom_TreeZV()
        if line('w0')!=1 && line('w$')!=line('$')
            normal! zz
        endif
    endif

    "--- the end if markup mode ---
    " blnShow is set by voom_TreeCreate() when there is Body headline marked with =
    if l:blnShow > 0
        " go to Body
        let wnr_ = winnr()
        if Voom_ToBody(a:body) < 0 | return | endif
        " show fold at l:blnShow
        exe 'keepj normal! '.l:blnShow.'G'
        if &fdm==#'marker'
            normal! zMzvzt
        else
            normal! zvzt
        endif
        " go back to Tree
        let wnr_ = winnr('#')
        if winbufnr(wnr_)==tree
            exe wnr_.'wincmd w'
        else
            exe bufwinnr(tree).'wincmd w'
        endif
    endif
endfunc


func! Voom_TreeConfig(body) "{{{2
" Configure current buffer as a Tree buffer.
    augroup VoomTree
        au! * <buffer>
        au BufEnter  <buffer> call Voom_TreeBufEnter()
        "au BufUnload <buffer> call Voom_TreeBufUnload()
        au BufUnload <buffer> nested call Voom_TreeBufUnload()
    augroup END

    call Voom_TreeMap()

    " Options local to window.
    call Voom_TreeConfigWin()

    " local to buffer, may be changed by the user
    setl bufhidden=wipe

    " This should allow customizing via ftplugin. Removes syntax hi.
    setl ft=voomtree

    " Options local to buffer. Should not be changed.
    setl nobuflisted buftype=nofile noswapfile
    setl noro ma ff=unix noma

    call Voom_TreeSyntax(a:body)
endfunc


func! Voom_TreeConfigWin() "{{{2
" Tree window-local options.
    setl foldenable
    setl foldtext=getline(v:foldstart).'\ \ \ /'.(v:foldend-v:foldstart)
    setl foldmethod=expr
    setl foldexpr=Voom_TreeFoldexpr(v:lnum)
    setl cul nocuc nowrap nolist
    "setl winfixheight
    setl winfixwidth

    let w:voom_tree = 'VOoM'
endfunc


func! Voom_TreeBufEnter() "{{{2
" Tree BufEnter au.
" Update outline if Body changed since last update. Redraw Tree if needed.
    let tree = bufnr('')
    let body = s:voom_trees[tree]

    if !exists('w:voom_tree')
        call Voom_TreeConfigWin()
    endif

    """ update is not needed
    if s:voom_bodies[body].tick_==s:voom_bodies[body].tick
        return
    endif

    """ don't update if Body is not loaded
    if Voom_BufLoaded(body) < 0 | return | endif

    """ do update
    let snLn_ = s:voom_bodies[body].snLn
    setl ma
    let ul_=&ul | setl ul=-1
    try
        let l:ok = 0
        keepj python voom.updateTree(int(vim.eval('l:body')), int(vim.eval('l:tree')))
        if l:ok
            let s:voom_bodies[body].tick_ = s:voom_bodies[body].tick
        endif
    finally
        let &ul=ul_
        setl noma
    endtry

    " The = mark is placed by updateTree()
    " When nodes are deleted by editing Body, snLn can get > last Tree lnum,
    " updateTree() will set snLn to the last line lnum.
    if snLn_ != s:voom_bodies[body].snLn
        keepj normal! Gzv
    endif
endfunc


func! Voom_TreeBufUnload() "{{{2
" Tree BufUnload au. Wipe out Tree and cleanup.
    let tree = expand("<abuf>")
    if !exists("s:voom_trees") || !has_key(s:voom_trees, tree)
        echoerr "VOoM: internal error"
        return
    endif
    let body = s:voom_trees[tree]
    "echom bufexists(tree) --always 0
    "exe 'noautocmd bwipeout '.tree
    exe 'au! VoomTree * <buffer='.tree.'>'
    exe 'bwipeout '.tree
    call Voom_UnVoom(body,tree)
endfunc


func! Voom_TreeFoldexpr(lnum) "{{{2
    let ind = stridx(getline(a:lnum),'|') / 2
    let indn = stridx(getline(a:lnum+1),'|') / 2
    return indn>ind ? '>'.ind : ind-1
    "return indn>ind ? '>'.ind : indn<ind ? '<'.indn : ind-1
    "return indn==ind ? ind-1 : indn>ind ? '>'.ind : '<'.indn
endfunc


func! Voom_TreeSyntax(body) "{{{2
" Tree buffer default syntax highlighting.
    " first line
    syn match Title /\%1l.*/

    let FT = getbufvar(a:body, "&ft")
    if FT==#'text'
        " organizer nodes: /headline/
        syn match Comment '^[^|]\+|\zs[/#].*' contains=Todo
        syn keyword Todo TODO XXX FIXME
    elseif FT==#'python'
        syn match Statement /^[^|]\+|\zs\%(def\s\|class\s\)/
        syn match Define /^[^|]\+|\zs@/
        syn match Comment /^[^|]\+|\zs#.*/ contains=Todo
        syn keyword Todo contained TODO XXX FIXME
    elseif FT==#'vim'
        syn match Statement /^[^|]\+|\zs\%(fu\%[nction]\>\|def\s\|class\s\)/
        syn match Comment /^[^|]\+|\zs\%("\|#\).*/ contains=Todo
        syn keyword Todo contained TODO XXX FIXME
    elseif FT==#'html' || FT==#'xml'
        syn match Comment /^[^|]\+|\zs<!.*/ contains=Todo
        syn keyword Todo contained TODO XXX FIXME
    else
        """ organizer nodes: /headline/
        "syn match Directory @^[^|]\+|\zs/.*@ contains=Todo
        """ line comment chars: "  #  //  /*  %  ;  <!--
        "syn match Comment @^[^|]\+|\zs\%("\|#\|//\|/\*\|%\|<!--\).*@ contains=Todo
        """ line comment chars with / (organizer nodes) instead of // and /*
        syn match Comment '^[^|]\+|\zs["#/%;].*' contains=Todo
        syn keyword Todo TODO XXX FIXME
    endif

    syn match WarningMsg /^[^|]\+|\zs!\+/

    """ selected node hi, useless with folding
    "syn match Pmenu /^=.\{-}|\zs.*/
    "syn match Pmenu /^=.\{-}\ze|/
endfunc


func! Voom_TreeMap() "{{{2=
" Tree buffer local mappings and commands.
    let cpo_ = &cpo | set cpo&vim

    """ disable keys that change text {{{
" disable common text change commands
noremap <buffer><silent> i <Nop>
noremap <buffer><silent> I <Nop>
noremap <buffer><silent> a <Nop>
noremap <buffer><silent> A <Nop>
noremap <buffer><silent> o <Nop>
noremap <buffer><silent> O <Nop>
noremap <buffer><silent> s <Nop>
noremap <buffer><silent> S <Nop>
noremap <buffer><silent> r <Nop>
noremap <buffer><silent> R <Nop>
noremap <buffer><silent> x <Nop>
noremap <buffer><silent> X <Nop>
noremap <buffer><silent> D <Nop>
noremap <buffer><silent> J <Nop>
noremap <buffer><silent> c <Nop>
noremap <buffer><silent> C <Nop>
noremap <buffer><silent> P <Nop>
noremap <buffer><silent> . <Nop>
noremap <buffer><silent> = <Nop>
noremap <buffer><silent> <Ins> <Nop>
noremap <buffer><silent> <Del> <Nop>
noremap <buffer><silent> <C-x> <Esc>
noremap <buffer><silent> p <Esc>
noremap <buffer><silent> d <Esc>
noremap <buffer><silent> < <Esc>
noremap <buffer><silent> > <Esc>
noremap <buffer><silent> ^ <Esc>
noremap <buffer><silent> _ <Esc>

" disable undo (also case conversion)
noremap <buffer><silent> u <Nop>
noremap <buffer><silent> U <Nop>
noremap <buffer><silent> <C-r> <Nop>

" disable creation/deletion of folds
noremap <buffer><silent> zf <Nop>
noremap <buffer><silent> zF <Nop>
noremap <buffer><silent> zd <Nop>
noremap <buffer><silent> zD <Nop>
noremap <buffer><silent> zE <Nop>
    """ }}}

    """ edit headline {{{
nnoremap <buffer><silent> i :<C-u>call Voom_OopEdit()<CR>
nnoremap <buffer><silent> I :<C-u>call Voom_OopEdit()<CR>
nnoremap <buffer><silent> a :<C-u>call Voom_OopEdit()<CR>
nnoremap <buffer><silent> A :<C-u>call Voom_OopEdit()<CR>
    """ }}}

    """ node navigation and selection {{{
"--- the following select node -----------
exe "nnoremap <buffer><silent> ".g:voom_return_key." :<C-u>call Voom_TreeSelect(0)<CR>"
exe "vnoremap <buffer><silent> ".g:voom_return_key." <Esc>:<C-u>call Voom_TreeSelect(0)<CR>"
"exe "vnoremap <buffer><silent> ".g:voom_return_key." <Nop>"
exe "nnoremap <buffer><silent> ".g:voom_tab_key." :<C-u>call Voom_ToTreeOrBodyWin()<CR>"
exe "vnoremap <buffer><silent> ".g:voom_tab_key." <Esc>:<C-u>call Voom_ToTreeOrBodyWin()<CR>"
"exe "vnoremap <buffer><silent> ".g:voom_tab_key." <Nop>"

" MOUSE: Left mouse release. Triggered when resizing window with the mouse.
nnoremap <buffer><silent> <LeftRelease> <LeftRelease>:<C-u>call Voom_TreeMouseClick()<CR>
inoremap <buffer><silent> <LeftRelease> <LeftRelease><Esc>
" disable Left mouse double click to avoid entering Visual mode
nnoremap <buffer><silent> <2-LeftMouse> <Nop>

nnoremap <buffer><silent> <Down> <Down>:<C-u>call Voom_TreeSelect(1)<CR>
nnoremap <buffer><silent>   <Up>   <Up>:<C-u>call Voom_TreeSelect(1)<CR>

nnoremap <buffer><silent> <Left>  :<C-u>call Voom_TreeLeft()<CR>
nnoremap <buffer><silent> <Right> :<C-u>call Voom_TreeRight()<CR>

nnoremap <buffer><silent> x :<C-u>call Voom_TreeToMark(0)<CR>
nnoremap <buffer><silent> X :<C-u>call Voom_TreeToMark(1)<CR>

"--- the following don't select node -----------

nnoremap <buffer><silent> <Space> :<C-u>call Voom_TreeToggleFold()<CR>
"vnoremap <buffer><silent> <Space> :<C-u>call Voom_TreeToggleFold()<CR>

" put cursor on the selected node
nnoremap <buffer><silent> = :<C-u>call Voom_TreeToSelected()<CR>
" put cursor on the node marked with '=', if any
nnoremap <buffer><silent> + :<C-u>call Voom_TreeToStartupNode()<CR>

" go up to the parent node
nnoremap <buffer><silent> P :<C-u>call Voom_Tree_Pco('P','n')<CR>
" go up to the parent node and contract it
nnoremap <buffer><silent> c :<C-u>call Voom_Tree_Pco('c','n')<CR>
" go down to direct child node
nnoremap <buffer><silent> o :<C-u>call Voom_Tree_Pco('o','n')<CR>

" contract all siblings of current node
nnoremap <buffer><silent> C :<C-u>call Voom_Tree_CO('zC','n')<CR>
" contract all nodes in Visual selection
vnoremap <buffer><silent> C :<C-u>call Voom_Tree_CO('zC','v')<CR>
" expand all siblings of current node
nnoremap <buffer><silent> O :<C-u>call Voom_Tree_CO('zO','n')<CR>
" expand all nodes in Visual selection
vnoremap <buffer><silent> O :<C-u>call Voom_Tree_CO('zO','v')<CR>

" go up to the previous sibling
nnoremap <buffer><silent> K :<C-u>call Voom_Tree_KJUD('K','n')<CR>
vnoremap <buffer><silent> K :<C-u>call Voom_Tree_KJUD('K','v')<CR>
" go down to the next sibling
nnoremap <buffer><silent> J :<C-u>call Voom_Tree_KJUD('J','n')<CR>
vnoremap <buffer><silent> J :<C-u>call Voom_Tree_KJUD('J','v')<CR>
" go up to the uppermost sibling
nnoremap <buffer><silent> U :<C-u>call Voom_Tree_KJUD('U','n')<CR>
vnoremap <buffer><silent> U :<C-u>call Voom_Tree_KJUD('U','v')<CR>
" go down to the downmost sibling
nnoremap <buffer><silent> D :<C-u>call Voom_Tree_KJUD('D','n')<CR>
vnoremap <buffer><silent> D :<C-u>call Voom_Tree_KJUD('D','v')<CR>
    """ }}}

    """ outline operations {{{
" insert new node
nnoremap <buffer><silent> <LocalLeader>i  :<C-u>call Voom_OopInsert('')<CR>
nnoremap <buffer><silent> <LocalLeader>I  :<C-u>call Voom_OopInsert('as_child')<CR>

" move
nnoremap <buffer><silent> <LocalLeader>u  :<C-u>call Voom_Oop('up', 'n')<CR>
nnoremap <buffer><silent>         <C-Up>  :<C-u>call Voom_Oop('up', 'n')<CR>
nnoremap <buffer><silent>             ^^  :<C-u>call Voom_Oop('up', 'n')<CR>
vnoremap <buffer><silent> <LocalLeader>u  :<C-u>call Voom_Oop('up', 'v')<CR>
vnoremap <buffer><silent>         <C-Up>  :<C-u>call Voom_Oop('up', 'v')<CR>
vnoremap <buffer><silent>             ^^  :<C-u>call Voom_Oop('up', 'v')<CR>

nnoremap <buffer><silent> <LocalLeader>d  :<C-u>call Voom_Oop('down', 'n')<CR>
nnoremap <buffer><silent>       <C-Down>  :<C-u>call Voom_Oop('down', 'n')<CR>
nnoremap <buffer><silent>             __  :<C-u>call Voom_Oop('down', 'n')<CR>
vnoremap <buffer><silent> <LocalLeader>d  :<C-u>call Voom_Oop('down', 'v')<CR>
vnoremap <buffer><silent>       <C-Down>  :<C-u>call Voom_Oop('down', 'v')<CR>
vnoremap <buffer><silent>             __  :<C-u>call Voom_Oop('down', 'v')<CR>

nnoremap <buffer><silent> <LocalLeader>l  :<C-u>call Voom_Oop('left', 'n')<CR>
nnoremap <buffer><silent>       <C-Left>  :<C-u>call Voom_Oop('left', 'n')<CR>
nnoremap <buffer><silent>             <<  :<C-u>call Voom_Oop('left', 'n')<CR>
vnoremap <buffer><silent> <LocalLeader>l  :<C-u>call Voom_Oop('left', 'v')<CR>
vnoremap <buffer><silent>       <C-Left>  :<C-u>call Voom_Oop('left', 'v')<CR>
vnoremap <buffer><silent>             <<  :<C-u>call Voom_Oop('left', 'v')<CR>

nnoremap <buffer><silent> <LocalLeader>r  :<C-u>call Voom_Oop('right', 'n')<CR>
nnoremap <buffer><silent>      <C-Right>  :<C-u>call Voom_Oop('right', 'n')<CR>
nnoremap <buffer><silent>             >>  :<C-u>call Voom_Oop('right', 'n')<CR>
vnoremap <buffer><silent> <LocalLeader>r  :<C-u>call Voom_Oop('right', 'v')<CR>
vnoremap <buffer><silent>      <C-Right>  :<C-u>call Voom_Oop('right', 'v')<CR>
vnoremap <buffer><silent>             >>  :<C-u>call Voom_Oop('right', 'v')<CR>

" cut/copy/paste
nnoremap <buffer><silent>  dd  :<C-u>call Voom_Oop('cut', 'n')<CR>
vnoremap <buffer><silent>  dd  :<C-u>call Voom_Oop('cut', 'v')<CR>

nnoremap <buffer><silent>  yy  :<C-u>call Voom_Oop('copy', 'n')<CR>
vnoremap <buffer><silent>  yy  :<C-u>call Voom_Oop('copy', 'v')<CR>

nnoremap <buffer><silent>  pp  :<C-u>call Voom_OopPaste()<CR>

" mark/unmark
nnoremap <buffer><silent> <LocalLeader>m   :<C-u>call Voom_OopMark('mark', 'n')<CR>
vnoremap <buffer><silent> <LocalLeader>m   :<C-u>call Voom_OopMark('mark', 'v')<CR>

nnoremap <buffer><silent> <LocalLeader>M   :<C-u>call Voom_OopMark('unmark', 'n')<CR>
vnoremap <buffer><silent> <LocalLeader>M   :<C-u>call Voom_OopMark('unmark', 'v')<CR>

" mark node as selected node
nnoremap <buffer><silent> <LocalLeader>=   :<C-u>call Voom_OopMarkStartup()<CR>

" select Body region
nnoremap <buffer><silent> R  :<C-u>call Voom_OopSelectBodyRange('n')<CR>
vnoremap <buffer><silent> R  :<C-u>call Voom_OopSelectBodyRange('v')<CR>
    """ }}}

    """ save/Restore Tree folding {{{
nnoremap <buffer><silent> <LocalLeader>fs  :<C-u>call Voom_OopFolding(line('.'),line('.'), 'save')<CR>
nnoremap <buffer><silent> <LocalLeader>fr  :<C-u>call Voom_OopFolding(line('.'),line('.'), 'restore')<CR>
nnoremap <buffer><silent> <LocalLeader>fas :<C-u>call Voom_OopFolding(1,line('$'), 'save')<CR>
nnoremap <buffer><silent> <LocalLeader>far :<C-u>call Voom_OopFolding(1,line('$'), 'restore')<CR>
    """ }}}

    """ various commands {{{

" echo Tree headline
nnoremap <buffer><silent> s :<C-u>echo getline('.')[(stridx(getline('.'),'<Bar>')+1):]<CR>
" echo UNL
nnoremap <buffer><silent> S :<C-u>call Voom_EchoUNL()<CR>
"nnoremap <buffer><silent> <F1> :<C-u>call Voom_Help()<CR>
nnoremap <buffer><silent> <LocalLeader>e :<C-u>call Voom_Exec('')<CR>
" delete outline
nnoremap <buffer><silent> q :<C-u>call Voom_DeleteOutline()<CR>

    """ }}}

    let &cpo = cpo_
    return
    " Use noremap to disable keys. This must be done first.
    " Use nnoremap and vnoremap in VOoM mappings, don't use noremap.
    " Some keys should be disabled via <Esc> instead of <Nop>:
    "       ../doc/voom.txt#id_20110121201243
    "
    " Do not map <LeftMouse>. Not triggered on first click in the buffer.
    " Triggered on first click in another buffer. Vim probably doesn't know
    " what buffer it is until after the click.
    "
    " Can't use Ctrl: <C-i> is Tab; <C-u>, <C-d> are page up/down.
    " Use <LocalLeader> instead of Ctrl.
    "
    " Still up for grabs: <C-x> <C-j> <C-k> <C-p> <C-n> [ ] { }
endfunc


func! Voom_TreeSessionLoad() "{{{2
" Create outline when loading session created with :mksession.
    if !exists('g:SessionLoad') || &modified || line('$')>1 || getline(1)!=''
        return
    endif
    call setline(1,[' PLEASE','  KILL','   ME (:bw)'])
    setl nomod noma bh=wipe
    " don't -- horrible errors if two tabs with a Tree in each
    "exe 'au SessionLoadPost <buffer> bw '.bufnr('')
    "au SessionLoadPost <buffer> call Voom_TreeSessionLoadPost()
    let [tree, tname] = [bufnr(''), bufname('')]
    if has_key(s:voom_trees,tree) | return | endif
    """ try to find Body matching this Tree buffer name
    let treeName = fnamemodify(tname,':t')
    if treeName !~# '^.\+_VOOM\d\+$' | return | endif
    let bodyName = substitute(treeName, '\C_VOOM\d\+$', '', '')
    let bodyNameM = substitute(bodyName, '[', '[[]', 'g') . '$'
    let [body, bodyWnr] = [bufnr(bodyNameM), bufwinnr(bodyNameM)]
    "echo 'DEBUG' treeName tree '|' bodyName body bodyWnr
    " Body must exist and be in a window in the current tabpage
    if body < 0 || bodyName !=# fnamemodify(bufname(body),':t')
        return
    elseif bodyWnr < 0 || bodyWnr == winnr() || bodyWnr != bufwinnr(body)
        return
    " there is already an outline for this Body
    elseif has_key(s:voom_bodies, body)
        exe 'b'.s:voom_bodies[body].tree
        call Voom_TreeConfigWin()
        return
    endif
    " rename Tree (current buffer), if needed, to correct Body bufnr
    let tname_new = substitute(tname, '\C_VOOM\d\+$', '_VOOM'.body, '')
    if tname !=# tname_new
        if bufexists(tname_new) | return | endif
        let bnrMax_ = bufnr('$')
        exe 'silent file '.fnameescape(tname_new)
        " An unlisted buffer is created to hold the old name. Kill it.
        let bnrMax = bufnr('$')
        if bnrMax > bnrMax_ && bnrMax==bufnr(tname.'$')
            exe 'bwipeout '.bnrMax
        endif
    endif
    """ go to Body, create outline, go back, configure Tree
    let wnr_ = winnr()
    let wnr_p = winnr('#')
    try
        exe 'noautocmd '.bodyWnr.'wincmd w'
        let s:voom_bodies[body] = {}
        let s:voom_bodies[body].blnr = line('.')
        let b_dir = expand('%:p:h')
        let l:firstLine = ' '.bodyName.' ['.b_dir.'], b'.body
        let [l:mmode, l:qargs] = [-1, '']
        python voom.voom_Init(int(vim.eval('l:body')))
        if l:mmode < 0 | unlet s:voom_bodies[body] | return | endif
        let s:voom_bodies[body].mmode = l:mmode
        call Voom_BodyConfig()
    finally
        if wnr_p | exe 'noautocmd '.wnr_p.'wincmd w' | endif
        exe 'noautocmd '.wnr_.'wincmd w'
    endtry
    if bufnr('')==tree
        call Voom_TreeCreate(body)
    endif
endfunc


"---Outline Navigation---{{{2
" To select node from Tree, call Voom_TreeSelect().  ALWAYS return immediately
" after calling Voom_TreeSelect() in case Body checks fail.
"
" To position cursor on | in Tree (not needed if Voom_TreeSelect() is called):
"   call cursor(0,stridx(getline('.'),'|')+1)
"       or
"   normal! 0f|

" Notes: ../doc/voom.txt#id_20110116213809

" zt is affected by 'scrolloff' (Voom_TreeSelect)


func! Voom_TreeSelect(stayInTree) "{{{3
" Select node corresponding to the current Tree line.
" Show correspoding Body's node.
" Leave cursor in Body if current line was in the selected node and !stayInTree.
    let tree = bufnr('')
    let body = s:voom_trees[tree]
    if Voom_BufLoaded(body) < 0 | return | endif
    let lnum = line('.')

    let snLn = s:voom_bodies[body].snLn

    let lz_ = &lz | set lz
    call Voom_TreeZV()
    call cursor(0,stridx(getline('.'),'|')+1)

    " compute l:blnum1, l:blnum2 -- start and end of the selected Body node
    " set VO.snLn before going to Body in case outline update is forced
    python voom.voom_TreeSelect()

    """ Mark new line with =. Remove old = mark.
    if lnum != snLn
        setl ma | let ul_ = &ul | setl ul=-1
        keepj call setline(lnum, '='.getline(lnum)[1:])
        keepj call setline(snLn, ' '.getline(snLn)[1:])
        setl noma | let &ul = ul_
        let s:voom_bodies[body].snLn = lnum
    endif

    """ Go to Body, show selected node, come back or stay in Body.
    if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
    if Voom_BodyCheckTicks(body) < 0 | let &lz=lz_ | return | endif
    let blnum = line('.')
    let gotNewNode = (blnum < l:blnum1) || (blnum > l:blnum2)
    if gotNewNode
        exe 'keepj normal! '.l:blnum1.'G'
        if &fdm ==# 'marker'
            normal! zMzvzt
        else
            normal! zvzt
        endif
    endif

    """ Go back to Tree after showing new node in Body.
    """ Stay in Body if Body's current line was in the selected node.
    if gotNewNode || a:stayInTree
        let wnr_ = winnr('#')
        if winbufnr(wnr_)==tree
            exe wnr_.'wincmd w'
        else
            exe bufwinnr(tree).'wincmd w'
        endif
    endif

    let &lz=lz_
endfunc


func! Voom_TreeZV() "{{{3
" Make current line visible. Return -1 if it was hidden. Like zv, but when
" current line starts a fold, do not open that fold.
    let lnum = line('.')
    let fc = foldclosed(lnum)
    if fc < lnum && fc > 0
        normal! zo
        let fc = foldclosed(lnum)
        while fc < lnum && fc > 0
            normal! zo
            let fc = foldclosed(lnum)
        endwhile
        return -1
    endif
endfunc


func! Voom_TreeToLine(lnum) "{{{3
" Put cursor on line lnum, e.g., snLn.
    if (line('w0') < a:lnum) && (a:lnum > 'w$')
        let offscreen = 0
    else
        let offscreen = 1
    endif
    exe 'keepj normal! '.a:lnum.'G'
    call Voom_TreeZV()
    call cursor(0,stridx(getline('.'),'|')+1)
    if offscreen==1
        normal! zz
    endif
endfunc


func! Voom_TreeToggleFold() "{{{3
" Toggle fold at cursor: expand/contract node.
    let lnum=line('.')
    let ln_status = Voom_FoldStatus(lnum)

    if ln_status=='folded'
        normal! zo
    elseif ln_status=='notfolded'
        if stridx(getline(lnum),'|') < stridx(getline(lnum+1),'|')
            normal! zc
        endif
    elseif ln_status=='hidden'
        call Voom_TreeZV()
    endif
endfunc


func! Voom_TreeMouseClick() "{{{3
" Select node. Toggle fold if click is outside of headline text.
    if !has_key(s:voom_trees, bufnr(''))
        call Voom_ErrorMsg('VOoM: <LeftRelease> in wrong buffer')
        return
    endif
    if virtcol('.')+1 >= virtcol('$') || col('.')-1 < stridx(getline('.'),'|')
        call Voom_TreeToggleFold()
    endif
    call Voom_TreeSelect(1)
endfunc


func! Voom_TreeLeft() "{{{3
" Go to parent node, but first contract current node if it's expanded.
    if Voom_TreeZV() < 0
        call Voom_TreeSelect(1)
        return
    endif
    let lnum = line('.')
    if lnum==1 | return | endif

    let ind = stridx(getline(lnum),'|')
    " next line has bigger indent and line is an opened fold -- close fold
    if stridx(getline(lnum+1),'|') > ind && foldclosed(lnum) < 0
        normal! zc
    " top level -- do not go anywhere
    elseif ind < 3
    " go to parent
    else
        call search('\m^[^|]\{0,'.(ind-2).'}|', 'bWe')
    endif
    call Voom_TreeSelect(1)
endfunc


func! Voom_TreeRight() "{{{3
" Go to first child of current node.
    if Voom_TreeZV() < 0
        call Voom_TreeSelect(1)
        return
    endif
    let lnum = line('.')
    if lnum==1 | return | endif

    " line is first line of a closed fold
    if foldclosed(lnum)==lnum
        normal! zoj
    " line is not in a closed fold and next line has bigger indent
    elseif stridx(getline(lnum),'|') < stridx(getline(lnum+1),'|')
        normal! j
    endif
    call Voom_TreeSelect(1)
endfunc


func! Voom_Tree_KJUD(action, mode) "{{{3
" Move cursor to a sibling node as specified by action: U D K J.
    if Voom_TreeZV() < 0
        call cursor(0,stridx(getline('.'),'|')+1)
        return
    endif
    let lnum = line('.')
    if lnum==1 | return | endif

    if a:mode==#'v'
        let [ln1,ln2] = [line("'<"), line("'>")]
    else
        let [ln1,ln2] = [lnum, lnum]
    endif

    " make sure we are on the first line of visual selection
    " node level is indent of first |
    let vcount1 = v:count1
    exe 'keepj normal! '.ln1.'G0f|'
    let ind = virtcol('.')-1

    " go to the uppermost sibling: up to parent, down to sibling
    if a:action==#'U'
        call search('\m^[^|]\{0,'.(ind-2).'}|', 'bWe')
        if line('.') < lnum
            call search('\m^[^|]\{'.(ind).'}|', 'We')
        else
            keepj normal! gg
            call search('\m^[^|]\{'.(ind).'}|', 'We')
        endif

    " go to the downmost sibling: down to next elder, up to sibling
    elseif a:action==#'D'
        call search('\m^[^|]\{0,'.(ind-2).'}|', 'We')
        if line('.') > lnum
            call search('\m^[^|]\{'.(ind).'}|', 'bWe')
        else
            keepj normal! G0f|
            call search('\m^[^|]\{'.(ind).'}|', 'bcWe')
        endif

    " go up to the previous sibling, stopline is parent
    elseif a:action==#'K'
        let stopline = search('\m^[^|]\{0,'.(ind-2).'}|', 'bWn')
        for i in range(vcount1)
            call search('\m^[^|]\{'.(ind).'}|', 'bWe', stopline)
        endfor

    " go down to the next sibling, stopline is next elder node
    elseif a:action==#'J'
        " must first move to the last sibling in Visual selection
        let sibln = lnum
        while sibln > 0
            let sibln = search('\m^[^|]\{'.(ind).'}|', 'We', ln2)
        endwhile
        let stopline = search('\m^[^|]\{0,'.(ind-2).'}|', 'Wn')
        for i in range(vcount1)
            call search('\m^[^|]\{'.(ind).'}|', 'We', stopline)
        endfor

    endif

    " restore and extend Visual selection
    if a:mode==#'v'
        let lnum = line(".")
        if a:action==#'U' || a:action==#'K'
            exe 'keepj normal! '.ln2.'GV'.lnum.'G0f|'
        elseif a:action==#'D' || a:action==#'J'
            exe 'keepj normal! '.ln1.'GV'.lnum.'G0f|'
        endif
    endif
endfunc


func! Voom_Tree_Pco(action, mode) "{{{3
" action: P c o
    if Voom_TreeZV() < 0
        call cursor(0,stridx(getline('.'),'|')+1)
        return
    endif
    let lnum = line('.')
    if lnum==1 | return | endif

    """ action 'P' or 'c': go up to parent, contract if 'c'
    if a:action==#'c' || a:action==#'P'
        keepj normal! 0f|
        let ind = virtcol('.')-1
        call search('\m^[^|]\{0,'.(ind-2).'}|', 'bWe')
        if a:action==#'c' && line('.') < lnum
            normal! zc
        endif
        return
    " action 'o': go to first child node, same as Voom_TreeRight()
    elseif a:action==#'o'
        " line is first line of a closed fold
        if foldclosed(lnum)==lnum
            normal! zoj0f|
            let fc = foldclosed(lnum)
            if fc < lnum && fc > 0
                normal! zo
            endif
        " line is not in a closed fold and next line has bigger indent
        elseif stridx(getline(lnum),'|') < stridx(getline(lnum+1),'|')
            normal! j0f|
            let fc = foldclosed(lnum)
            if fc < lnum && fc > 0
                normal! zo
            endif
        endif
    endif
endfunc



func! Voom_Tree_CO(action, mode) "{{{3
" action: zC zO
    if Voom_TreeZV() < 0
        call cursor(0,stridx(getline('.'),'|')+1)
        return
    endif
    let lnum = line('.')
    if lnum==1 | return | endif

    """ do 'zC' or 'zO' for all siblings of current node
    if a:mode==#'n'
        keepj normal! 0f|
        let ind = virtcol('.')-1

        let winsave_dict = winsaveview()

        " go the uppermost sibling: up to parent, down to sibling
        call search('\m^[^|]\{0,'.(ind-2).'}|', 'bWe')
        if line('.') < lnum
            let lnUp = search('\m^[^|]\{'.(ind).'}|', 'We')
        else
            keepj normal! gg
            let lnUp = search('\m^[^|]\{'.(ind).'}|', 'We')
        endif
        exe 'keepj normal! '.lnum.'G0f|'

        " go to the last subnode of the downmost sibling: down to elder node, up
        call search('\m^[^|]\{0,'.(ind-2).'}|', 'We')
        if line('.') > lnum
            exe 'keepj normal! '.(line('.')-1).'G0f|'
        else
            keepj normal! G0f|
        endif

        try
            "exe 'keepj normal! V'.lnUp.'GzC'
            exe 'keepj normal! V'.lnUp.'G'.a:action
        catch /^Vim\%((\a\+)\)\=:E490/
        endtry

        call winrestview(winsave_dict)
        exe 'keepj normal! '.lnum.'G0f|'

    """ do 'zC' or 'zO' for all nodes in Visual selection
    elseif a:mode==#'v'
        try
            "normal! gvzC
            exe 'normal! gv'.a:action
        catch /^Vim\%((\a\+)\)\=:E490/
        endtry
    endif

    call Voom_TreeZV()
endfunc



func! Voom_TreeToSelected() "{{{3
" Put cursor on selected node, that is on SnLn line.
    let lnum = s:voom_bodies[s:voom_trees[bufnr('')]].snLn
    call Voom_TreeToLine(lnum)
endfunc


func! Voom_TreeToStartupNode() "{{{3
" Put cursor on startup node, if any: node marked with '=' in Body headline.
" Warn if there are several such nodes.
    let body = s:voom_trees[bufnr('')]
    if s:voom_bodies[body].mmode
        call Voom_ErrorMsg('VOoM: startup nodes are not available in this markup mode')
        return
    endif
    " this creates l:lnums
    python voom.voom_TreeToStartupNode()
    if len(l:lnums)==0
        call Voom_WarningMsg("VOoM: no nodes marked with '='")
        return
    endif
    call Voom_TreeToLine(l:lnums[-1])
    if len(l:lnums)>1
        call Voom_WarningMsg("VOoM: multiple nodes marked with '=': ".join(l:lnums, ', '))
    endif
endfunc


func! Voom_TreeToMark(back) "{{{3
" Go to next or previous marked node.
    if a:back==1
        normal! 0
        let found = search('\C\v^.x', 'bw')
    else
        let found = search('\C\v^.x', 'w')
    endif

    if found==0
        call Voom_WarningMsg("VOoM: there are no marked nodes")
    else
        call Voom_TreeSelect(1)
    endif
endfunc


"---Outline Operations---{{{2

func! Voom_OopSelectBodyRange(mode) "{{{3
" Move to Body and select region corresponding to node(s) in the Tree.
    let tree = bufnr('')
    let body = s:voom_trees[tree]
    if Voom_BufLoaded(body) < 0 | return | endif
    if Voom_BufEditable(body) < 0 | return | endif
    let ln = line('.')
    let ln_status = Voom_FoldStatus(ln)
    " current line must not be hidden in a fold
    if ln_status=='hidden'
        call Voom_ErrorMsg("VOoM: current line is hidden in fold")
        return
    endif
    " normal mode: use current line
    if a:mode=='n'
        let [ln1, ln2] = [ln, ln]
    " visual mode: use range
    elseif a:mode=='v'
        let [ln1, ln2] = [line("'<"), line("'>")]
    endif

    if Voom_ToBody(body) < 0 | return | endif
    if Voom_BodyCheckTicks(body) < 0 | return | endif
    " compute bln1 and bln2
    python voom.voom_OopSelectBodyRange()
    " this happens when ln2==1 and the first headline is top of buffer
    if l:bln2==0 | return | endif
    exe 'normal! '.bln1.'Gzv'.bln2.'GzvV'.bln1.'G'
    if line('w$') < bln2
        normal! zt
    endif
endfunc


func! Voom_OopEdit() "{{{3
" Edit headline text: move into Body, put cursor on headline.
    let tree = bufnr('')
    let body = s:voom_trees[tree]
    if Voom_BufLoaded(body) < 0 | return | endif
    if Voom_BufEditable(body) < 0 | return | endif
    let lnum = line('.')
    if lnum==1 | return | endif

    python vim.command("let l:bLnr=%s" %voom.VOOMS[int(vim.eval('l:body'))].bnodes[int(vim.eval('l:lnum'))-1])

    let lz_ = &lz | set lz
    if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
    if Voom_BodyCheckTicks(body) < 0 | let &lz=lz_ | return | endif

    " do zz only when target line is not in the window
    if l:bLnr < line('w0') || l:bLnr > line('w$')
        let do_zz = 1
    else
        let do_zz = 0
    endif
    exe 'keepj normal! '.l:bLnr.'Gzv^'
    if do_zz
        normal! zz
    endif
    " put cursor on the first word char
    call search('\m\<', 'c', line('.'))
    let &lz=lz_
endfunc


func! Voom_OopInsert(as_child) "{{{3
" Insert new node, headline text should be NewHeadline.
    let tree = bufnr('')
    let body = s:voom_trees[tree]
    if Voom_BufLoaded(body) < 0 | return | endif
    if Voom_BufEditable(body) < 0 | return | endif
    let ln = line('.')
    let ln_status = Voom_FoldStatus(ln)
    if ln_status=='hidden'
        call Voom_ErrorMsg("VOoM: current line is hidden in fold")
        return
    endif

    let lz_ = &lz | set lz
    if v:version > 703 || v:version==703 && has('patch105')
        if s:voom_bodies[body].tick_ != getbufvar(body,'changedtick')
            if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
            if Voom_BodyCheckTicks(body) < 0 | let &lz=lz_ | return | endif
            call Voom_OopFromBody(body,tree,-1,1)
        endif
    else
        if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
        if Voom_BodyCheckTicks(body) < 0 | let &lz=lz_ | return | endif
        call Voom_OopFromBody(body,tree,-1,1)
    endif

    setl ma
    if a:as_child=='as_child'
        keepj python voom.voom_OopInsert(as_child=True)
    else
        keepj python voom.voom_OopInsert(as_child=False)
    endif
    setl noma

    let snLn = s:voom_bodies[body].snLn
    exe "keepj normal! ".snLn."G0f|"
    call Voom_TreeZV()

    if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
    exe "keepj normal! ".l:bLnum."Gzvz\<CR>"
    call search('\CNewHeadline', 'c', line('.'))
    let &lz=lz_
endfunc


func! Voom_OopPaste() "{{{3
" Paste nodes in the clipboard.
    let tree = bufnr('')
    let body = s:voom_trees[tree]
    if Voom_BufLoaded(body) < 0 | return | endif
    if Voom_BufEditable(body) < 0 | return | endif
    let ln = line('.')
    let ln_status = Voom_FoldStatus(ln)
    if ln_status=='hidden'
        call Voom_ErrorMsg("VOoM: current line is hidden in fold")
        return
    endif

    let lz_ = &lz | set lz
    if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
    if Voom_BodyCheckTicks(body) < 0 | let &lz=lz_ | return | endif

    " default bnlShow -1 means pasting not possible
    let l:blnShow = -1

    call setbufvar(tree, '&ma', 1)
    keepj python voom.voom_OopPaste()
    call setbufvar(tree, '&ma', 0)

    " no pasting was done or Python code failed
    if l:blnShow < 0 | let &lz=lz_ | return | endif

    let s:voom_bodies[body].snLn = l:ln1
    if l:ln1==l:ln2
        call Voom_OopShowTree(l:ln1, l:ln2, 'n')
    else
        call Voom_OopShowTree(l:ln1, l:ln2, 'v')
    endif
    let &lz=lz_

    call Voom_OopVerify(body, tree, 'paste')
endfunc


func! Voom_OopMark(op, mode) "{{{3
" Mark or unmark current node or all nodes in selection

    " Checks and init vars. {{{
    let tree = bufnr('')
    let body = s:voom_trees[tree]
    if s:voom_bodies[body].mmode
        call Voom_ErrorMsg('VOoM: marked nodes are not available in this markup mode')
        return
    endif
    if Voom_BufLoaded(body) < 0 | return | endif
    if Voom_BufEditable(body) < 0 | return | endif
    let ln = line('.')
    let ln_status = Voom_FoldStatus(ln)
    " current line must not be hidden in a fold
    if ln_status=='hidden'
        call Voom_ErrorMsg("VOoM: current line is hidden in fold")
        return
    endif
    " normal mode: use current line
    if a:mode=='n'
        let ln1 = ln
        let ln2 = ln
    " visual mode: use range
    elseif a:mode=='v'
        let ln1 = line("'<")
        let ln2 = line("'>")
    endif
    " don't touch first line
    if ln1==1 && ln2==ln1
        return
    elseif ln1==1 && ln2>1
        let ln1=2
    endif
    " }}}

    let lz_ = &lz | set lz
    let fdm_t = &fdm
    if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
    if Voom_BodyCheckTicks(body) < 0 | let &lz=lz_ | return | endif

    let fdm_b=&fdm | setl fdm=manual
    call setbufvar(tree, '&fdm', 'manual')
    call setbufvar(tree, '&ma', 1)
    if a:op=='mark'
        keepj python voom.voom_OopMark()
    elseif a:op=='unmark'
        keepj python voom.voom_OopUnmark()
    endif
    call setbufvar(tree, '&ma', 0)
    let &fdm=fdm_b

    call Voom_OopFromBody(body,tree,0,1)
    let &fdm=fdm_t
    let &lz=lz_

    call Voom_OopVerify(body, tree, a:op)
endfunc


func! Voom_OopMarkStartup() "{{{3
" Mark current node as startup node.
    let tree = bufnr('')
    let body = s:voom_trees[tree]
    if s:voom_bodies[body].mmode
        call Voom_ErrorMsg('VOoM: startup nodes are not available in this markup mode')
        return
    endif
    if Voom_BufLoaded(body) < 0 | return | endif
    if Voom_BufEditable(body) < 0 | return | endif
    let ln = line('.')
    let ln_status = Voom_FoldStatus(ln)
    " current line must not be hidden in a fold
    if ln_status=='hidden'
        call Voom_ErrorMsg("VOoM: current line is hidden in fold")
        return
    endif

    let lz_ = &lz | set lz
    if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
    if Voom_BodyCheckTicks(body) < 0 | let &lz=lz_ | return | endif

    call setbufvar(tree, '&ma', 1)
    keepj python voom.voom_OopMarkStartup()
    call setbufvar(tree, '&ma', 0)

    call Voom_OopFromBody(body,tree,0,1)
    let &lz=lz_

    call Voom_OopVerify(body, tree, 'markStartup')
endfunc


func! Voom_Oop(op, mode) "{{{3
" Outline operations that can be perfomed on current node or on nodes in Visual
" selection. All apply to branches, not to single nodes.

    " Checks and init vars. {{{
    let tree = bufnr('')
    let body = s:voom_trees[tree]
    if Voom_BufLoaded(body) < 0 | return | endif
    if a:op!='copy' && Voom_BufEditable(body) < 0 | return | endif
    let ln = line('.')
    let ln_status = Voom_FoldStatus(ln)
    if ln_status=='hidden'
        call Voom_ErrorMsg("VOoM: node is hidden in fold")
        return
    endif
    " normal mode: use current line
    if a:mode=='n'
        let [ln1,ln2] = [ln,ln]
    " visual mode: use range
    elseif a:mode=='v'
        let [ln1,ln2] = [line("'<"),line("'>")]
        " before op: move cursor to ln1 or ln2
    endif
    if ln1==1
        call Voom_ErrorMsg("VOoM (".a:op."): first Tree line cannot be operated on")
        return
    endif
    " set ln2 to last node in the last sibling branch in selection
    " check validity of selection
    python vim.command('let ln2=%s' %voom.voom_OopSelEnd())
    if ln2==0
        call Voom_ErrorMsg("VOoM: invalid Tree selection")
        return
    endif
    " }}}

    " default bnlShow -1 means no changes were made
    let l:blnShow = -1
    let lz_ = &lz | set lz

    if     a:op=='up' " {{{
        if ln1<3 | let &lz=lz_ | return | endif
        if a:mode=='v'
            " must be on first line of selection
            exe "keepj normal! ".ln1."G"
        endif
        " ln before which to insert, also, new snLn
        normal! k
        let lnUp1 = line('.')
        " top node of a tree after which to insert
        normal! k
        let lnUp2 = line('.')

        if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
        if Voom_BodyCheckTicks(body) < 0 | let &lz=lz_ | return | endif

        call setbufvar(tree, '&ma', 1)
        keepj python voom.voom_OopUp()
        call setbufvar(tree, '&ma', 0)
        " Python code failed
        if l:blnShow < 0 | let &lz=lz_ | return | endif

        let s:voom_bodies[body].snLn = lnUp1
        let lnEnd = lnUp1+ln2-ln1
        call Voom_OopShowTree(lnUp1, lnEnd, a:mode)
        " }}}

    elseif a:op=='down' " {{{
        if ln2==line('$') | let &lz=lz_ | return | endif
        " must be on the last node of current tree or last tree in selection
        exe "keepj normal! ".ln2."G"
        " line after which to insert
        normal! j
        let lnDn1 = line('.') " should be ln2+1
        let lnDn1_status = Voom_FoldStatus(lnDn1)

        if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
        if Voom_BodyCheckTicks(body) < 0 | let &lz=lz_ | return | endif

        call setbufvar(tree, '&ma', 1)
        keepj python voom.voom_OopDown()
        call setbufvar(tree, '&ma', 0)
        " Python code failed
        if l:blnShow < 0 | let &lz=lz_ | return | endif

        let s:voom_bodies[body].snLn = l:snLn
        let lnEnd = snLn+ln2-ln1
        call Voom_OopShowTree(snLn, lnEnd, a:mode)
        " }}}

    elseif a:op=='right' " {{{
        if ln1==2 | let &lz=lz_ | return | endif

        if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
        if Voom_BodyCheckTicks(body) < 0 | let &lz=lz_ | return | endif

        let fdm_b=&fdm | setl fdm=manual
        call setbufvar(tree, '&ma', 1)
        keepj python voom.voom_OopRight()
        call setbufvar(tree, '&ma', 0)

        " can't move right or Python code failed
        if l:blnShow < 0
            call setbufvar(body, '&fdm', fdm_b)
            let &lz=lz_
            return
        endif

        let s:voom_bodies[body].snLn = ln1
        call Voom_OopShowTree(ln1, ln2, a:mode)
        " }}}

    elseif a:op=='left' " {{{
        if ln1==2 | let &lz=lz_ | return | endif

        if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
        if Voom_BodyCheckTicks(body) < 0 | let &lz=lz_ | return | endif

        let fdm_b=&fdm | setl fdm=manual
        call setbufvar(tree, '&ma', 1)
        keepj python voom.voom_OopLeft()
        call setbufvar(tree, '&ma', 0)

        " can't move right or Python code failed
        if l:blnShow < 0
            call setbufvar(body, '&fdm', fdm_b)
            let &lz=lz_
            return
        endif

        let s:voom_bodies[body].snLn = ln1
        call Voom_OopShowTree(ln1, ln2, a:mode)
        " }}}

    elseif a:op=='copy' " {{{
        if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
        if Voom_BodyCheckTicks(body) < 0 | let &lz=lz_ | return | endif

        keepj python voom.voom_OopCopy()

        call Voom_OopFromBody(body,tree,-1,1)
        "}}}

    elseif a:op=='cut' " {{{
        if a:mode=='v'
            " must be on first line of selection
            exe "keepj normal! ".ln1."G"
        endif
        " new snLn
        normal! k
        let lnUp1 = line('.')

        if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
        if Voom_BodyCheckTicks(body) < 0 | let &lz=lz_ | return | endif

        call setbufvar(tree, '&ma', 1)
        keepj python voom.voom_OopCut()
        call setbufvar(tree, '&ma', 0)

        let s:voom_bodies[body].snLn = lnUp1
        call cursor(0,stridx(getline('.'),'|')+1)
        " }}}
    endif

    let &lz=lz_

    call Voom_OopVerify(body, tree, a:op)
endfunc


func! Voom_OopFolding(ln1, ln2, action) "{{{3
" Deal with Tree folding in range ln1-ln2 according to action:
" save, restore, cleanup. Range is ignored if 'cleanup'.
" Since potentially large lists are involved, folds are manipulated in Python.

    " must be in Tree buffer
    let tree = bufnr('')
    if !has_key(s:voom_trees, tree)
        call Voom_ErrorMsg("VOoM: this command must be executed in Tree buffer")
        return
    endif
    let body = s:voom_trees[tree]
    if s:voom_bodies[body].mmode
        call Voom_ErrorMsg('VOoM: Tree folding operations are not available in this markup mode')
        return
    endif
    if Voom_BufLoaded(body) < 0 | return | endif
    if a:action!=#'restore' && Voom_BufEditable(body) < 0
        return
    endif

    " can't deal with folds of node hidden in a fold
    if a:action!=#'cleanup' && Voom_FoldStatus(a:ln1)=='hidden'
        call Voom_ErrorMsg("VOoM: node is hidden in fold")
        return
    endif

    let lz_ = &lz | set lz

    " go to Body, check ticks, go back
    if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
    if Voom_BodyCheckTicks(body) < 0 | let &lz=lz_ | return | endif
    call Voom_OopFromBody(body,tree,-1,1)
    " make sure we are back
    if bufnr('')!=tree
        echoerr "VOoM: internal error" | let &lz=lz_ | return
    endif

    """ diddle with folds
    let winsave_dict = winsaveview()
    python voom.voom_OopFolding(vim.eval('a:action'))
    call winrestview(winsave_dict)

    if a:action!=#'restore'
        " go to Body, set ticks, go back
        if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
        call Voom_OopFromBody(body,tree,0,1)
    endif

    let &lz=lz_
endfunc

func! Voom_OopSort(ln1,ln2,qargs) "{{{3
" Sort siblings in Tree range ln1:ln2 according to options qargs.
" Sort siblings of the current node if range is one line (ln1==ln2).
" If one of the options is 'deep' -- also sort siblings in all subnodes.
" Options are dealt with in the Python code.

    " must be in Tree buffer
    let tree = bufnr('')
    if !has_key(s:voom_trees, tree)
        call Voom_ErrorMsg("VOoM (sort): this command must be executed in Tree buffer")
        return
    endif
    let body = s:voom_trees[tree]
    if Voom_BufLoaded(body) < 0 | return | endif
    if Voom_BufEditable(body) < 0 | return | endif
    if a:ln1 < 2 || a:ln2 < 2
        call Voom_ErrorMsg("VOoM (sort): first Tree line cannot be operated on")
        return
    endif
    if Voom_FoldStatus(a:ln1)=='hidden'
        call Voom_ErrorMsg("VOoM (sort): line is hidden in fold")
        return
    endif

    let Z = line('$')

    let lz_ = &lz | set lz
    """ go to Body window
    if Voom_ToBody(body) < 0 | let &lz=lz_ | return | endif
    if Voom_BodyCheckTicks(body) < 0 | let &lz=lz_ | return | endif

    " default l:bnlShow -1 means no changes were made
    let l:blnShow = -1
    " Modify Body buffer. Tree buffer and outline data are not adjusted.
    keepj python voom.voom_OopSort()
    " IMPORTANT: we rely on Tree BufEnter au to update outline
    call Voom_OopFromBody(body,tree,l:blnShow,0)
    if l:blnShow > 0
        call Voom_OopShowTree(l:lnum1, l:lnum2, a:ln1==a:ln2 ? 'n' : 'v')
    endif

    let &lz=lz_

    " Sorting must not change the number of headlines!
    " (This is problem with reST and Python modes.)
    if Z != line('$')
        call Voom_ErrorMsg("VOoM (sort): ERROR has occurred during sorting!!!", "The number of headlines has changed!!!", "You must undo this sort!!!")
    endif
endfunc


func! Voom_OopFromBody(body, tree, blnShow, setTick) "{{{3
" Move from Body to Tree after an outline operation.
" Set ticks if a:setTick to suppress Tree update on BufEnter.
" Show node (or just line) at Body lnum blnShow.
" Go back to Tree.
" Special blnShow values:
"   -1 --don't set ticks and don't show node.
"    0 --set ticks, but don't show node.

    if bufnr('')!=a:body
        echoerr 'VOoM: internal error'
        return
    endif

    let body_tick = b:changedtick

    if a:blnShow >= 0 && a:setTick
        " set ticks to suppress Tree update
        let s:voom_bodies[a:body].tick_ = b:changedtick
        let s:voom_bodies[a:body].tick  = b:changedtick
    endif

    if a:blnShow > 0
        " show fold at blnShow
        exe 'keepj normal! '.a:blnShow.'G'
        if &fdm==#'marker'
            normal! zMzvzt
        else
            normal! zvzt
        endif
    endif

    " go back to Tree window, which should be previous window
    let wnr_ = winnr('#')
    if winbufnr(wnr_)==a:tree
        exe wnr_.'wincmd w'
    else
        exe bufwinnr(a:tree).'wincmd w'
    endif
    if bufnr('')!=a:tree
        throw 'This is not Tree!'
    endif
    if s:voom_bodies[a:body].tick_ != body_tick
        echoerr 'VOoM: wrong ticks! Forcing outline update.'
        let s:voom_bodies[a:body].tick = body_tick
        call Voom_TreeBufEnter()
    endif
endfunc


func! Voom_OopShowTree(ln1, ln2, mode) " {{{3
" Adjust Tree view after an outline operation.
" ln1 and ln2 are first and last line of the range.
"
" After outline operation Tree folds in the affected range are usually
" completely expanded. To be consistent: close all folds in the range
" (select range, zC, show first line).

    " zv ensures ln1 node is expanded before next GV
    exe 'keepj normal! '.a:ln1.'Gzv'
    " select range and close all folds in range
    exe 'keepj normal! '.a:ln2.'GV'.a:ln1.'G'
    try
        normal! zC
    " E490: No fold found
    catch /^Vim\%((\a\+)\)\=:E490/
    endtry

    " show first node
    call Voom_TreeZV()
    call cursor(0,stridx(getline('.'),'|')+1)

    " restore visual mode selection
    if a:mode=='v'
        normal! gv
    endif
endfunc


func! Voom_OopVerify(body, tree, op) "{{{3
" Verify outline after outline operation. Current buffer is Tree.
    if !g:voom_verify_oop || a:op=='copy'
        return
    endif

    python voom.voom_OopVerify()
    if exists('l:ok')
        return
    endif

    echoerr 'VOoM: outline verification failed after "'.a:op.'". Forcing outline update.'
    let s:voom_bodies[a:body].tick_ = -1
    if bufnr('')!=a:tree
        echoerr 'Current buffer is not Tree! Aborting outline update.'
        return
    endif
    call Voom_TreeBufEnter()
endfunc


"---BODY BUFFERS------------------------------{{{1

func! Voom_BodyConfig() "{{{2
" Configure current buffer as a Body buffer.
    augroup VoomBody
        au! * <buffer>
        au BufLeave <buffer> call Voom_BodyBufLeave()
        au BufEnter <buffer> call Voom_BodyBufEnter()
    augroup END

    " redundant: will be set on BufLeave
    let s:voom_bodies[bufnr('')].tick = b:changedtick

    call Voom_BodyMap()
endfunc


func! Voom_BodyBufLeave() "{{{2
" Body BufLeave au.
" getbufvar() doesn't work with b:changedtick, thus the need for this au
    let s:voom_bodies[bufnr('')].tick = b:changedtick
endfunc


func! Voom_BodyBufEnter() "{{{2
" Body BufEnter au. Restore buffer-local mappings lost after :bd.
    if !hasmapto('Voom_ToTreeOrBodyWin','n')
        call Voom_BodyMap()
    endif
endfunc


func! Voom_BodyMap() "{{{2
" Body buffer local mappings.
    let cpo_ = &cpo | set cpo&vim
    exe "nnoremap <buffer><silent> ".g:voom_return_key." :<C-u>call Voom_BodySelect()<CR>"
    exe "nnoremap <buffer><silent> ".g:voom_tab_key.   " :<C-u>call Voom_ToTreeOrBodyWin()<CR>"
    let &cpo = cpo_
endfunc


func! Voom_BodyUnMap() "{{{2
" Remove Body local mappings. Must be called from Body.
    let cpo_ = &cpo | set cpo&vim
    exe "nunmap <buffer> ".g:voom_return_key
    exe "nunmap <buffer> ".g:voom_tab_key
    let &cpo = cpo_
endfunc


func! Voom_BodySelect() "{{{2
" Select current Body node. Show corresponding line in the Tree.
" Stay in the Tree if the node is already selected.
    let body = bufnr('')
    " Tree has been wiped out.
    if !has_key(s:voom_bodies, body)
        call Voom_BodyUnMap()
        return
    endif

    let wnr_ = winnr()
    let tree = s:voom_bodies[body].tree
    let blnr = line('.')
    let s:voom_bodies[body].blnr = blnr

    let bchangedtick = b:changedtick
    " Go to Tree. Outline will be updated on BufEnter.
    if Voom_ToTree(tree) < 0 | return | endif
    " Check for ticks.
    if s:voom_bodies[body].tick_!=bchangedtick
        exe bufwinnr(body).'wincmd w'
        call Voom_BodyCheckTicks(body)
        return
    endif

    " updateTree() sets = mark and may change snLn to a wrong value if outline was modified from Body.
    let snLn_ = s:voom_bodies[body].snLn
    " Compute new and correct snLn with updated outline.
    python voom.computeSnLn(int(vim.eval('l:body')), int(vim.eval('l:blnr')))
    let snLn = s:voom_bodies[body].snLn

    call Voom_TreeToLine(snLn)
    " Node has not changed. Stay in Tree.
    if snLn==snLn_
        return
    endif

    " Node has changed. Draw marks. Go back to Body
    setl ma | let ul_ = &ul | setl ul=-1
    keepj call setline(snLn_, ' '.getline(snLn_)[1:])
    keepj call setline(snLn, '='.getline(snLn)[1:])
    setl noma | let &ul = ul_

    let wnr_ = winnr('#')
    if winbufnr(wnr_)==body
        exe wnr_.'wincmd w'
    else
        exe bufwinnr(body).'wincmd w'
    endif
endfunc


func! Voom_BodyCheckTicks(body) "{{{2
" Current buffer is Body body. Check ticks assuming that outline is up to date,
" as after going to Body from Tree.
" note: 'abort' argument is not needed and would be counterproductive
    if bufnr('')!=a:body
        echoerr 'VOoM: wrong buffer'
        return -1
    endif
    " Wrong ticks, probably after :bun or :bd. Force outline update.
    if s:voom_bodies[a:body].tick_!=b:changedtick
        let tree = s:voom_bodies[a:body].tree
        if !exists("s:voom_trees") || !has_key(s:voom_trees, tree)
            echoerr "VOoM: internal error"
            return -1
        endif
        call Voom_BodyUpdateTree()
        call Voom_ErrorMsg('VOoM: wrong ticks for Body buffer '.a:body.'. Updated outline.')
        return -1
    endif
endfunc


func! Voom_BodyUpdateTree() "{{{2
" Current buffer is Body. Update outline and Tree.
    let body = bufnr('')
    if !has_key(s:voom_bodies, body)
        call Voom_ErrorMsg('VOoM: current buffer is not Body')
        return -1
    endif

    let tree = s:voom_bodies[body].tree

    " paranoia
    if !bufloaded(tree)
        call Voom_UnVoom(body,tree)
        echoerr "VOoM: Tree buffer" tree "is not loaded or does not exist. Cleanup has been performed."
        return -1
    endif

    """" update is not needed
    if s:voom_bodies[body].tick_==b:changedtick
        return
    endif

    """" do update
    call setbufvar(tree, '&ma', 1)
    let ul_=&ul | setl ul=-1
    try
        let l:ok = 0
        keepj python voom.updateTree(int(vim.eval('l:body')), int(vim.eval('l:tree')))
        if l:ok
            let s:voom_bodies[body].tick_ = b:changedtick
            let s:voom_bodies[body].tick  = b:changedtick
        endif
    finally
        " Why: &ul is global, but this causes 'undo list corrupt' error
        "let &ul=ul_
        call setbufvar(tree, '&ul', ul_)
        call setbufvar(tree, '&ma', 0)
    endtry
endfunc


"---Tree or Body------------------------------{{{1

func! Voom_EchoUNL() "{{{2
" Display UNL (Uniformed Node Locator) of current node.
" Copy UNL to register 'n'.
" Can be called from any buffer.
    let bnr = bufnr('')
    let lnum = line('.')
    if has_key(s:voom_trees, bnr)
        let [bufType, body, tree] = ['Tree', s:voom_trees[bnr], bnr]
        if Voom_BufLoaded(body) < 0 | return | endif
    elseif has_key(s:voom_bodies, bnr)
        let [bufType, body, tree] = ['Body', bnr, s:voom_bodies[bnr].tree]
        if Voom_BodyUpdateTree() < 0 | return | endif
    else
        call Voom_ErrorMsg("VOoM: current buffer is not a VOoM buffer")
        return
    endif
    python voom.voom_EchoUNL()
endfunc


func! Voom_Grep(input) "{{{2
" Seach Body for pattern(s). Show list of UNLs of nodes with matches.
" Input can have several patterns separated by boolean 'AND' and 'NOT'.
" Stop each search after 10,000 matches.
" Set search register to the first AND pattern.

    """ Process input first in case we are in Tree and want word under cursor.
    if a:input==''
        let input = expand('<cword>')
        let input = substitute(input, '\s\+$', '', '')
        if input=='' | return | endif
        let [pattsAND, pattsNOT] = [['\<'.input.'\>'], []]
    else
        let input = substitute(a:input, '\s\+$', '', '')
        if input=='' | return | endif
        let [pattsAND, pattsNOT] = Voom_GrepParseInput(input)
    endif

    """ Search must be done in Body buffer. Move to Body if in Tree.
    let bnr = bufnr('')
    if has_key(s:voom_trees, bnr)
        let body = s:voom_trees[bnr]
        let tree = bnr
        if Voom_BufLoaded(body) < 0 | return | endif
        if Voom_ToBody(body) < 0 | return | endif
        if Voom_BodyCheckTicks(body) < 0 | return | endif
    elseif has_key(s:voom_bodies, bnr)
        let body = bnr
        let tree = s:voom_bodies[bnr].tree
        " update outline
        if Voom_BodyUpdateTree() < 0 | return | endif
    else
        call Voom_ErrorMsg("VOoM: current buffer is not a VOoM buffer")
        return
    endif

    """ Search for each pattern with search().
    let lz_ = &lz | set lz
    let winsave_dict = winsaveview()
    let [matchesAND, matchesNOT] = [[], []]
    for patt in pattsAND
        let matches = Voom_GrepSearch(patt)
        if matches==[0]
            call Voom_WarningMsg('VOoM (Voomgrep): pattern not found: '.patt)
            call winrestview(winsave_dict)
            call winline()
            let &lz=lz_
            return
        endif
        call add(matchesAND, matches)
    endfor
    for patt in pattsNOT
        call add(matchesNOT, Voom_GrepSearch(patt))
    endfor
    call winrestview(winsave_dict)
    call winline()
    let &lz=lz_

    """ Highlight first AND pattern.
    " Problem: there is no search highlight after :noh
    " Consider: use matchadd() if several AND patterns
    if len(pattsAND)>0
        let @/ = pattsAND[0]
    endif

    """ Set and display quickfix list.
    " first line shows patterns and number of matches
    let line1 = ''
    for i in range(len(pattsAND))
        let L = matchesAND[i]
        let line1 = i==0 ? line1.pattsAND[i].' {' : line1.'AND '.pattsAND[i].' {'
        let line1 = L[-1]==0 ? line1. (len(L)-1) .' matches}  ' : line1.'>10,000 matches}  '
    endfor
    for i in range(len(pattsNOT))
        let L = matchesNOT[i]
        let line1 = line1.'NOT '.pattsNOT[i].' {'
        let line1 = L[-1]==0 ? line1. (len(L)-1) .' matches}  ' : line1.'>10,000 matches}  '
    endfor
    let line1 = 'Voomgrep '. substitute(line1,"'","''",'g')
    exe "call setqflist([{'text':'".line1."'}])"

    python voom.voom_Grep()
    botright copen
endfunc


func! Voom_GrepParseInput(input) "{{{2
" Input string is patterns separated by AND or NOT.
" There can be a leading NOT, but not leading AND.
" Segregate patterns into AND and NOT lists.
    let [pattsAND, pattsNOT] = [[], []]
    " split at AND
    let andParts = split(a:input, '\v\c\s+and\s+')
    let i = 1
    for part in andParts
        " split at NOT
        let notParts = split(part, '\v\c\s+not\s+')
        " check for leading NOT
        if i==1
            let i+=1
            let parts1 = split(notParts[0], '\v\c^\s*not\s+', 1)
            if len(parts1)>1
                call add(pattsNOT, parts1[1])
            else
                call add(pattsAND, notParts[0])
            endif
        else
            call add(pattsAND, notParts[0])
        endif
        if len(notParts)>1
            let pattsNOT+=notParts[1:]
        endif
    endfor
    return [pattsAND, pattsNOT]
endfunc


func! Voom_GrepSearch(pattern) "{{{2
" Seach buffer for pattern. Return [lnums of matching lines].
" Stop search after first 10000 matches.
    let matches = []
    " always search from start
    keepj normal! gg0
    " special effort needed to detect match at cursor
    if searchpos(a:pattern, 'nc')==[1,1]
        call add(matches,1)
    endif
    " do search
    let found = 1
    while found>0 && len(matches)<10000
        let found = search(a:pattern, 'W')
        call add(matches, found)
    endwhile
    " search was terminated after 10000 matches were found
    if matches[-1]!=0
        call add(matches,-1)
    endif
    return matches
endfunc


"---LOG BUFFER (Voomlog)----------------------{{{1
"
" Do "normal! G" to position cursor and scroll Log window.
" "call cursor('$',1)" does not scroll Log window.


func! Voom_LogInit() "{{{2
" Create and configure PyLog buffer or show existing one.
    let bnr_ = bufnr('')
    """ Log buffer exists, show it.
    if s:voom_logbnr
        if !bufloaded(s:voom_logbnr)
            python sys.stdout, sys.stderr = _voom_py_sys_stdout, _voom_py_sys_stderr
            python if 'pydoc' in sys.modules: del sys.modules['pydoc']
            if bufexists(s:voom_logbnr)
                exe 'au! VoomLog * <buffer='.s:voom_logbnr.'>'
                exe 'bwipeout '.s:voom_logbnr
            endif
            let bnr = s:voom_logbnr
            let s:voom_logbnr = 0
            echoerr "VOoM: PyLog buffer" bnr "was not shut down properly. Cleanup has been performed. Execute the command :Voomlog again."
            return
        endif
        if bufwinnr(s:voom_logbnr) < 0
            call Voom_ToLogWin()
            silent exe 'b '.s:voom_logbnr
            keepj normal! G
            exe bufwinnr(bnr_).'wincmd w'
        endif
        return
    endif

    """ Create and configure PyLog buffer.
    if bufexists('__PyLog__') > 0
        call Voom_ErrorMsg('VOoM: there is already a buffer named __PyLog__')
        return
    endif
    call Voom_ToLogWin()
    silent edit __PyLog__
    call Voom_LogConfig()
    """ Go back.
    exe bufwinnr(bnr_).'wincmd w'
endfunc


func! Voom_LogConfig() "{{{2
" Configure current buffer as PyLog. Redirect Python stdout and stderr to it.
" NOTE: the caller must check if PyLog already exists.
    let s:voom_logbnr = bufnr('')
    augroup VoomLog
        au! * <buffer>
        au BufUnload <buffer> nested call Voom_LogBufUnload()
    augroup END
    setl cul nocuc list wrap
    setl bufhidden=wipe
    setl ft=voomlog
    setl noro ma ff=unix
    setl nobuflisted buftype=nofile noswapfile
    call Voom_LogSyntax()
python << EOF
_voom_py_sys_stdout, _voom_py_sys_stderr = sys.stdout, sys.stderr
sys.stdout = sys.stderr = voom.LogBufferClass()
if 'pydoc' in sys.modules: del sys.modules['pydoc']
EOF
endfunc


func! Voom_LogBufUnload() "{{{2
    if !s:voom_logbnr || expand("<abuf>")!=s:voom_logbnr
        echoerr 'VOoM: internal error'
        return
    endif
    python sys.stdout, sys.stderr = _voom_py_sys_stdout, _voom_py_sys_stderr
    python if 'pydoc' in sys.modules: del sys.modules['pydoc']
    exe 'au! VoomLog * <buffer='.s:voom_logbnr.'>'
    exe 'bwipeout '.s:voom_logbnr
    let s:voom_logbnr = 0
endfunc


func! Voom_LogSyntax() "{{{2
" Log buffer syntax highlighting.

    " Python tracebacks
    syn match Error /^Traceback (most recent call last):/
    syn match Error /^\u\h*Error/
    syn match Error /^vim\.error/
    syn region WarningMsg start="^Traceback (most recent call last):" end="\%(^\u\h*Error.*\)\|\%(^\s*$\)\|\%(^vim\.error\)" contains=Error keepend

    "Vim exceptions
    syn match Error /^Vim.*:E\d\+:.*/

    " VOoM messages
    syn match Error /^ERROR: .*/
    syn match Error /^EXCEPTION: .*/
    syn match PreProc /^---end of \w\+ script.*---$/

    " -> UNL separator
    syn match Title / -> /

endfunc


func! Voom_LogScroll() "{{{2
" Scroll windows with the __PyLog__ buffer.
" All tabs are searched. Only the first found Log window in each tab is scrolled.
" Uses noautocmd when jumping between tabs and windows.
" Note: don't use Python here: an error will result in recursive loop.

    " can't go to other windows when in Ex mode (after 'Q' or 'gQ')
    if mode()=='c' | return | endif
    " This should never happen.
    if !s:voom_logbnr || !bufloaded(s:voom_logbnr)
        echoerr "VOoM: internal error"
        return
    endif

    let lz_=&lz | set lz
    let log_found = 0
    let [tnr_, wnr_, bnr_] = [tabpagenr(), winnr(), bufnr('')]
    " search among visible buffers in all tabs
    for tnr in range(1, tabpagenr('$'))
        if index(tabpagebuflist(tnr), s:voom_logbnr) > -1
            let log_found = 1
            if tabpagenr() != tnr
                exe 'noautocmd tabnext '.tnr
            endif
            let [wnr__, wnr__p] = [winnr(), winnr('#')]
            exe 'noautocmd '. bufwinnr(s:voom_logbnr).'wincmd w'
            keepj normal! G
            " restore tab's current and previous window numbers
            if wnr__p
                exe 'noautocmd '.wnr__p.'wincmd w'
            endif
            exe 'noautocmd '.wnr__.'wincmd w'
        endif
    endfor
    " At least one Log window was found and scrolled. Return to original tab and window.
    if log_found==1
        if tabpagenr() != tnr_
            exe 'noautocmd tabnext '.tnr_
            exe 'noautocmd '.wnr_.'wincmd w'
        endif
    " Log window was not found. Create it.
    else
        call Voom_ToLogWin()
        exe 'b '.s:voom_logbnr
        keepj normal! G
        exe 'tabnext '.tnr_
        exe bufwinnr(bnr_).'wincmd w'
    endif
    let &lz=lz_
endfunc


func! Voom_LogSessionLoad() "{{{2
" Activate PyLog when loading Vim session created with :mksession.
    if !exists('g:SessionLoad') || &modified || line('$')>1 || getline(1)!='' || (exists('s:voom_logbnr') && s:voom_logbnr)
        return
    endif
    call Voom_LogConfig()
endfunc


"---EXECUTE SCRIPT (Voomexec)-----------------{{{1

func! Voom_GetVoomRange(lnum, withSubnodes) "{{{2
    let bnr = bufnr('')
    if has_key(s:voom_trees, bnr)
        let [bufType, body, tree] = ['Tree', s:voom_trees[bnr], bnr]
        if Voom_BufLoaded(body) < 0 | return ['Tree',-1,-1,-1] | endif
    elseif has_key(s:voom_bodies, bnr)
        let [bufType, body, tree] = ['Body', bnr, s:voom_bodies[bnr].tree]
        if Voom_BodyUpdateTree() < 0 | return ['Body',-1,-1,-1] | endif
    else
        return ['None',0,0,0]
    endif
    if a:withSubnodes
        python voom.voom_GetVoomRange(withSubnodes=1)
    else
        python voom.voom_GetVoomRange()
    return [bufType, body, l:bln1, l:bln2]
" Return [bufType, body, bln1, bln2] for node at line lnum of the current
" VOoM buffer (Tree or Body).
" bln1, bln2: VOoM node's first and last Body lnums. Current node only if
" a:withSubnodes==0. Include all subnodes if a:withSubnodes==1.
" Return [bufType,-1,-1,-1] in case of an error (unloaded Body, etc.)
" Return ['None',0,0,0] for a non-VOoM buffer.
" This is for use by external scripts:
"       let [bufType, body, bln1, bln2] = Voom_GetVoomRange(line('.'),0)
"       let bodyLines = getbufline(body,bln1,bln2)
endfunc


func! Voom_GetBuffRange(ln1, ln2) "{{{2
    let bnr = bufnr('')
    if has_key(s:voom_trees, bnr)
        let [bufType, body, tree] = ['Tree', s:voom_trees[bnr], bnr]
        if Voom_BufLoaded(body) < 0 | return ['Tree',-1,-1,-1] | endif
        python voom.voom_GetBuffRange()
        return [bufType, body, l:bln1, l:bln2]
    elseif has_key(s:voom_bodies, bnr)
        return ['Body',bnr,a:ln1,a:ln2]
    else
        return ['None',bnr,a:ln1,a:ln2]
    endif
" Return [bufType, body, bln1, bln2] for line range lnum1,lnum2.
" If current buffer is a Tree: bln1, bln2 are start and end lnums of the
" corresponding Body line range; 'body' is Body's buffer number.
" Return ['Tree',-1,-1,-1] in case of an error (unloaded Body.)
" If current buffer is not a Tree: bln1, bln2 are lnum1, lnum2; 'body' is the
" current buffer number.
" NOTE: Outline is not updated if the current buffer is Body.
endfunc


func! Voom_GetExecRange(lnum) "{{{2
" Return line range info for Voomexec: [bufType, bufnr, start lnum, end lnum]
    let bnr = bufnr('')
    let status = Voom_FoldStatus(a:lnum)
    if status=='hidden'
        call Voom_ErrorMsg('VOoM: line is hidden in fold')
        return ['',-1,-1,-1] 
    endif
    " Tree buffer: get start/end of Body node and subnodes.
    if has_key(s:voom_trees, bnr)
        let [bufType, body, tree] = ['Tree', s:voom_trees[bnr], bnr]
        if Voom_BufLoaded(body) < 0 | return ['',-1,-1,-1] | endif
        python voom.voom_GetVoomRange(withSubnodes=1)
        return [bufType, body, l:bln1, l:bln2]
    endif
    " Any other buffer: get start/end of the current fold and subfolds.
    if &fdm !=# 'marker'
        call Voom_ErrorMsg('VOoM: ''foldmethod'' must be "marker"')
        return ['',-1,-1,-1]
    endif
    if status=='nofold'
        call Voom_ErrorMsg('VOoM: no fold at cursor')
        return ['',-1,-1,-1]
    elseif status=='folded'
        return ['', bnr, foldclosed(a:lnum), foldclosedend(a:lnum)]
    elseif status=='notfolded'
        let lz_ = &lz | set lz
        let winsave_dict = winsaveview()
        normal! zc
        let foldStart = foldclosed(a:lnum)
        let foldEnd   = foldclosedend(a:lnum)
        normal! zo
        call winrestview(winsave_dict)
        let &lz=lz_
        return ['', bnr, foldStart, foldEnd]
    endif
endfunc


func! Voom_Exec(qargs) "{{{2
" Execute text from the current node (Tree or Body, include subnodes) or fold
" (non-VOoM buffer, include subfolds) as a script.
" If argument is 'vim' or 'py'/'python': execute as Vim or Python script.
" Otherwise execute according to filetype.

    " If current buffer is a Tree: use Body filetype, encodings, etc.
    let bnr = bufnr('')
    if has_key(s:voom_trees, bnr)
        let bnr = s:voom_trees[bnr]
    endif
    let FT = getbufvar(bnr, '&ft')

    if a:qargs==#'vim'
        let scriptType = 'vim'
    elseif a:qargs==#'py' || a:qargs==#'python'
        let scriptType = 'python'
    elseif a:qargs!=''
        call Voom_ErrorMsg('VOoM: unsupported script type: "'.a:qargs.'"')
        return
    elseif FT==#'vim'
        let scriptType = 'vim'
    elseif FT==#'python'
        let scriptType = 'python'
    else
        call Voom_ErrorMsg('VOoM: unsupported script type: "'.FT.'"')
        return
    endif

    " Get script lines.
    let [bufType, body, bln1, bln2] = Voom_GetExecRange(line('.'))
    if body<1 | return | endif

    " Execute Vim script: Copy list of lines to register and execute it.
    " Problem: Python errors do not terminate script and Python tracebacks are
    " not printed. They are printed to the PyLog if it's enabled. Probably
    " caused by 'catch', but without it foldtext is temporarily messed up in
    " all windows after any error.
    if scriptType==#'vim'
        let lines = getbufline(body, bln1, bln2)
        if lines==[] | return | endif
        let reg_z = getreg('z')
        let reg_z_mode = getregtype('z')
        let script = join(lines, "\n") . "\n"
        call setreg('z', script, "l")
        try
            call s:Voom_ExecVim()
        catch
            call Voom_ErrorMsg(v:exception)
        finally
            call setreg('z', reg_z, reg_z_mode)
            echo '---end of Vim script ('.bln1.'-'.bln2.')---'
        endtry
    " Execute Python script.
    elseif scriptType==#'python'
        " do not change, see ./voom/voom.py#id_20101214100357
        if s:voom_logbnr
            try
                python voom.voom_Exec()
            catch
                python print vim.eval('v:exception')
            endtry
        else
            python voom.voom_Exec()
        endif
    endif
endfunc


func! s:Voom_ExecVim() "{{{2
    @z
endfunc


"---execute user command----------------------{{{1
if exists('g:voom_user_command')
    execute g:voom_user_command
endif


" modelines {{{1
" vim:fdm=marker:fdl=0:
" vim:foldtext=getline(v\:foldstart).'...'.(v\:foldend-v\:foldstart):
