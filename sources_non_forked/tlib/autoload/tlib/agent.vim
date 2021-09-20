" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    362

" :filedoc:
" Various agents for use as key handlers in tlib#input#List()

" Number of items to move when pressing <c-up/down> in the input list window.
TLet g:tlib_scroll_lines = 10


" General {{{1

function! tlib#agent#Exit(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    if a:world.key_mode ==# 'default'
        call a:world.CloseScratch()
        let a:world.state = 'exit empty escape'
        let a:world.list = []
        " let a:world.base = []
        call a:world.ResetSelected()
    else
        let a:world.key_mode = 'default'
        let a:world.state = 'redisplay'
    endif
    return a:world
endf


function! tlib#agent#CopyItems(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let @* = join(a:selected, "\n")
    let a:world.state = 'redisplay'
    return a:world
endf



" InputList related {{{1

function! tlib#agent#PageUp(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let a:world.offset -= (winheight(0) / 2)
    let a:world.state = 'scroll'
    return a:world
endf


function! tlib#agent#PageDown(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let a:world.offset += (winheight(0) / 2)
    let a:world.state = 'scroll'
    return a:world
endf


function! tlib#agent#Home(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let a:world.prefidx = 1
    let a:world.state = 'redisplay'
    return a:world
endf


function! tlib#agent#End(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let a:world.prefidx = len(a:world.list)
    let a:world.state = 'redisplay'
    return a:world
endf


function! tlib#agent#Up(world, selected, ...) "{{{3
    TVarArg ['lines', 1]
    Tlibtrace 'tlib', a:selected, lines
    let a:world.idx = ''
    if a:world.prefidx > lines
        let a:world.prefidx -= lines
    else
        let a:world.prefidx = len(a:world.list)
    endif
    let a:world.state = 'redisplay'
    return a:world
endf


function! tlib#agent#Down(world, selected, ...) "{{{3
    TVarArg ['lines', 1]
    Tlibtrace 'tlib', a:selected, lines
    let a:world.idx = ''
    if a:world.prefidx <= (len(a:world.list) - lines)
        let a:world.prefidx += lines
    else
        let a:world.prefidx = 1
    endif
    let a:world.state = 'redisplay'
    return a:world
endf


function! tlib#agent#UpN(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    return tlib#agent#Up(a:world, a:selected, g:tlib_scroll_lines)
endf


function! tlib#agent#DownN(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    return tlib#agent#Down(a:world, a:selected, g:tlib_scroll_lines)
endf


function! tlib#agent#ShiftLeft(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let a:world.offset_horizontal -= (winwidth(0) / 2)
    if a:world.offset_horizontal < 0
        let a:world.offset_horizontal = 0
    endif
    let a:world.state = 'display shift'
    return a:world
endf


function! tlib#agent#ShiftRight(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let a:world.offset_horizontal += (winwidth(0) / 2)
    let a:world.state = 'display shift'
    return a:world
endf


function! tlib#agent#Reset(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let a:world.state = 'reset'
    return a:world
endf


function! tlib#agent#ToggleRestrictView(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    if empty(a:world.filtered_items)
        return tlib#agent#RestrictView(a:world, a:selected)
    else
        return tlib#agent#UnrestrictView(a:world, a:selected)
    endif
endf


function! tlib#agent#RestrictView(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let filtered_items = map(copy(a:selected), 'index(a:world.base, v:val) + 1')
    Tlibtrace 'tlib', 1, filtered_items
    let filtered_items = filter(filtered_items, 'v:val > 0')
    Tlibtrace 'tlib', 2, filtered_items
    if !empty(filtered_items)
        let a:world.filtered_items = filtered_items
    endif
    let a:world.state = 'display'
    return a:world
endf


function! tlib#agent#UnrestrictView(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let a:world.filtered_items = []
    let a:world.state = 'display'
    return a:world
endf


function! tlib#agent#Input(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let flt0 = a:world.CleanFilter(a:world.filter[0][0])
    let flt1 = input('Filter: ', flt0)
    echo
    if flt1 != flt0
        if empty(flt1)
            call getchar(0)
        else
            call a:world.SetFrontFilter(flt1)
        endif
    endif
    let a:world.state = 'display'
    return a:world
endf


" Suspend (see |tlib#agent#Suspend|) the input loop and jump back to the 
" original position in the parent window.
function! tlib#agent#SuspendToParentWindow(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let world = a:world
    let wid = world.win_id
    Tlibtrace 'tlib', wid
    if wid != -1
        let world = tlib#agent#Suspend(world, a:selected)
        if world.state =~# '\<suspend\>'
            call world.SwitchWindow('win')
            " let pos = world.cursor
            " Tlibtrace 'tlib', pos
            " if !empty(pos)
            "     call setpos('.', pos)
            " endif
            return world
        endif
    endif
    let world.state = 'redisplay'
    return world
endf


" Suspend lets you temporarily leave the input loop of 
" |tlib#input#List|. You can resume editing the list by pressing <c-z>, 
" <m-z>. <space>, <c-LeftMouse> or <MiddleMouse> in the suspended window.
" <cr> and <LeftMouse> will immediatly select the item under the cursor.
" < will select the item but the window will remain opened.
function! tlib#agent#Suspend(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    if a:world.allow_suspend
        " TAssert IsNotEmpty(a:world.scratch)
        " TLogDBG bufnr('%')
        let br = tlib#buffer#Set(a:world.scratch)
        Tlibtrace 'tlib', br, a:world.bufnr, a:world.scratch
        if bufnr('%') != a:world.scratch
            echohl WarningMsg
            echom "tlib#agent#Suspend: Internal error: Not a scratch buffer:" bufname('%')
            echohl NONE
        endif
        Tlibtrace 'tlib', bufnr('%'), bufname('%'), a:world.scratch
        call tlib#autocmdgroup#Init()
        exec 'autocmd TLib BufEnter <buffer='. a:world.scratch .'> call tlib#input#Resume("world", 0, '. a:world.scratch .')'
        let b:tlib_world = a:world
        exec br
        let a:world.state = 'exit suspend'
    else
        echom 'Suspend disabled'
        let a:world.state = 'redisplay'
    endif
    return a:world
endf


function! tlib#agent#Help(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let a:world.state = 'help'
    return a:world
endf


function! tlib#agent#OR(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    if !empty(a:world.filter[0])
        call insert(a:world.filter[0], '')
    endif
    let a:world.state = 'display'
    return a:world
endf


function! tlib#agent#AND(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    if !empty(a:world.filter[0])
        call insert(a:world.filter, [''])
    endif
    let a:world.state = 'display'
    return a:world
endf


function! tlib#agent#ReduceFilter(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    call a:world.ReduceFilter()
    let a:world.offset = 1
    let a:world.state = 'display'
    return a:world
endf


function! tlib#agent#PopFilter(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    call a:world.PopFilter()
    let a:world.offset = 1
    let a:world.state = 'display'
    return a:world
endf


function! tlib#agent#Debug(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    " echo string(world.state)
    echo string(a:world.filter)
    echo string(a:world.idx)
    echo string(a:world.prefidx)
    echo string(a:world.sel_idx)
    call getchar()
    let a:world.state = 'display'
    return a:world
endf


function! tlib#agent#Select(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    call a:world.SelectItem('toggle', a:world.prefidx)
    " let a:world.state = 'display keepcursor'
    let a:world.state = 'redisplay'
    return a:world
endf


function! tlib#agent#SelectUp(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    call a:world.SelectItem('toggle', a:world.prefidx)
    if a:world.prefidx > 1
        let a:world.prefidx -= 1
    endif
    let a:world.state = 'redisplay'
    return a:world
endf


function! tlib#agent#SelectDown(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    call a:world.SelectItem('toggle', a:world.prefidx)
    if a:world.prefidx < len(a:world.list)
        let a:world.prefidx += 1
    endif
    let a:world.state = 'redisplay'
    return a:world
endf


function! tlib#agent#SelectAll(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let listrange = range(1, len(a:world.list))
    let mode = empty(filter(copy(listrange), 'index(a:world.sel_idx, a:world.GetBaseIdx(v:val)) == -1'))
                \ ? 'toggle' : 'set'
    for i in listrange
        call a:world.SelectItem(mode, i)
    endfor
    let a:world.state = 'display keepcursor'
    return a:world
endf


function! tlib#agent#ToggleStickyList(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let a:world.sticky = !a:world.sticky
    let a:world.state = 'display keepcursor'
    return a:world
endf



" EditList related {{{1

function! tlib#agent#EditItem(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let lidx = a:world.prefidx
    Tlibtrace 'tlib', lidx
    let bidx = a:world.GetBaseIdx(lidx)
    Tlibtrace 'tlib', bidx
    let item = a:world.GetBaseItem(bidx)
    let item = input(lidx .'@'. bidx .': ', item)
    if item != ''
        call a:world.SetBaseItem(bidx, item)
    endif
    let a:world.state = 'display'
    return a:world
endf


" Insert a new item below the current one.
function! tlib#agent#NewItem(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let basepi = a:world.GetBaseIdx(a:world.prefidx)
    let item = input('New item: ')
    call insert(a:world.base, item, basepi)
    let a:world.state = 'reset'
    return a:world
endf


function! tlib#agent#DeleteItems(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let remove = copy(a:world.sel_idx)
    let basepi = a:world.GetBaseIdx(a:world.prefidx)
    if index(remove, basepi) == -1
        call add(remove, basepi)
    endif
    " call map(remove, 'a:world.GetBaseIdx(v:val)')
    for idx in reverse(sort(remove))
        call remove(a:world.base, idx - 1)
    endfor
    let a:world.state = 'display'
    call a:world.ResetSelected()
    " let a:world.state = 'reset'
    return a:world
endf


function! tlib#agent#Cut(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let world = tlib#agent#Copy(a:world, a:selected)
    return tlib#agent#DeleteItems(world, a:selected)
endf


function! tlib#agent#Copy(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let a:world.clipboard = []
    let bidxs = copy(a:world.sel_idx)
    call add(bidxs, a:world.GetBaseIdx(a:world.prefidx))
    for bidx in sort(bidxs)
        call add(a:world.clipboard, a:world.GetBaseItem(bidx))
    endfor
    let a:world.state = 'redisplay'
    return a:world
endf


function! tlib#agent#Paste(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    if has_key(a:world, 'clipboard')
        for e in reverse(copy(a:world.clipboard))
            call insert(a:world.base, e, a:world.prefidx)
        endfor
    endif
    let a:world.state = 'display'
    call a:world.ResetSelected()
    return a:world
endf


function! tlib#agent#EditReturnValue(world, rv) "{{{3
    Tlibtrace 'tlib', a:rv
    return [a:world.state !~ '\<exit\>', a:world.base]
endf



" Files related {{{1

function! tlib#agent#ViewFile(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    if !empty(a:selected)
        let back = a:world.SwitchWindow('win')
        Tlibtrace 'tlib', back
        for filename in a:selected
            call tlib#file#Edit(filename)
        endfor
        call a:world.SetOrigin(1)
        silent! exec back
        let a:world.state = 'display'
    endif
    return a:world
endf


function! tlib#agent#EditFile(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    return tlib#agent#Exit(tlib#agent#ViewFile(a:world, a:selected), a:selected)
endf


function! tlib#agent#EditFileInSplit(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    call a:world.CloseScratch()
    call tlib#file#With('split', 'sbuffer', a:selected, a:world, 1)
    call a:world.SetOrigin(1)
    return tlib#agent#Exit(a:world, a:selected)
endf


function! tlib#agent#EditFileInVSplit(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    call a:world.CloseScratch()
    let winpos = tlib#fixes#Winpos()
    call tlib#file#With('vertical split', 'vertical sbuffer', a:selected, a:world, 1)
    if !empty(winpos)
        exec winpos
    endif
    call a:world.SetOrigin(1)
    return tlib#agent#Exit(a:world, a:selected)
endf


function! tlib#agent#EditFileInTab(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    call a:world.CloseScratch()
    call tlib#file#With('tabedit', 'tab sbuffer', a:selected, a:world, 1)
    call a:world.SetOrigin(1)
    return tlib#agent#Exit(a:world, a:selected)
endf


function! tlib#agent#EditFileInWindow(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    call a:world.CloseScratch()
    call tlib#file#With('hide edit', 'hide buffer', a:selected, a:world, 1)
    call a:world.SetOrigin(1)
    return tlib#agent#Exit(a:world, a:selected)
endf


function! tlib#agent#ToggleScrollbind(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let a:world.scrollbind = get(a:world, 'scrollbind') ? 0 : 1
    let a:world.state = 'redisplay'
    return a:world
endf


function! tlib#agent#ShowInfo(world, selected)
    Tlibtrace 'tlib', a:selected
    let lines = []
    for f in a:selected
        if filereadable(f)
            let desc = [getfperm(f), strftime('%c', getftime(f)),  getfsize(f) .' bytes', getftype(f)]
            call add(lines, fnamemodify(f, ':p'))
            call add(lines, '  '. join(desc, '; '))
        endif
    endfor
    let a:world.temp_lines = lines
    let a:world.state = 'printlines'
    return a:world
endf



" Buffer related {{{1

function! tlib#agent#ViewBufferInWindow(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    if !empty(a:selected)
        let back = a:world.SwitchWindow('win')
        Tlibtrace 'tlib', back
        for bufname in a:selected
            let cmd = &modified && !&hidden ? 'sbuffer' : 'buffer'
            exec cmd fnameescape(bufname)
        endfor
        " exec back
    endif
    return tlib#agent#Exit(a:world, a:selected)
endf


function! tlib#agent#PreviewLine(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let l = a:selected[0]
    " let ww = winnr()
    let wid = tlib#win#GetID()
    call tlib#agent#SuspendToParentWindow(a:world, a:selected)
    call tlib#buffer#ViewLine(l, 1)
    call tlib#win#GotoID(wid)
    " exec ww .'wincmd w'
    let a:world.state = 'redisplay'
    return a:world
endf


" If not called from the scratch, we assume/guess that we don't have to 
" suspend the input-evaluation loop.
function! tlib#agent#GotoLine(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    if !empty(a:selected)
        let l = a:selected[0]
        if a:world.win_id != tlib#win#GetID()
            let world = tlib#agent#Suspend(a:world, a:selected)
            call tlib#win#GotoID(a:world.win_id)
        endif
        call tlib#buffer#ViewLine(l, 1)
        
    endif
    return a:world
endf


function! tlib#agent#DoAtLine(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    if !empty(a:selected)
        let cmd = input('Command: ', '', 'command')
        if !empty(cmd)
            call a:world.SwitchWindow('win')
            " let pos = getpos('.')
            let view = winsaveview()
            for l in a:selected
                call tlib#buffer#ViewLine(l, '')
                exec cmd
            endfor
            " call setpos('.', pos)
            call winrestview(view)
        endif
    endif
    call a:world.ResetSelected()
    let a:world.state = 'exit'
    return a:world
endf


function! tlib#agent#Wildcard(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    if !empty(a:world.filter[0])
        let rx_type = a:world.matcher.FilterRxPrefix()
        let flt0 = a:world.CleanFilter(a:world.filter[0][0])
        if rx_type == '\V'
            let flt0 .= '\.\{-}'
        else
            let flt0 .= '.\{-}'
        endif
        call a:world.SetFrontFilter(flt0)
    endif
    let a:world.state = 'redisplay'
    return a:world
endf


function! tlib#agent#Null(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let a:world.state = 'redisplay'
    return a:world
endf


function! tlib#agent#ExecAgentByName(world, selected) "{{{3
    Tlibtrace 'tlib', a:selected
    let s:agent_names_world = a:world
    let agent_names = {'Help': 'tlib#agent#Help'}
    for def in values(a:world.key_map[a:world.key_mode])
        if has_key(def, 'help') && !empty(def.help) && has_key(def, 'agent') && !empty(def.agent)
            let agent_names[def.help] = def.agent
        endif
    endfor
    let s:agent_names = sort(keys(agent_names))
    let command = input('Command: ', '', 'customlist,tlib#agent#CompleteAgentNames')
    Tlibtrace 'tlib', command
    if !has_key(agent_names, command)
        Tlibtrace 'tlib', command
        silent! let matches = filter(keys(agent_names), 'v:val =~ command')
        Tlibtrace 'tlib', matches
        if len(matches) == 1
            let command = matches[0]
        endif
    endif
    if has_key(agent_names, command)
        let agent = agent_names[command]
        return call(agent, [a:world, a:selected])
    else
        if !empty(command)
            echohl WarningMsg
            echom "Unknown command:" command
            echohl NONE
            sleep 1
        endif
        let a:world.state = 'display'
        return a:world
    endif
endf


function! tlib#agent#CompleteAgentNames(ArgLead, CmdLine, CursorPos)
    let arglead = tolower(a:Arglead)
    return filter(copy(s:agent_names), 'stridx(tolower(v:val), arglead) != -1')
endf


function! tlib#agent#Complete(world, selected) abort "{{{3
    Tlibtrace 'tlib', a:selected
    let rxprefix = a:world.matcher.FilterRxPrefix()
    let flt = a:world.filter[0][0]
    Tlibtrace 'tlib', flt
    let fltrx = rxprefix . flt . '\m[^[:space:][:cntrl:][:punct:]<>*+?&~{}()\[\]\\/]\+'
    let fltrx0 = '\m^' . fltrx
    Tlibtrace 'tlib', fltrx, fltrx0
    let words = {}
    for item in a:world.list
        let parts = split(item, '\ze'. fltrx)
        Tlibtrace 'tlib', item, parts
        for part in parts
            let word = matchstr(part, fltrx0)
            Tlibtrace 'tlib', part, word
            if !empty(word)
                let words[word] = 1
            endif
        endfor
    endfor
    Tlibtrace 'tlib', keys(words)
    let completions = keys(words)
    " let completions = filter(keys(words), 'matchstr(v:val, fltrx0)')
    let completions = sort(completions, 's:SortCompletions')
    let completions = tlib#list#Uniq(completions)
    Tlibtrace 'tlib', 0, completions
    while len(completions) > 1
        let nchar = strwidth(completions[0]) - 1
        let completions = map(completions, 'tlib#string#Strcharpart(v:val, 0, nchar)')
        Tlibtrace 'tlib', 'reduce', completions
        let completions = tlib#list#Uniq(completions)
        Tlibtrace 'tlib', 'unique', len(completions), completions
    endwh
    Tlibtrace 'tlib', 9, completions
    if empty(completions)
        let a:world.state = 'redisplay update'
    else
        let a:world.filter[0][0] = completions[0]
        let a:world.state = 'display update'
    endif
    return a:world
endf


function! s:SortCompletions(a, b) abort "{{{3
    let i1 = strwidth(a:a)
    let i2 = strwidth(a:b)
    return i2 - i1
endf

