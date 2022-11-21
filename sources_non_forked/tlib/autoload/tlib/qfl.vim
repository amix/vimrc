" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     https://github.com/tomtom
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Last Change: 2018-02-08
" @Revision:    69

" :nodoc:
TLet g:tlib#qfl#world = {
            \ 'type': 'mi',
            \ 'query': 'Select entry',
            \ 'pick_last_item': 0,
            \ 'resize_vertical': 0,
            \ 'resize': 20,
            \ 'scratch': '__TLibQFL__',
            \ 'tlib_UseInputListScratch': 'call tlib#qfl#InitListBuffer(world)',
            \ 'key_handlers': [
                \ {'key':  5, 'agent': 'tlib#qfl#AgentWithSelected', 'key_name': '<c-e>', 'help': 'Run a command on selected lines'},
                \ {'key': 16, 'agent': 'tlib#qfl#AgentPreviewQFE',   'key_name': '<c-p>', 'help': 'Preview'},
                \ {'key': 60, 'agent': 'tlib#qfl#AgentGotoQFE',      'key_name': '<',     'help': 'Jump (don''t close the list)'},
                \ {'key': 19, 'agent': 'tlib#qfl#AgentSplitBuffer',  'key_name': '<c-s>', 'help': 'Show in split buffer'},
                \ {'key': 20, 'agent': 'tlib#qfl#AgentTabBuffer',    'key_name': '<c-t>', 'help': 'Show in tab'},
                \ {'key': 22, 'agent': 'tlib#qfl#AgentVSplitBuffer', 'key_name': '<c-v>', 'help': 'Show in vsplit buffer'},
                \ {'key': 12, 'agent': 'tlib#qfl#AgentEditLine',     'key_name': '<c-l>', 'help': 'Edit selected line(s)'},
                \ {'key': "\<c-insert>", 'agent': 'tlib#qfl#SetFollowCursor', 'key_name': '<c-ins>', 'help': 'Toggle trace cursor'},
            \ ],
            \ 'return_agent': 'tlib#qfl#AgentEditQFE',
            \ }


function! tlib#qfl#FormatQFLE(qfe) dict abort "{{{3
    let filename = tlib#qfl#QfeFilename(a:qfe)
    let short_filename = get(self, 'qfl_short_filename', '')
    if short_filename ==# 'basename'
        let filename = matchstr(filename, '[^\\/]\+$')
    elseif !empty(short_filename)
        let filename = pathshorten(filename)
    endif
    return printf("%s|%d| %s", filename, a:qfe.lnum, get(a:qfe, "text"))
endf


function! tlib#qfl#QfeFilename(qfe) abort "{{{3
    let filename = get(a:qfe, 'filename')
    if empty(filename)
        let filename = bufname(get(a:qfe, 'bufnr'))
    endif
    return filename
endf


function! tlib#qfl#InitListBuffer(world) "{{{3
    let set_syntax = get(a:world, 'set_syntax', 'tlib#qfl#SetSyntax')
    call call(set_syntax, [], a:world)
    if has('balloon_eval')
        setlocal ballooneval balloonexpr=tlib#qfl#Balloon()
    endif
endf


function! tlib#qfl#SetSyntax() dict abort "{{{3
    let syntax = get(self, 'qfl_list_syntax', '')
    let nextgroup = get(self, 'qfl_list_syntax_nextgroup', '')
    " TLogVAR syntax, nextgroup
    if !empty(syntax)
        exec printf('runtime syntax/%s.vim', syntax)
    endif
    syn match TTagedFilesFilename /\%(\f\+\| \)\+\ze|\d\+| / nextgroup=TTagedFilesLNum
    if !empty(nextgroup)
        exec 'syn match TTagedFilesLNum /|\d\+|\s\+/ nextgroup='. nextgroup
    else
        syn match TTagedFilesLNum /|\d\+|/
    endif
    hi def link TTagedFilesFilename Directory
    hi def link TTagedFilesLNum LineNr
endf


function! tlib#qfl#Balloon() "{{{3
    let world = getbufvar(v:beval_bufnr, 'tlibDisplayListWorld')
    let current = max([1, world.offset]) + v:beval_lnum - 1
    if current > len(world.table)
        let current = len(world.table)
    endif
    let baseidx = world.GetBaseIdx0(current)
    " TLogVAR world.offset, v:beval_lnum, current, baseidx
    let item = world.data[baseidx]
    let bufnr = get(item, 'bufnr', 0)
    let bufname = get(item, 'filename', '')
    if bufnr == 0 && !empty(bufname)
        let bufnr = bufnr(bufname)
    endif
    if empty(bufname) && bufnr > 0
        let bufname = bufname(bufnr)
    endif
    " TLogVAR item
    if bufnr == 0
        return ''
    else
        let lines = [printf("%d#%d: %s", bufnr, item.lnum, bufname)]
        if has('balloon_multiline')
            let desc = {'nr': 'Error number', 'type': 'Error type', 'text': ''}
            for key in ['nr', 'type', 'text']
                if has_key(item, key) && !empty(item[key])
                    let keydesc = get(desc, key, key)
                    if empty(keydesc)
                        let text = item[key]
                    else
                        let text = printf("%s: %s", key, item[key])
                    endif
                    call add(lines, text)
                endif
            endfor
        endif
        return join(lines, "\n")
    endif
    " v:beval_bufnr	number of the buffer in which balloon is going to show
    " v:beval_winnr	number of the window
    " v:beval_lnum	line number
    " v:beval_col	column number (byte index)
    " v:beval_text	word under or after the mouse pointer
endf


function! tlib#qfl#AgentEditQFE(world, selected, ...) "{{{3
    TVarArg ['cmd_edit', ''], ['cmd_buffer', ''], ['set_origin', 1]
    " TVarArg ['cmd_edit', 'edit'], ['cmd_buffer', 'buffer']
    " TLogVAR a:selected
    if empty(a:selected)
        " call a:world.RestoreOrigin()
        " call a:world.ResetSelected()
    else
        call a:world.RestoreOrigin()
        for idx in a:selected
            let idx -= 1
            " TLogVAR idx
            if idx >= 0
                " TLogVAR a:world.data
                " call tlog#Debug(string(map(copy(a:world.data), 'v:val.bufnr')))
                " TLogVAR idx, a:world.data[idx]
                let qfe = a:world.data[idx]
                " let back = a:world.SwitchWindow('win')
                " TLogVAR cmd_edit, cmd_buffer, qfe
                let fn = tlib#qfl#QfeFilename(qfe)
                " TLogVAR cmd_edit, cmd_buffer, fn
                if empty(cmd_edit) && empty(cmd_buffer)
                    if tlib#file#Edit(fn)
                        call tlib#buffer#ViewLine(qfe.lnum)
                    endif
                else
                    call tlib#file#With(cmd_edit, cmd_buffer, [fn], a:world)
                    " TLogDBG bufname('%')
                    " TLogVAR &filetype
                    call tlib#buffer#ViewLine(qfe.lnum)
                    " exec back
                endif
            endif
        endfor
        if set_origin
            call a:world.SetOrigin()
        endif
    endif
    return a:world
endf 


function! tlib#qfl#AgentPreviewQFE(world, selected) "{{{3
    " TLogVAR a:selected
    let back = a:world.SwitchWindow('win')
    call tlib#qfl#AgentEditQFE(a:world, a:selected[0:0], '', '', 0)
    exec back
    redraw
    let a:world.state = 'redisplay'
    return a:world
endf


function! tlib#qfl#AgentGotoQFE(world, selected) "{{{3
    let world = a:world
    if !empty(a:selected)
        let world = tlib#agent#Suspend(world, a:selected)
        call tlib#qfl#AgentEditQFE(world, a:selected[0:0])
    endif
    return world
endf


function! tlib#qfl#AgentWithSelected(world, selected, ...) "{{{3
    let cmd = a:0 >= 1 ? a:1 : input('Ex command: ', '', 'command')
    let world = a:world
    if !empty(cmd)
        let world = tlib#qfl#RunCmdOnSelected(world, a:selected, cmd)
    else
        let world.state = 'redisplay'
    endif
    return world
endf


function! tlib#qfl#RunCmdOnSelected(world, selected, cmd, ...) "{{{3
    let close_scratch = a:0 >= 1 ? a:1 : 1
    if close_scratch
        call a:world.CloseScratch()
    endif
    " TLogVAR a:cmd
    for entry in a:selected
        " TLogVAR entry, a:world.GetBaseItem(entry)
        call tlib#qfl#AgentEditQFE(a:world, [entry], '', '', 0)
        " TLogDBG bufname('%')
        exec a:cmd
        " let item = a:world.data[a:world.GetBaseIdx(entry - 1)]
        " <+TODO+>
        let item = a:world.data[entry - 1]
        " TLogVAR entry, item, getline('.')
        if has_key(a:world, 'GetBufferLines')
            let lines = a:world.GetBufferLines('.', '.')
        else
            let lines = getline('.', '.')
        endif
        let item['text'] = tlib#string#Strip(lines[0])
    endfor
    if has_key(a:world, 'AfterRunCmd')
        if bufnr('%') == a:world.bufnr
            call a:world.AfterRunCmd()
        else
            " <+TODO+> Run in other buffer
        endif
    endif
    " call s:FormatBase(a:world)
    call a:world.RestoreOrigin()
    let a:world.state = 'reset'
    return a:world
endf


function! tlib#qfl#AgentSplitBuffer(world, selected) "{{{3
    call a:world.CloseScratch()
    return tlib#qfl#AgentEditQFE(a:world, a:selected, 'split', 'sbuffer')
endf


function! tlib#qfl#AgentTabBuffer(world, selected) "{{{3
    call a:world.CloseScratch()
    return tlib#qfl#AgentEditQFE(a:world, a:selected, 'tabedit', 'tab sbuffer')
endf


function! tlib#qfl#AgentVSplitBuffer(world, selected) "{{{3
    call a:world.CloseScratch()
    return tlib#qfl#AgentEditQFE(a:world, a:selected, 'vertical split', 'vertical sbuffer')
endf


" function! tlib#qfl#AgentOpenBuffer(world, selected) "{{{3
" endf


function! tlib#qfl#AgentEditLine(world, selected) "{{{3
    call a:world.CloseScratch()
    let cmd = 'call tlib#qfl#EditLine(".")'
    return tlib#qfl#RunCmdOnSelected(a:world, a:selected, cmd)
    let a:world.state = 'reset'
    return a:world
endf


function! tlib#qfl#EditLine(lnum) "{{{3
    call inputsave()
    let line = input('', getline(a:lnum))
    call inputrestore()
    if !empty(line)
        call setline(line(a:lnum), line)
    endif
endf


function! tlib#qfl#SetFollowCursor(world, selected) "{{{3
    if empty(a:world.follow_cursor)
        let a:world.follow_cursor = 'tlib#qfl#AgentPreviewQFE'
    else
        let a:world.follow_cursor = ''
    endif
    let a:world.state = 'redisplay'
    return a:world
endf


function! tlib#qfl#QflList(list, ...) abort "{{{3
    TVarArg ['world_dict', {}], ['anyway', 0], ['suspended', 0]
    Tlibtrace 'tlib', world_dict, anyway, suspended
    " TLogVAR a:list, world_dict, anyway, suspended
    if !anyway && empty(a:list)
        return
    endif
    let world = copy(g:tlib#qfl#world)
    if !empty(world_dict)
        let world = tlib#eval#Extend(world, world_dict)
    endif
    " TLogVAR world
    let world = tlib#World#New(world)
    " echom "DBG world" string(sort(keys(world)))
    let world.data  = copy(a:list)
    if !has_key(world, 'format_data')
        let world.format_data = 'tlib#qfl#FormatQFLE'
    endif
    " TLogVAR world
    " TLogVAR world.data
    " call s:FormatBase(world)
    " TLogVAR world.base
    return tlib#input#ListW(world, suspended ? 'hibernate' : '')
endf


function! tlib#qfl#Browse(...) abort "{{{3
    let list = getqflist()
    return call(function('tlib#qfl#QflList'), [list] + a:000)
endf

