" Author: Jerko Steiner <jerko.steiner@gmail.com>
" Description: Code action support for LSP / tsserver

function! ale#code_action#ReloadBuffer() abort
    let l:buffer = bufnr('')

    execute 'augroup ALECodeActionReloadGroup' . l:buffer
        autocmd!
    augroup END

    silent! execute 'augroup! ALECodeActionReloadGroup' . l:buffer

    call ale#util#Execute(':e!')
endfunction

function! ale#code_action#HandleCodeAction(code_action, options) abort
    let l:current_buffer = bufnr('')
    let l:changes = a:code_action.changes
    let l:should_save = get(a:options, 'should_save')

    for l:file_code_edit in l:changes
        call ale#code_action#ApplyChanges(
        \   l:file_code_edit.fileName,
        \   l:file_code_edit.textChanges,
        \   l:should_save,
        \)
    endfor
endfunction

function! s:ChangeCmp(left, right) abort
    if a:left.start.line < a:right.start.line
        return -1
    endif

    if a:left.start.line > a:right.start.line
        return 1
    endif

    if a:left.start.offset < a:right.start.offset
        return -1
    endif

    if a:left.start.offset > a:right.start.offset
        return 1
    endif

    if a:left.end.line < a:right.end.line
        return -1
    endif

    if a:left.end.line > a:right.end.line
        return 1
    endif

    if a:left.end.offset < a:right.end.offset
        return -1
    endif

    if a:left.end.offset > a:right.end.offset
        return 1
    endif

    return 0
endfunction

function! ale#code_action#ApplyChanges(filename, changes, should_save) abort
    let l:current_buffer = bufnr('')
    " The buffer is used to determine the fileformat, if available.
    let l:buffer = bufnr(a:filename)
    let l:is_current_buffer = l:buffer > 0 && l:buffer == l:current_buffer

    if l:buffer > 0
        let l:lines = getbufline(l:buffer, 1, '$')

        " Add empty line if there's trailing newline, like readfile() does.
        if getbufvar(l:buffer, '&eol')
            let l:lines += ['']
        endif
    else
        let l:lines = readfile(a:filename, 'b')
    endif

    if l:is_current_buffer
        let l:pos = getpos('.')[1:2]
    else
        let l:pos = [1, 1]
    endif

    " Changes have to be sorted so we apply them from bottom-to-top
    for l:code_edit in reverse(sort(copy(a:changes), function('s:ChangeCmp')))
        let l:line = l:code_edit.start.line
        let l:column = l:code_edit.start.offset
        let l:end_line = l:code_edit.end.line
        let l:end_column = l:code_edit.end.offset
        let l:text = l:code_edit.newText

        let l:insertions = split(l:text, '\n', 1)

        " Fix invalid columns
        let l:column = l:column > 0 ? l:column : 1
        let l:end_column = l:end_column > 0 ? l:end_column : 1

        " Clamp start to BOF
        if l:line < 1
            let [l:line, l:column] = [1, 1]
        endif

        " Clamp start to EOF
        if l:line > len(l:lines) || l:line == len(l:lines) && l:column > len(l:lines[-1]) + 1
            let [l:line, l:column] = [len(l:lines), len(l:lines[-1]) + 1]
        " Special case when start is after EOL
        elseif l:line < len(l:lines) && l:column > len(l:lines[l:line - 1]) + 1
            let [l:line, l:column] = [l:line + 1, 1]
        endif

        " Adjust end: clamp if invalid and/or adjust if we moved start
        if l:end_line < l:line || l:end_line == l:line && l:end_column < l:column
            let [l:end_line, l:end_column] = [l:line, l:column]
        endif

        " Clamp end to EOF
        if l:end_line > len(l:lines) || l:end_line == len(l:lines) && l:end_column > len(l:lines[-1]) + 1
            let [l:end_line, l:end_column] = [len(l:lines), len(l:lines[-1]) + 1]
        " Special case when end is after EOL
        elseif l:end_line < len(l:lines) && l:end_column > len(l:lines[l:end_line - 1]) + 1
            let [l:end_line, l:end_column] = [l:end_line + 1, 1]
        endif

        " Careful, [:-1] is not an empty list
        let l:start = l:line is 1 ? [] : l:lines[: l:line - 2]
        let l:middle = l:column is 1 ? [''] : [l:lines[l:line - 1][: l:column - 2]]

        let l:middle[-1] .= l:insertions[0]
        let l:middle     += l:insertions[1:]
        let l:middle[-1] .= l:lines[l:end_line - 1][l:end_column - 1 :]

        let l:end_line_len = len(l:lines[l:end_line - 1])
        let l:lines_before_change = len(l:lines)
        let l:lines = l:start + l:middle + l:lines[l:end_line :]

        let l:current_line_offset = len(l:lines) - l:lines_before_change
        let l:column_offset = len(l:middle[-1]) - l:end_line_len

        " Keep cursor where it was (if outside of changes) or move it after
        " the changed text (if inside), but don't touch it when the change
        " spans the entire buffer, in which case we have no clue and it's
        " better to not do anything.
        if l:line isnot 1 || l:column isnot 1
        \|| l:end_line < l:lines_before_change
        \|| l:end_line == l:lines_before_change && l:end_column <= l:end_line_len
            let l:pos = s:UpdateCursor(l:pos,
            \ [l:line, l:column],
            \ [l:end_line, l:end_column],
            \ [l:current_line_offset, l:column_offset])
        endif
    endfor

    if l:buffer > 0
        " Make sure ale#util#{Writefile,SetBufferContents} add trailing
        " newline if and only if it should be added.
        if l:lines[-1] is# '' && getbufvar(l:buffer, '&eol')
            call remove(l:lines, -1)
        else
            call setbufvar(l:buffer, '&eol', 0)
        endif
    elseif exists('+fixeol') && &fixeol && l:lines[-1] is# ''
        " Not in buffer, ale#util#Writefile can't check &eol and always adds
        " newline if &fixeol: remove to prevent double trailing newline.
        call remove(l:lines, -1)
    endif

    if a:should_save || l:buffer < 0
        call ale#util#Writefile(l:buffer, l:lines, a:filename)
    else
        call ale#util#SetBufferContents(l:buffer, l:lines)
    endif

    if l:is_current_buffer
        if a:should_save
            call ale#util#Execute(':e!')
        endif

        call setpos('.', [0, l:pos[0], l:pos[1], 0])
    endif

    if a:should_save && l:buffer > 0 && !l:is_current_buffer
        " Set up a one-time use event that will delete itself to reload the
        " buffer next time it's entered to view the changes made to it.
        execute 'augroup ALECodeActionReloadGroup' . l:buffer
            autocmd!

            execute printf(
            \   'autocmd BufEnter <buffer=%d>'
            \       . ' call ale#code_action#ReloadBuffer()',
            \   l:buffer
            \)
        augroup END
    endif
endfunction

function! s:UpdateCursor(cursor, start, end, offset) abort
    let l:cur_line = a:cursor[0]
    let l:cur_column = a:cursor[1]
    let l:line = a:start[0]
    let l:column = a:start[1]
    let l:end_line = a:end[0]
    let l:end_column = a:end[1]
    let l:line_offset = a:offset[0]
    let l:column_offset = a:offset[1]

    if l:end_line < l:cur_line
        " both start and end lines are before the cursor. only line offset
        " needs to be updated
        let l:cur_line += l:line_offset
    elseif l:end_line == l:cur_line
        " end line is at the same location as cursor, which means
        " l:line <= l:cur_line
        if l:line < l:cur_line || l:column <= l:cur_column
            " updates are happening either before or around the cursor
            if l:end_column < l:cur_column
                " updates are happening before the cursor, update the
                " column offset for cursor
                let l:cur_line += l:line_offset
                let l:cur_column += l:column_offset
            else
                " updates are happening around the cursor, move the cursor
                " to the end of the changes
                let l:cur_line += l:line_offset
                let l:cur_column = l:end_column + l:column_offset
            endif
        " else is not necessary, it means modifications are happening
        " after the cursor so no cursor updates need to be done
        endif
    else
        " end line is after the cursor
        if l:line < l:cur_line || l:line == l:cur_line && l:column <= l:cur_column
            " changes are happening around the cursor, move the cursor
            " to the end of the changes
            let l:cur_line = l:end_line + l:line_offset
            let l:cur_column = l:end_column + l:column_offset
        " else is not necesary, it means modifications are happening
        " after the cursor so no cursor updates need to be done
        endif
    endif

    return [l:cur_line, l:cur_column]
endfunction

function! ale#code_action#GetChanges(workspace_edit) abort
    if a:workspace_edit is v:null
        return {}
    endif

    let l:changes = {}

    if has_key(a:workspace_edit, 'changes') && !empty(a:workspace_edit.changes)
        return a:workspace_edit.changes
    elseif has_key(a:workspace_edit, 'documentChanges')
        let l:document_changes = []

        if type(a:workspace_edit.documentChanges) is v:t_dict
        \ && has_key(a:workspace_edit.documentChanges, 'edits')
            call add(l:document_changes, a:workspace_edit.documentChanges)
        elseif type(a:workspace_edit.documentChanges) is v:t_list
            let l:document_changes = a:workspace_edit.documentChanges
        endif

        for l:text_document_edit in l:document_changes
            let l:filename = l:text_document_edit.textDocument.uri
            let l:edits = l:text_document_edit.edits
            let l:changes[l:filename] = l:edits
        endfor
    endif

    return l:changes
endfunction

function! ale#code_action#BuildChangesList(changes_map) abort
    let l:changes = []

    for l:file_name in keys(a:changes_map)
        let l:text_edits = a:changes_map[l:file_name]
        let l:text_changes = []

        for l:edit in l:text_edits
            let l:range = l:edit.range
            let l:new_text = l:edit.newText

            call add(l:text_changes, {
            \ 'start': {
            \   'line': l:range.start.line + 1,
            \   'offset': l:range.start.character + 1,
            \ },
            \ 'end': {
            \   'line': l:range.end.line + 1,
            \   'offset': l:range.end.character + 1,
            \ },
            \ 'newText': l:new_text,
            \})
        endfor

        call add(l:changes, {
        \   'fileName': ale#path#FromURI(l:file_name),
        \   'textChanges': l:text_changes,
        \})
    endfor

    return l:changes
endfunction

function! s:EscapeMenuName(text) abort
    return substitute(a:text, '\\\| \|\.\|&', '\\\0', 'g')
endfunction

function! s:UpdateMenu(data, menu_items) abort
    silent! aunmenu PopUp.Refactor\.\.\.

    if empty(a:data)
        return
    endif

    for [l:type, l:item] in a:menu_items
        let l:name = l:type is# 'tsserver' ? l:item.name : l:item.title
        let l:func_name = l:type is# 'tsserver'
        \   ? 'ale#codefix#ApplyTSServerCodeAction'
        \   : 'ale#codefix#ApplyLSPCodeAction'

        execute printf(
        \   'anoremenu <silent> PopUp.&Refactor\.\.\..%s'
        \       . ' :call %s(%s, %s)<CR>',
        \   s:EscapeMenuName(l:name),
        \   l:func_name,
        \   string(a:data),
        \   string(l:item),
        \)
    endfor

    if empty(a:menu_items)
        silent! anoremenu PopUp.Refactor\.\.\..(None) :silent
    endif
endfunction

function! s:GetCodeActions(linter, options) abort
    let l:buffer = bufnr('')
    let [l:line, l:column] = getpos('.')[1:2]
    let l:column = min([l:column, len(getline(l:line))])

    let l:location = {
    \   'buffer': l:buffer,
    \   'line': l:line,
    \   'column': l:column,
    \   'end_line': l:line,
    \   'end_column': l:column,
    \}
    let l:Callback = function('s:OnReady', [l:location, a:options])
    call ale#lsp_linter#StartLSP(l:buffer, a:linter, l:Callback)
endfunction

function! ale#code_action#GetCodeActions(options) abort
    silent! aunmenu PopUp.Rename
    silent! aunmenu PopUp.Refactor\.\.\.

    " Only display the menu items if there's an LSP server.
    let l:has_lsp = 0

    for l:linter in ale#linter#Get(&filetype)
        if !empty(l:linter.lsp)
            let l:has_lsp = 1

            break
        endif
    endfor

    if l:has_lsp
        if !empty(expand('<cword>'))
            silent! anoremenu <silent> PopUp.Rename :ALERename<CR>
        endif

        silent! anoremenu <silent> PopUp.Refactor\.\.\..(None) :silent<CR>

        call ale#codefix#Execute(
        \   mode() is# 'v' || mode() is# "\<C-V>",
        \   function('s:UpdateMenu')
        \)
    endif
endfunction

function! s:Setup(enabled) abort
    augroup ALECodeActionsGroup
        autocmd!

        if a:enabled
            autocmd MenuPopup * :call ale#code_action#GetCodeActions({})
        endif
    augroup END

    if !a:enabled
        silent! augroup! ALECodeActionsGroup

        silent! aunmenu PopUp.Rename
        silent! aunmenu PopUp.Refactor\.\.\.
    endif
endfunction

function! ale#code_action#EnablePopUpMenu() abort
    call s:Setup(1)
endfunction

function! ale#code_action#DisablePopUpMenu() abort
    call s:Setup(0)
endfunction
