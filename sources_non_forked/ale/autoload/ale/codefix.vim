" Author: Dalius Dobravolskas <dalius.dobravolskas@gmail.com>
" Description: Code Fix support for tsserver and LSP servers

let s:codefix_map = {}

" Used to get the codefix map in tests.
function! ale#codefix#GetMap() abort
    return deepcopy(s:codefix_map)
endfunction

" Used to set the codefix map in tests.
function! ale#codefix#SetMap(map) abort
    let s:codefix_map = a:map
endfunction

function! ale#codefix#ClearLSPData() abort
    let s:codefix_map = {}
endfunction

function! s:message(message) abort
    call ale#util#Execute('echom ' . string(a:message))
endfunction

function! ale#codefix#ApplyTSServerCodeAction(data, item) abort
    if has_key(a:item, 'changes')
        let l:changes = a:item.changes

        call ale#code_action#HandleCodeAction(
        \   {
        \       'description': 'codefix',
        \       'changes': l:changes,
        \   },
        \   {},
        \)
    else
        let l:message = ale#lsp#tsserver_message#GetEditsForRefactor(
        \   a:data.buffer,
        \   a:data.line,
        \   a:data.column,
        \   a:data.end_line,
        \   a:data.end_column,
        \   a:item.id[0],
        \   a:item.id[1],
        \)

        let l:request_id = ale#lsp#Send(a:data.connection_id, l:message)

        let s:codefix_map[l:request_id] = a:data
    endif
endfunction

function! ale#codefix#HandleTSServerResponse(conn_id, response) abort
    if !has_key(a:response, 'request_seq')
    \ || !has_key(s:codefix_map, a:response.request_seq)
        return
    endif

    let l:data = remove(s:codefix_map, a:response.request_seq)
    let l:MenuCallback = get(l:data, 'menu_callback', v:null)

    if get(a:response, 'command', '') is# 'getCodeFixes'
        if get(a:response, 'success', v:false) is v:false
        \&& l:MenuCallback is v:null
            let l:message = get(a:response, 'message', 'unknown')
            call s:message('Error while getting code fixes. Reason: ' . l:message)

            return
        endif

        let l:result = get(a:response, 'body', [])
        call filter(l:result, 'has_key(v:val, ''changes'')')

        if l:MenuCallback isnot v:null
            call l:MenuCallback(
            \   l:data,
            \   map(copy(l:result), '[''tsserver'', v:val]')
            \)

            return
        endif

        if len(l:result) == 0
            call s:message('No code fixes available.')

            return
        endif

        let l:code_fix_to_apply = 0

        if len(l:result) == 1
            let l:code_fix_to_apply = 1
        else
            let l:codefix_no = 1
            let l:codefixstring = "Code Fixes:\n"

            for l:codefix in l:result
                let l:codefixstring .= l:codefix_no . ') '
                \   . l:codefix.description . "\n"
                let l:codefix_no += 1
            endfor

            let l:codefixstring .= 'Type number and <Enter> (empty cancels): '

            let l:code_fix_to_apply = ale#util#Input(l:codefixstring, '')
            let l:code_fix_to_apply = str2nr(l:code_fix_to_apply)

            if l:code_fix_to_apply == 0
                return
            endif
        endif

        call ale#codefix#ApplyTSServerCodeAction(
        \   l:data,
        \   l:result[l:code_fix_to_apply - 1],
        \)
    elseif get(a:response, 'command', '') is# 'getApplicableRefactors'
        if get(a:response, 'success', v:false) is v:false
        \&& l:MenuCallback is v:null
            let l:message = get(a:response, 'message', 'unknown')
            call s:message('Error while getting applicable refactors. Reason: ' . l:message)

            return
        endif

        let l:result = get(a:response, 'body', [])

        if len(l:result) == 0
            call s:message('No applicable refactors available.')

            return
        endif

        let l:refactors = []

        for l:item in l:result
            for l:action in l:item.actions
                call add(l:refactors, {
                \   'name': l:action.description,
                \   'id': [l:item.name, l:action.name],
                \})
            endfor
        endfor

        if l:MenuCallback isnot v:null
            call l:MenuCallback(
            \   l:data,
            \   map(copy(l:refactors), '[''tsserver'', v:val]')
            \)

            return
        endif

        let l:refactor_no = 1
        let l:refactorstring = "Applicable refactors:\n"

        for l:refactor in l:refactors
            let l:refactorstring .= l:refactor_no . ') '
            \   . l:refactor.name . "\n"
            let l:refactor_no += 1
        endfor

        let l:refactorstring .= 'Type number and <Enter> (empty cancels): '

        let l:refactor_to_apply = ale#util#Input(l:refactorstring, '')
        let l:refactor_to_apply = str2nr(l:refactor_to_apply)

        if l:refactor_to_apply == 0
            return
        endif

        let l:id = l:refactors[l:refactor_to_apply - 1].id

        call ale#codefix#ApplyTSServerCodeAction(
        \   l:data,
        \   l:refactors[l:refactor_to_apply - 1],
        \)
    elseif get(a:response, 'command', '') is# 'getEditsForRefactor'
        if get(a:response, 'success', v:false) is v:false
            let l:message = get(a:response, 'message', 'unknown')
            call s:message('Error while getting edits for refactor. Reason: ' . l:message)

            return
        endif

        call ale#code_action#HandleCodeAction(
        \   {
        \       'description': 'editsForRefactor',
        \       'changes': a:response.body.edits,
        \   },
        \   {},
        \)
    endif
endfunction

function! ale#codefix#ApplyLSPCodeAction(data, item) abort
    if has_key(a:item, 'command')
    \&& type(a:item.command) == v:t_dict
        let l:command = a:item.command
        let l:message = ale#lsp#message#ExecuteCommand(
        \   l:command.command,
        \   l:command.arguments,
        \)

        let l:request_id = ale#lsp#Send(a:data.connection_id, l:message)
    elseif has_key(a:item, 'edit') || has_key(a:item, 'arguments')
        if has_key(a:item, 'edit')
            let l:topass = a:item.edit
        else
            let l:topass = a:item.arguments[0]
        endif

        let l:changes_map = ale#code_action#GetChanges(l:topass)

        if empty(l:changes_map)
            return
        endif

        let l:changes = ale#code_action#BuildChangesList(l:changes_map)

        call ale#code_action#HandleCodeAction(
        \   {
        \       'description': 'codeaction',
        \       'changes': l:changes,
        \   },
        \   {},
        \)
    endif
endfunction

function! ale#codefix#HandleLSPResponse(conn_id, response) abort
    if has_key(a:response, 'method')
    \ && a:response.method is# 'workspace/applyEdit'
    \ && has_key(a:response, 'params')
        let l:params = a:response.params

        let l:changes_map = ale#code_action#GetChanges(l:params.edit)

        if empty(l:changes_map)
            return
        endif

        let l:changes = ale#code_action#BuildChangesList(l:changes_map)

        call ale#code_action#HandleCodeAction(
        \   {
        \       'description': 'applyEdit',
        \       'changes': l:changes,
        \   },
        \   {}
        \)
    elseif has_key(a:response, 'id')
    \&& has_key(s:codefix_map, a:response.id)
        let l:data = remove(s:codefix_map, a:response.id)
        let l:MenuCallback = get(l:data, 'menu_callback', v:null)

        let l:result = get(a:response, 'result')

        if type(l:result) != v:t_list
            let l:result = []
        endif

        " Send the results to the menu callback, if set.
        if l:MenuCallback isnot v:null
            call l:MenuCallback(map(copy(l:result), '[''lsp'', v:val]'))

            return
        endif

        if len(l:result) == 0
            call s:message('No code actions received from server')

            return
        endif

        let l:codeaction_no = 1
        let l:codeactionstring = "Code Fixes:\n"

        for l:codeaction in l:result
            let l:codeactionstring .= l:codeaction_no . ') '
            \   . l:codeaction.title . "\n"
            let l:codeaction_no += 1
        endfor

        let l:codeactionstring .= 'Type number and <Enter> (empty cancels): '

        let l:codeaction_to_apply = ale#util#Input(l:codeactionstring, '')
        let l:codeaction_to_apply = str2nr(l:codeaction_to_apply)

        if l:codeaction_to_apply == 0
            return
        endif

        let l:item = l:result[l:codeaction_to_apply - 1]

        call ale#codefix#ApplyLSPCodeAction(l:data, l:item)
    endif
endfunction

function! s:FindError(buffer, line, column, end_line, end_column) abort
    let l:nearest_error = v:null

    if a:line == a:end_line
    \&& a:column == a:end_column
    \&& has_key(g:ale_buffer_info, a:buffer)
        let l:nearest_error_diff = -1

        for l:error in get(g:ale_buffer_info[a:buffer], 'loclist', [])
            if has_key(l:error, 'code') && l:error.lnum == a:line
                let l:diff = abs(l:error.col - a:column)

                if l:nearest_error_diff == -1 || l:diff < l:nearest_error_diff
                    let l:nearest_error_diff = l:diff
                    let l:nearest_error = l:error
                endif
            endif
        endfor
    endif

    return l:nearest_error
endfunction

function! s:OnReady(
\   line,
\   column,
\   end_line,
\   end_column,
\   MenuCallback,
\   linter,
\   lsp_details,
\) abort
    let l:id = a:lsp_details.connection_id

    if !ale#lsp#HasCapability(l:id, 'code_actions')
        return
    endif

    let l:buffer = a:lsp_details.buffer

    if a:linter.lsp is# 'tsserver'
        let l:nearest_error =
        \   s:FindError(l:buffer, a:line, a:column, a:end_line, a:end_column)

        if l:nearest_error isnot v:null
            let l:message = ale#lsp#tsserver_message#GetCodeFixes(
            \   l:buffer,
            \   a:line,
            \   a:column,
            \   a:line,
            \   a:column,
            \   [l:nearest_error.code],
            \)
        else
            let l:message = ale#lsp#tsserver_message#GetApplicableRefactors(
            \   l:buffer,
            \   a:line,
            \   a:column,
            \   a:end_line,
            \   a:end_column,
            \)
        endif
    else
        " Send a message saying the buffer has changed first, otherwise
        " completions won't know what text is nearby.
        call ale#lsp#NotifyForChanges(l:id, l:buffer)

        let l:diagnostics = []
        let l:nearest_error =
        \   s:FindError(l:buffer, a:line, a:column, a:end_line, a:end_column)

        if l:nearest_error isnot v:null
            let l:diagnostics = [
            \   {
            \       'code': l:nearest_error.code,
            \       'message': l:nearest_error.text,
            \       'range': {
            \           'start': {
            \               'line': l:nearest_error.lnum - 1,
            \               'character': l:nearest_error.col - 1,
            \           },
            \           'end': {
            \               'line': l:nearest_error.end_lnum - 1,
            \               'character': l:nearest_error.end_col,
            \           },
            \       },
            \   },
            \]
        endif

        let l:message = ale#lsp#message#CodeAction(
        \   l:buffer,
        \   a:line,
        \   a:column,
        \   a:end_line,
        \   a:end_column,
        \   l:diagnostics,
        \)
    endif

    let l:Callback = a:linter.lsp is# 'tsserver'
    \   ? function('ale#codefix#HandleTSServerResponse')
    \   : function('ale#codefix#HandleLSPResponse')

    call ale#lsp#RegisterCallback(l:id, l:Callback)

    let l:request_id = ale#lsp#Send(l:id, l:message)

    let s:codefix_map[l:request_id] = {
    \   'connection_id': l:id,
    \   'buffer': l:buffer,
    \   'line': a:line,
    \   'column': a:column,
    \   'end_line': a:end_line,
    \   'end_column': a:end_column,
    \   'menu_callback': a:MenuCallback,
    \}
endfunction

function! s:ExecuteGetCodeFix(linter, range, MenuCallback) abort
    let l:buffer = bufnr('')

    if a:range == 0
        let [l:line, l:column] = getpos('.')[1:2]
        let l:end_line = l:line
        let l:end_column = l:column

        " Expand the range to cover the current word, if there is one.
        let l:cword = expand('<cword>')

        if !empty(l:cword)
            let l:search_pos = searchpos('\V' . l:cword, 'bn', l:line)

            if l:search_pos != [0, 0]
                let l:column = l:search_pos[1]
                let l:end_column = l:column + len(l:cword) - 1
            endif
        endif
    elseif mode() is# 'v' || mode() is# "\<C-V>"
        " You need to get the start and end in a different way when you're in
        " visual mode.
        let [l:line, l:column] = getpos('v')[1:2]
        let [l:end_line, l:end_column] = getpos('.')[1:2]
    else
        let [l:line, l:column] = getpos("'<")[1:2]
        let [l:end_line, l:end_column] = getpos("'>")[1:2]
    endif

    let l:column = min([l:column, len(getline(l:line))])
    let l:end_column = min([l:end_column, len(getline(l:end_line))])

    let l:Callback = function(
    \ 's:OnReady', [l:line, l:column, l:end_line, l:end_column, a:MenuCallback]
    \)

    call ale#lsp_linter#StartLSP(l:buffer, a:linter, l:Callback)
endfunction

function! ale#codefix#Execute(range, ...) abort
    if a:0 > 1
        throw 'Too many arguments'
    endif

    let l:MenuCallback = get(a:000, 0, v:null)
    let l:lsp_linters = []

    for l:linter in ale#linter#Get(&filetype)
        if !empty(l:linter.lsp)
            call add(l:lsp_linters, l:linter)
        endif
    endfor

    if empty(l:lsp_linters)
        if l:MenuCallback is v:null
            call s:message('No active LSPs')
        else
            call l:MenuCallback({}, [])
        endif

        return
    endif

    for l:lsp_linter in l:lsp_linters
        call s:ExecuteGetCodeFix(l:lsp_linter, a:range, l:MenuCallback)
    endfor
endfunction
