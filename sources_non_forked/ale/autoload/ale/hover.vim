" Author: w0rp <devw0rp@gmail.com>
" Description: Hover support for LSP linters.

let s:hover_map = {}

" Used to get the hover map in tests.
function! ale#hover#GetMap() abort
    return deepcopy(s:hover_map)
endfunction

" Used to set the hover map in tests.
function! ale#hover#SetMap(map) abort
    let s:hover_map = a:map
endfunction

function! ale#hover#ClearLSPData() abort
    let s:hover_map = {}
endfunction

function! ale#hover#HandleTSServerResponse(conn_id, response) abort
    if get(a:response, 'command', '') is# 'quickinfo'
    \&& has_key(s:hover_map, a:response.request_seq)
        let l:options = remove(s:hover_map, a:response.request_seq)

        if get(a:response, 'success', v:false) is v:true
        \&& get(a:response, 'body', v:null) isnot v:null
            " If we pass the show_documentation flag, we should show the full
            " documentation, and always in the preview window.
            if get(l:options, 'show_documentation', 0)
                let l:documentation = get(a:response.body, 'documentation', '')

                " displayString is not included here, because it can be very
                " noisy and run on for many lines for complex types. A less
                " verbose alternative may be nice in future.
                if !empty(l:documentation)
                    call ale#preview#Show(split(l:documentation, "\n"), {
                    \   'filetype': 'ale-preview.message',
                    \   'stay_here': 1,
                    \})
                endif
            elseif get(l:options, 'hover_from_balloonexpr', 0)
            \&& exists('*balloon_show')
            \&& ale#Var(l:options.buffer, 'set_balloons')
                call balloon_show(a:response.body.displayString)
            else
                call ale#util#ShowMessage(a:response.body.displayString)
            endif
        endif
    endif
endfunction

function! ale#hover#HandleLSPResponse(conn_id, response) abort
    if has_key(a:response, 'id')
    \&& has_key(s:hover_map, a:response.id)
        let l:options = remove(s:hover_map, a:response.id)

        " If the call did __not__ come from balloonexpr...
        if !get(l:options, 'hover_from_balloonexpr', 0)
            let l:buffer = bufnr('')
            let [l:line, l:column] = getcurpos()[1:2]
            let l:end = len(getline(l:line))

            if l:buffer isnot l:options.buffer
            \|| l:line isnot l:options.line
            \|| min([l:column, l:end]) isnot min([l:options.column, l:end])
                " ... Cancel display the message if the cursor has moved.
                return
            endif
        endif

        " The result can be a Dictionary item, a List of the same, or null.
        let l:result = get(a:response, 'result', v:null)

        if l:result is v:null
            return
        endif

        let l:result = l:result.contents

        if type(l:result) is v:t_string
             " The result can be just a string.
             let l:result = [l:result]
        endif

        if type(l:result) is v:t_dict
            " If the result is an object, then it's markup content.
            let l:result = [l:result.value]
        endif

        if type(l:result) is v:t_list
            " Replace objects with text values.
            call map(l:result, 'type(v:val) is v:t_string ? v:val : v:val.value')
            let l:str = join(l:result, "\n")
            let l:str = substitute(l:str, '^\s*\(.\{-}\)\s*$', '\1', '')

            if !empty(l:str)
                if get(l:options, 'hover_from_balloonexpr', 0)
                \&& exists('*balloon_show')
                \&& ale#Var(l:options.buffer, 'set_balloons')
                    call balloon_show(l:str)
                else
                    call ale#util#ShowMessage(l:str)
                endif
            endif
        endif
    endif
endfunction

function! s:OnReady(linter, lsp_details, line, column, opt, ...) abort
    let l:buffer = a:lsp_details.buffer
    let l:id = a:lsp_details.connection_id

    let l:Callback = a:linter.lsp is# 'tsserver'
    \   ? function('ale#hover#HandleTSServerResponse')
    \   : function('ale#hover#HandleLSPResponse')
    call ale#lsp#RegisterCallback(l:id, l:Callback)

    if a:linter.lsp is# 'tsserver'
        let l:column = a:column

        let l:message = ale#lsp#tsserver_message#Quickinfo(
        \   l:buffer,
        \   a:line,
        \   l:column
        \)
    else
        " Send a message saying the buffer has changed first, or the
        " hover position probably won't make sense.
        call ale#lsp#NotifyForChanges(l:id, l:buffer)

        let l:column = min([a:column, len(getbufline(l:buffer, a:line)[0])])

        let l:message = ale#lsp#message#Hover(l:buffer, a:line, l:column)
    endif

    let l:request_id = ale#lsp#Send(l:id, l:message)

    let s:hover_map[l:request_id] = {
    \   'buffer': l:buffer,
    \   'line': a:line,
    \   'column': l:column,
    \   'hover_from_balloonexpr': get(a:opt, 'called_from_balloonexpr', 0),
    \   'show_documentation': get(a:opt, 'show_documentation', 0),
    \}
endfunction

function! s:ShowDetails(linter, buffer, line, column, opt, ...) abort
    let l:lsp_details = ale#lsp_linter#StartLSP(a:buffer, a:linter)

    if empty(l:lsp_details)
        return 0
    endif

    let l:id = l:lsp_details.connection_id

    call ale#lsp#WaitForCapability(l:id, 'hover', function('s:OnReady', [
    \   a:linter, l:lsp_details, a:line, a:column, a:opt
    \]))
endfunction

" Obtain Hover information for the specified position
" Pass optional arguments in the dictionary opt.
" Currently, only one key/value is useful:
"   - called_from_balloonexpr, this flag marks if we want the result from this
"     ale#hover#Show to display in a balloon if possible
"
" Currently, the callbacks displays the info from hover :
" - in the balloon if opt.called_from_balloonexpr and balloon_show is detected
" - as status message otherwise
function! ale#hover#Show(buffer, line, col, opt) abort
    let l:show_documentation = get(a:opt, 'show_documentation', 0)

    for l:linter in ale#linter#Get(getbufvar(a:buffer, '&filetype'))
        " Only tsserver supports documentation requests at the moment.
        if !empty(l:linter.lsp)
        \&& (!l:show_documentation || l:linter.lsp is# 'tsserver')
            call s:ShowDetails(l:linter, a:buffer, a:line, a:col, a:opt)
        endif
    endfor
endfunction

" This function implements the :ALEHover command.
function! ale#hover#ShowAtCursor() abort
    let l:buffer = bufnr('')
    let l:pos = getcurpos()

    call ale#hover#Show(l:buffer, l:pos[1], l:pos[2], {})
endfunction

" This function implements the :ALEDocumentation command.
function! ale#hover#ShowDocumentationAtCursor() abort
    let l:buffer = bufnr('')
    let l:pos = getcurpos()
    let l:options = {'show_documentation': 1}

    call ale#hover#Show(l:buffer, l:pos[1], l:pos[2], l:options)
endfunction
