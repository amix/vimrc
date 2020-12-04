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
            let l:set_balloons = ale#Var(l:options.buffer, 'set_balloons')

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
            \&& (l:set_balloons is 1 || l:set_balloons is# 'hover')
                call balloon_show(a:response.body.displayString)
            elseif get(l:options, 'truncated_echo', 0)
                call ale#cursor#TruncatedEcho(split(a:response.body.displayString, "\n")[0])
            elseif g:ale_hover_to_preview
                call ale#preview#Show(split(a:response.body.displayString, "\n"), {
                \   'filetype': 'ale-preview.message',
                \   'stay_here': 1,
                \})
            else
                call ale#util#ShowMessage(a:response.body.displayString)
            endif
        endif
    endif
endfunction

" Convert a language name to another one.
" The language name could be an empty string or v:null
function! s:ConvertLanguageName(language) abort
    return a:language
endfunction

function! ale#hover#ParseLSPResult(contents) abort
    let l:includes = {}
    let l:highlights = []
    let l:lines = []
    let l:list = type(a:contents) is v:t_list ? a:contents : [a:contents]
    let l:region_index = 0

    for l:item in l:list
        if !empty(l:lines)
            call add(l:lines, '')
        endif

        if type(l:item) is v:t_dict && has_key(l:item, 'kind')
            if l:item.kind is# 'markdown'
                " Handle markdown values as we handle strings below.
                let l:item = get(l:item, 'value', '')
            elseif l:item.kind is# 'plaintext'
                " We shouldn't try to parse plaintext as markdown.
                " Pass the lines on and skip parsing them.
                call extend(l:lines, split(get(l:item, 'value', ''), "\n"))

                continue
            endif
        endif

        let l:marked_list = []

        " If the item is a string, then we should parse it as Markdown text.
        if type(l:item) is v:t_string
            let l:fence_language = v:null
            let l:fence_lines = []

            for l:line in split(l:item, "\n")
                if l:fence_language is v:null
                    " Look for the start of a code fence. (```python, etc.)
                    let l:match = matchlist(l:line, '^```\(.*\)$')

                    if !empty(l:match)
                        let l:fence_language = l:match[1]

                        if !empty(l:marked_list)
                            call add(l:fence_lines, '')
                        endif
                    else
                        if !empty(l:marked_list)
                        \&& l:marked_list[-1][0] isnot v:null
                            call add(l:marked_list, [v:null, ['']])
                        endif

                        call add(l:marked_list, [v:null, [l:line]])
                    endif
                elseif l:line =~# '^```$'
                    " When we hit the end of a code fence, pass the fenced
                    " lines on to the next steps below.
                    call add(l:marked_list, [l:fence_language, l:fence_lines])
                    let l:fence_language = v:null
                    let l:fence_lines = []
                else
                    " Gather lines inside of a code fence.
                    call add(l:fence_lines, l:line)
                endif
            endfor
        " If the result from the LSP server is a {language: ..., value: ...}
        " Dictionary, then that should be interpreted as if it was:
        "
        " ```${language}
        " ${value}
        " ```
        elseif type(l:item) is v:t_dict
        \&& has_key(l:item, 'language')
        \&& type(l:item.language) is v:t_string
        \&& has_key(l:item, 'value')
        \&& type(l:item.value) is v:t_string
            call add(
            \   l:marked_list,
            \   [l:item.language, split(l:item.value, "\n")],
            \)
        endif

        for [l:language, l:marked_lines] in l:marked_list
            if l:language is v:null
                " NOTE: We could handle other Markdown formatting here.
                call map(
                \   l:marked_lines,
                \   'substitute(v:val, ''\\_'', ''_'', ''g'')',
                \)
            else
                let l:language = s:ConvertLanguageName(l:language)

                if !empty(l:language)
                    let l:includes[l:language] = printf(
                    \   'syntax/%s.vim',
                    \   l:language,
                    \)

                    let l:start = len(l:lines) + 1
                    let l:end = l:start + len(l:marked_lines)
                    let l:region_index += 1

                    call add(l:highlights, 'syntax region'
                    \   . ' ALE_hover_' . l:region_index
                    \   . ' start=/\%' . l:start . 'l/'
                    \   . ' end=/\%' . l:end . 'l/'
                    \   . ' contains=@ALE_hover_' . l:language
                    \)
                endif
            endif

            call extend(l:lines, l:marked_lines)
        endfor
    endfor

    let l:include_commands = []

    for [l:language, l:lang_path] in sort(items(l:includes))
        call add(l:include_commands, 'unlet! b:current_syntax')
        call add(
        \   l:include_commands,
        \   printf('syntax include @ALE_hover_%s %s', l:language, l:lang_path),
        \)
    endfor

    return [l:include_commands + l:highlights, l:lines]
endfunction

function! ale#hover#HandleLSPResponse(conn_id, response) abort
    if has_key(a:response, 'id')
    \&& has_key(s:hover_map, a:response.id)
        let l:options = remove(s:hover_map, a:response.id)

        " If the call did __not__ come from balloonexpr...
        if !get(l:options, 'hover_from_balloonexpr', 0)
            let l:buffer = bufnr('')
            let [l:line, l:column] = getpos('.')[1:2]
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

        let [l:commands, l:lines] = ale#hover#ParseLSPResult(l:result.contents)

        if !empty(l:lines)
            let l:set_balloons = ale#Var(l:options.buffer, 'set_balloons')

            if get(l:options, 'hover_from_balloonexpr', 0)
            \&& exists('*balloon_show')
            \&& (l:set_balloons is 1 || l:set_balloons is# 'hover')
                call balloon_show(join(l:lines, "\n"))
            elseif get(l:options, 'truncated_echo', 0)
                call ale#cursor#TruncatedEcho(l:lines[0])
            elseif g:ale_hover_to_preview
                call ale#preview#Show(l:lines, {
                \   'filetype': 'ale-preview.message',
                \   'stay_here': 1,
                \   'commands': l:commands,
                \})
            else
                call ale#util#ShowMessage(join(l:lines, "\n"), {
                \   'commands': l:commands,
                \})
            endif
        endif
    endif
endfunction

function! s:OnReady(line, column, opt, linter, lsp_details) abort
    let l:id = a:lsp_details.connection_id

    if !ale#lsp#HasCapability(l:id, 'hover')
        return
    endif

    let l:buffer = a:lsp_details.buffer

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

        let l:column = max([
        \   min([a:column, len(getbufline(l:buffer, a:line)[0])]),
        \   1,
        \])

        let l:message = ale#lsp#message#Hover(l:buffer, a:line, l:column)
    endif

    let l:request_id = ale#lsp#Send(l:id, l:message)

    let s:hover_map[l:request_id] = {
    \   'buffer': l:buffer,
    \   'line': a:line,
    \   'column': l:column,
    \   'hover_from_balloonexpr': get(a:opt, 'called_from_balloonexpr', 0),
    \   'show_documentation': get(a:opt, 'show_documentation', 0),
    \   'truncated_echo': get(a:opt, 'truncated_echo', 0),
    \}
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
    let l:Callback = function('s:OnReady', [a:line, a:col, a:opt])

    for l:linter in ale#linter#Get(getbufvar(a:buffer, '&filetype'))
        " Only tsserver supports documentation requests at the moment.
        if !empty(l:linter.lsp)
        \&& (!l:show_documentation || l:linter.lsp is# 'tsserver')
            call ale#lsp_linter#StartLSP(a:buffer, l:linter, l:Callback)
        endif
    endfor
endfunction

let s:last_pos = [0, 0, 0]

" This function implements the :ALEHover command.
function! ale#hover#ShowAtCursor() abort
    let l:buffer = bufnr('')
    let l:pos = getpos('.')

    call ale#hover#Show(l:buffer, l:pos[1], l:pos[2], {})
endfunction

function! ale#hover#ShowTruncatedMessageAtCursor() abort
    let l:buffer = bufnr('')
    let l:pos = getpos('.')[0:2]

    if l:pos != s:last_pos
        let s:last_pos = l:pos
        let [l:info, l:loc] = ale#util#FindItemAtCursor(l:buffer)

        if empty(l:loc)
            call ale#hover#Show(
            \   l:buffer,
            \   l:pos[1],
            \   l:pos[2],
            \   {'truncated_echo': 1},
            \)
        endif
    endif
endfunction

" This function implements the :ALEDocumentation command.
function! ale#hover#ShowDocumentationAtCursor() abort
    let l:buffer = bufnr('')
    let l:pos = getpos('.')
    let l:options = {'show_documentation': 1}

    call ale#hover#Show(l:buffer, l:pos[1], l:pos[2], l:options)
endfunction
