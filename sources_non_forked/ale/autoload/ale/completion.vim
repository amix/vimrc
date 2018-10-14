" Author: w0rp <devw0rp@gmail.com>
" Description: Completion support for LSP linters

" The omnicompletion menu is shown through a special Plug mapping which is
" only valid in Insert mode. This way, feedkeys() won't send these keys if you
" quit Insert mode quickly enough.
inoremap <silent> <Plug>(ale_show_completion_menu) <C-x><C-o>
" If we hit the key sequence in normal mode, then we won't show the menu, so
" we should restore the old settings right away.
nnoremap <silent> <Plug>(ale_show_completion_menu) :call ale#completion#RestoreCompletionOptions()<CR>
cnoremap <silent> <Plug>(ale_show_completion_menu) <Nop>
vnoremap <silent> <Plug>(ale_show_completion_menu) <Nop>
onoremap <silent> <Plug>(ale_show_completion_menu) <Nop>

let g:ale_completion_delay = get(g:, 'ale_completion_delay', 100)
let g:ale_completion_excluded_words = get(g:, 'ale_completion_excluded_words', [])
let g:ale_completion_max_suggestions = get(g:, 'ale_completion_max_suggestions', 50)

let s:timer_id = -1
let s:last_done_pos = []

" CompletionItemKind values from the LSP protocol.
let s:LSP_COMPLETION_TEXT_KIND = 1
let s:LSP_COMPLETION_METHOD_KIND = 2
let s:LSP_COMPLETION_FUNCTION_KIND = 3
let s:LSP_COMPLETION_CONSTRUCTOR_KIND = 4
let s:LSP_COMPLETION_FIELD_KIND = 5
let s:LSP_COMPLETION_VARIABLE_KIND = 6
let s:LSP_COMPLETION_CLASS_KIND = 7
let s:LSP_COMPLETION_INTERFACE_KIND = 8
let s:LSP_COMPLETION_MODULE_KIND = 9
let s:LSP_COMPLETION_PROPERTY_KIND = 10
let s:LSP_COMPLETION_UNIT_KIND = 11
let s:LSP_COMPLETION_VALUE_KIND = 12
let s:LSP_COMPLETION_ENUM_KIND = 13
let s:LSP_COMPLETION_KEYWORD_KIND = 14
let s:LSP_COMPLETION_SNIPPET_KIND = 15
let s:LSP_COMPLETION_COLOR_KIND = 16
let s:LSP_COMPLETION_FILE_KIND = 17
let s:LSP_COMPLETION_REFERENCE_KIND = 18

let s:lisp_regex = '\v[a-zA-Z_\-][a-zA-Z_\-0-9]*$'

" Regular expressions for checking the characters in the line before where
" the insert cursor is. If one of these matches, we'll check for completions.
let s:should_complete_map = {
\   '<default>': '\v[a-zA-Z$_][a-zA-Z$_0-9]*$|\.$',
\   'clojure': s:lisp_regex,
\   'lisp': s:lisp_regex,
\   'typescript': '\v[a-zA-Z$_][a-zA-Z$_0-9]*$|\.$|''$|"$',
\   'rust': '\v[a-zA-Z$_][a-zA-Z$_0-9]*$|\.$|::$',
\}

" Regular expressions for finding the start column to replace with completion.
let s:omni_start_map = {
\   '<default>': '\v[a-zA-Z$_][a-zA-Z$_0-9]*$',
\}

" A map of exact characters for triggering LSP completions.
let s:trigger_character_map = {
\   '<default>': ['.'],
\   'typescript': ['.', '''', '"'],
\   'rust': ['.', '::'],
\}

function! s:GetFiletypeValue(map, filetype) abort
    for l:part in reverse(split(a:filetype, '\.'))
        let l:regex = get(a:map, l:part, [])

        if !empty(l:regex)
            return l:regex
        endif
    endfor

    " Use the default regex for other files.
    return a:map['<default>']
endfunction

" Check if we should look for completions for a language.
function! ale#completion#GetPrefix(filetype, line, column) abort
    let l:regex = s:GetFiletypeValue(s:should_complete_map, a:filetype)

    " The column we're using completions for is where we are inserting text,
    " like so:
    "   abc
    "      ^
    " So we need check the text in the column before that position.
    return matchstr(getline(a:line)[: a:column - 2], l:regex)
endfunction

function! ale#completion#GetTriggerCharacter(filetype, prefix) abort
    let l:char_list = s:GetFiletypeValue(s:trigger_character_map, a:filetype)

    if index(l:char_list, a:prefix) >= 0
        return a:prefix
    endif

    return ''
endfunction

function! ale#completion#Filter(buffer, filetype, suggestions, prefix) abort
    let l:excluded_words = ale#Var(a:buffer, 'completion_excluded_words')
    let l:triggers = s:GetFiletypeValue(s:trigger_character_map, a:filetype)

    " For completing...
    "   foo.
    "       ^
    " We need to include all of the given suggestions.
    if index(l:triggers, a:prefix) >= 0
        let l:filtered_suggestions = a:suggestions
    else
        let l:filtered_suggestions = []

        " Filter suggestions down to those starting with the prefix we used for
        " finding suggestions in the first place.
        "
        " Some completion tools will include suggestions which don't even start
        " with the characters we have already typed.
        for l:item in a:suggestions
            " A List of String values or a List of completion item Dictionaries
            " is accepted here.
            let l:word = type(l:item) is v:t_string ? l:item : l:item.word

            " Add suggestions if the suggestion starts with a case-insensitive
            " match for the prefix.
            if l:word[: len(a:prefix) - 1] is? a:prefix
                call add(l:filtered_suggestions, l:item)
            endif
        endfor
    endif

    if !empty(l:excluded_words)
        " Copy the List if needed. We don't want to modify the argument.
        " We shouldn't make a copy if we don't need to.
        if l:filtered_suggestions is a:suggestions
            let l:filtered_suggestions = copy(a:suggestions)
        endif

        " Remove suggestions with words in the exclusion List.
        call filter(
        \   l:filtered_suggestions,
        \   'index(l:excluded_words, type(v:val) is v:t_string ? v:val : v:val.word) < 0',
        \)
    endif

    return l:filtered_suggestions
endfunction

function! s:ReplaceCompletionOptions() abort
    " Remember the old omnifunc value, if there is one.
    " If we don't store an old one, we'll just never reset the option.
    " This will stop some random exceptions from appearing.
    if !exists('b:ale_old_omnifunc') && !empty(&l:omnifunc)
        let b:ale_old_omnifunc = &l:omnifunc
    endif

    let &l:omnifunc = 'ale#completion#OmniFunc'

    if !exists('b:ale_old_completopt')
        let b:ale_old_completopt = &l:completeopt
    endif

    if &l:completeopt =~# 'preview'
        let &l:completeopt = 'menu,menuone,preview,noselect,noinsert'
    else
        let &l:completeopt = 'menu,menuone,noselect,noinsert'
    endif
endfunction

function! ale#completion#RestoreCompletionOptions() abort
    " Reset settings when completion is done.
    if exists('b:ale_old_omnifunc')
        if b:ale_old_omnifunc isnot# 'pythoncomplete#Complete'
            let &l:omnifunc = b:ale_old_omnifunc
        endif

        unlet b:ale_old_omnifunc
    endif

    if exists('b:ale_old_completopt')
        let &l:completeopt = b:ale_old_completopt
        unlet b:ale_old_completopt
    endif
endfunction

function! ale#completion#OmniFunc(findstart, base) abort
    if a:findstart
        let l:line = b:ale_completion_info.line
        let l:column = b:ale_completion_info.column
        let l:regex = s:GetFiletypeValue(s:omni_start_map, &filetype)
        let l:up_to_column = getline(l:line)[: l:column - 2]
        let l:match = matchstr(l:up_to_column, l:regex)

        return l:column - len(l:match) - 1
    else
        " Parse a new response if there is one.
        if exists('b:ale_completion_response')
        \&& exists('b:ale_completion_parser')
            let l:response = b:ale_completion_response
            let l:parser = b:ale_completion_parser

            unlet b:ale_completion_response
            unlet b:ale_completion_parser

            let b:ale_completion_result = function(l:parser)(l:response)
        endif

        call s:ReplaceCompletionOptions()

        return get(b:, 'ale_completion_result', [])
    endif
endfunction

function! ale#completion#Show(response, completion_parser) abort
    if ale#util#Mode() isnot# 'i'
        return
    endif

    " Set the list in the buffer, temporarily replace omnifunc with our
    " function, and then start omni-completion.
    let b:ale_completion_response = a:response
    let b:ale_completion_parser = a:completion_parser
    " Replace completion options shortly before opening the menu.
    call s:ReplaceCompletionOptions()

    call timer_start(0, {-> ale#util#FeedKeys("\<Plug>(ale_show_completion_menu)")})
endfunction

function! s:CompletionStillValid(request_id) abort
    let [l:line, l:column] = getcurpos()[1:2]

    return ale#util#Mode() is# 'i'
    \&& has_key(b:, 'ale_completion_info')
    \&& b:ale_completion_info.request_id == a:request_id
    \&& b:ale_completion_info.line == l:line
    \&& b:ale_completion_info.column == l:column
endfunction

function! ale#completion#ParseTSServerCompletions(response) abort
    let l:names = []

    for l:suggestion in a:response.body
        call add(l:names, l:suggestion.name)
    endfor

    return l:names
endfunction

function! ale#completion#ParseTSServerCompletionEntryDetails(response) abort
    let l:buffer = bufnr('')
    let l:results = []
    let l:names_with_details = []

    for l:suggestion in a:response.body
        let l:displayParts = []

        for l:part in l:suggestion.displayParts
            call add(l:displayParts, l:part.text)
        endfor

        " Each one of these parts has 'kind' properties
        let l:documentationParts = []

        for l:part in get(l:suggestion, 'documentation', [])
            call add(l:documentationParts, l:part.text)
        endfor

        if l:suggestion.kind is# 'className'
            let l:kind = 'f'
        elseif l:suggestion.kind is# 'parameterName'
            let l:kind = 'f'
        else
            let l:kind = 'v'
        endif

        " See :help complete-items
        call add(l:results, {
        \   'word': l:suggestion.name,
        \   'kind': l:kind,
        \   'icase': 1,
        \   'menu': join(l:displayParts, ''),
        \   'info': join(l:documentationParts, ''),
        \})
    endfor

    let l:names = getbufvar(l:buffer, 'ale_tsserver_completion_names', [])

    if !empty(l:names) && len(l:names) != len(l:results)
        let l:names_with_details = map(copy(l:results), 'v:val.word')
        let l:missing_names = filter(
        \   copy(l:names),
        \   'index(l:names_with_details, v:val) < 0',
        \)

        for l:name in l:missing_names
            call add(l:results, {
            \   'word': l:name,
            \   'kind': 'v',
            \   'icase': 1,
            \   'menu': '',
            \   'info': '',
            \})
        endfor
    endif

    return l:results
endfunction

function! ale#completion#NullFilter(buffer, item) abort
   return 1
endfunction

function! ale#completion#ParseLSPCompletions(response) abort
    let l:buffer = bufnr('')
    let l:info = get(b:, 'ale_completion_info', {})
    let l:Filter = get(l:info, 'completion_filter', v:null)

    if l:Filter is v:null
        let l:Filter = function('ale#completion#NullFilter')
    else
        let l:Filter = ale#util#GetFunction(l:Filter)
    endif

    let l:item_list = []

    if type(get(a:response, 'result')) is v:t_list
        let l:item_list = a:response.result
    elseif type(get(a:response, 'result')) is v:t_dict
    \&& type(get(a:response.result, 'items')) is v:t_list
        let l:item_list = a:response.result.items
    endif

    let l:results = []

    for l:item in l:item_list
        if !call(l:Filter, [l:buffer, l:item])
            continue
        endif

        let l:word = matchstr(l:item.label, '\v^[^(]+')

        if empty(l:word)
            continue
        endif

        " See :help complete-items for Vim completion kinds
        if !has_key(l:item, 'kind')
            let l:kind = 'v'
        elseif l:item.kind is s:LSP_COMPLETION_METHOD_KIND
            let l:kind = 'm'
        elseif l:item.kind is s:LSP_COMPLETION_CONSTRUCTOR_KIND
            let l:kind = 'm'
        elseif l:item.kind is s:LSP_COMPLETION_FUNCTION_KIND
            let l:kind = 'f'
        elseif l:item.kind is s:LSP_COMPLETION_CLASS_KIND
            let l:kind = 'f'
        elseif l:item.kind is s:LSP_COMPLETION_INTERFACE_KIND
            let l:kind = 'f'
        else
            let l:kind = 'v'
        endif

        let l:doc = get(l:item, 'documentation', '')

        if type(l:doc) is v:t_dict && has_key(l:doc, 'value')
            let l:doc = l:doc.value
        endif

        call add(l:results, {
        \   'word': l:word,
        \   'kind': l:kind,
        \   'icase': 1,
        \   'menu': get(l:item, 'detail', ''),
        \   'info': (type(l:doc) is v:t_string ? l:doc : ''),
        \})
    endfor

    if has_key(l:info, 'prefix')
        return ale#completion#Filter(l:buffer, &filetype, l:results, l:info.prefix)
    endif

    return l:results
endfunction

function! ale#completion#HandleTSServerResponse(conn_id, response) abort
    if !s:CompletionStillValid(get(a:response, 'request_seq'))
        return
    endif

    if !has_key(a:response, 'body')
        return
    endif

    let l:buffer = bufnr('')
    let l:command = get(a:response, 'command', '')

    if l:command is# 'completions'
        let l:names = ale#completion#Filter(
        \   l:buffer,
        \   &filetype,
        \   ale#completion#ParseTSServerCompletions(a:response),
        \   b:ale_completion_info.prefix,
        \)[: g:ale_completion_max_suggestions - 1]

        " We need to remember some names for tsserver, as it doesn't send
        " details back for everything we send.
        call setbufvar(l:buffer, 'ale_tsserver_completion_names', l:names)

        if !empty(l:names)
            let b:ale_completion_info.request_id = ale#lsp#Send(
            \   b:ale_completion_info.conn_id,
            \   ale#lsp#tsserver_message#CompletionEntryDetails(
            \       l:buffer,
            \       b:ale_completion_info.line,
            \       b:ale_completion_info.column,
            \       l:names,
            \   ),
            \)
        endif
    elseif l:command is# 'completionEntryDetails'
        call ale#completion#Show(
        \   a:response,
        \   'ale#completion#ParseTSServerCompletionEntryDetails',
        \)
    endif
endfunction


function! ale#completion#HandleLSPResponse(conn_id, response) abort
    if !s:CompletionStillValid(get(a:response, 'id'))
        return
    endif

    call ale#completion#Show(
    \   a:response,
    \   'ale#completion#ParseLSPCompletions',
    \)
endfunction

function! s:OnReady(linter, lsp_details, ...) abort
    let l:buffer = a:lsp_details.buffer
    let l:id = a:lsp_details.connection_id

    " If we have sent a completion request already, don't send another.
    if b:ale_completion_info.request_id
        return
    endif

    let l:Callback = a:linter.lsp is# 'tsserver'
    \   ? function('ale#completion#HandleTSServerResponse')
    \   : function('ale#completion#HandleLSPResponse')
    call ale#lsp#RegisterCallback(l:id, l:Callback)

    if a:linter.lsp is# 'tsserver'
        let l:message = ale#lsp#tsserver_message#Completions(
        \   l:buffer,
        \   b:ale_completion_info.line,
        \   b:ale_completion_info.column,
        \   b:ale_completion_info.prefix,
        \)
    else
        " Send a message saying the buffer has changed first, otherwise
        " completions won't know what text is nearby.
        call ale#lsp#NotifyForChanges(l:id, l:buffer)

        " For LSP completions, we need to clamp the column to the length of
        " the line. python-language-server and perhaps others do not implement
        " this correctly.
        let l:message = ale#lsp#message#Completion(
        \   l:buffer,
        \   b:ale_completion_info.line,
        \   min([
        \       b:ale_completion_info.line_length,
        \       b:ale_completion_info.column,
        \   ]),
        \   ale#completion#GetTriggerCharacter(&filetype, b:ale_completion_info.prefix),
        \)
    endif

    let l:request_id = ale#lsp#Send(l:id, l:message)

    if l:request_id
        let b:ale_completion_info.conn_id = l:id
        let b:ale_completion_info.request_id = l:request_id

        if has_key(a:linter, 'completion_filter')
            let b:ale_completion_info.completion_filter = a:linter.completion_filter
        endif
    endif
endfunction

function! s:GetLSPCompletions(linter) abort
    let l:buffer = bufnr('')
    let l:lsp_details = ale#lsp_linter#StartLSP(l:buffer, a:linter)

    if empty(l:lsp_details)
        return 0
    endif

    let l:id = l:lsp_details.connection_id

    let l:OnReady = function('s:OnReady', [a:linter, l:lsp_details])

    call ale#lsp#WaitForCapability(l:id, 'completion', l:OnReady)
endfunction

function! ale#completion#GetCompletions() abort
    if !g:ale_completion_enabled
        return
    endif

    let [l:line, l:column] = getcurpos()[1:2]

    let l:prefix = ale#completion#GetPrefix(&filetype, l:line, l:column)

    if empty(l:prefix)
        return
    endif

    let l:line_length = len(getline('.'))

    let b:ale_completion_info = {
    \   'line': l:line,
    \   'line_length': l:line_length,
    \   'column': l:column,
    \   'prefix': l:prefix,
    \   'conn_id': 0,
    \   'request_id': 0,
    \}

    for l:linter in ale#linter#Get(&filetype)
        if !empty(l:linter.lsp)
            call s:GetLSPCompletions(l:linter)
        endif
    endfor
endfunction

function! s:TimerHandler(...) abort
    let s:timer_id = -1

    let [l:line, l:column] = getcurpos()[1:2]

    " When running the timer callback, we have to be sure that the cursor
    " hasn't moved from where it was when we requested completions by typing.
    if s:timer_pos == [l:line, l:column] && ale#util#Mode() is# 'i'
        call ale#completion#GetCompletions()
    endif
endfunction

" Stop any completion timer that is queued. This is useful for tests.
function! ale#completion#StopTimer() abort
    if s:timer_id != -1
        call timer_stop(s:timer_id)
    endif

    let s:timer_id = -1
endfunction

function! ale#completion#Queue() abort
    if !g:ale_completion_enabled
        return
    endif

    let s:timer_pos = getcurpos()[1:2]

    if s:timer_pos == s:last_done_pos
        " Do not ask for completions if the cursor rests on the position we
        " last completed on.
        return
    endif

    " If we changed the text again while we're still waiting for a response,
    " then invalidate the requests before the timer ticks again.
    if exists('b:ale_completion_info')
        let b:ale_completion_info.request_id = 0
    endif

    call ale#completion#StopTimer()

    let s:timer_id = timer_start(g:ale_completion_delay, function('s:TimerHandler'))
endfunction

function! ale#completion#Done() abort
    silent! pclose

    call ale#completion#RestoreCompletionOptions()

    let s:last_done_pos = getcurpos()[1:2]
endfunction

function! s:Setup(enabled) abort
    augroup ALECompletionGroup
        autocmd!

        if a:enabled
            autocmd TextChangedI * call ale#completion#Queue()
            autocmd CompleteDone * call ale#completion#Done()
        endif
    augroup END

    if !a:enabled
        augroup! ALECompletionGroup
    endif
endfunction

function! ale#completion#Enable() abort
    let g:ale_completion_enabled = 1
    call s:Setup(1)
endfunction

function! ale#completion#Disable() abort
    let g:ale_completion_enabled = 0
    call s:Setup(0)
endfunction
