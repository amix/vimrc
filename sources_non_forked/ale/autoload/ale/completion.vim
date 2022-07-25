" Author: w0rp <devw0rp@gmail.com>
" Description: Completion support for LSP linters
scriptencoding utf-8

" The omnicompletion menu is shown through a special Plug mapping which is
" only valid in Insert mode. This way, feedkeys() won't send these keys if you
" quit Insert mode quickly enough.
inoremap <silent> <Plug>(ale_show_completion_menu) <C-x><C-o><C-p>
" If we hit the key sequence in normal mode, then we won't show the menu, so
" we should restore the old settings right away.
nnoremap <silent> <Plug>(ale_show_completion_menu) :call ale#completion#RestoreCompletionOptions()<CR>
cnoremap <silent> <Plug>(ale_show_completion_menu) <Nop>
vnoremap <silent> <Plug>(ale_show_completion_menu) <Nop>
onoremap <silent> <Plug>(ale_show_completion_menu) <Nop>

let g:ale_completion_delay = get(g:, 'ale_completion_delay', 100)
let g:ale_completion_excluded_words = get(g:, 'ale_completion_excluded_words', [])
let g:ale_completion_max_suggestions = get(g:, 'ale_completion_max_suggestions', 50)
let g:ale_completion_autoimport = get(g:, 'ale_completion_autoimport', 0)
let g:ale_completion_tsserver_remove_warnings = get(g:, 'ale_completion_tsserver_remove_warnings', 0)

let s:timer_id = -1
let s:last_done_pos = []

" CompletionItemKind values from the LSP protocol.
let g:ale_lsp_types = {
\ 1: 'text',
\ 2: 'method',
\ 3: 'function',
\ 4: 'constructor',
\ 5: 'field',
\ 6: 'variable',
\ 7: 'class',
\ 8: 'interface',
\ 9: 'module',
\ 10: 'property',
\ 11: 'unit',
\ 12: 'value',
\ 13: 'enum',
\ 14: 'keyword',
\ 15: 'snippet',
\ 16: 'color',
\ 17: 'file',
\ 18: 'reference',
\ 19: 'folder',
\ 20: 'enum_member',
\ 21: 'constant',
\ 22: 'struct',
\ 23: 'event',
\ 24: 'operator',
\ 25: 'type_parameter',
\ }

" from https://github.com/microsoft/TypeScript/blob/29becf05012bfa7ba20d50b0d16813971e46b8a6/lib/protocol.d.ts#L2472
let g:ale_tsserver_types = {
\ 'warning': 'text',
\ 'keyword': 'keyword',
\ 'script': 'file',
\ 'module': 'module',
\ 'class': 'class',
\ 'local class': 'class',
\ 'interface': 'interface',
\ 'type': 'class',
\ 'enum': 'enum',
\ 'enum member': 'enum_member',
\ 'var': 'variable',
\ 'local var': 'variable',
\ 'function': 'function',
\ 'local function': 'function',
\ 'method': 'method',
\ 'getter': 'property',
\ 'setter': 'method',
\ 'property': 'property',
\ 'constructor': 'constructor',
\ 'call': 'method',
\ 'index': 'index',
\ 'construct': 'constructor',
\ 'parameter': 'parameter',
\ 'type parameter': 'type_parameter',
\ 'primitive type': 'unit',
\ 'label': 'text',
\ 'alias': 'class',
\ 'const': 'constant',
\ 'let': 'variable',
\ 'directory': 'folder',
\ 'external module name': 'text',
\ 'JSX attribute': 'parameter',
\ 'string': 'text'
\ }

" For compatibility reasons, we only use built in VIM completion kinds
" See :help complete-items for Vim completion kinds
let g:ale_completion_symbols = get(g:, 'ale_completion_symbols', {
\ 'text': 'v',
\ 'method': 'f',
\ 'function': 'f',
\ 'constructor': 'f',
\ 'field': 'm',
\ 'variable': 'v',
\ 'class': 't',
\ 'interface': 't',
\ 'module': 'd',
\ 'property': 'm',
\ 'unit': 'v',
\ 'value': 'v',
\ 'enum': 't',
\ 'keyword': 'v',
\ 'snippet': 'v',
\ 'color': 'v',
\ 'file': 'v',
\ 'reference': 'v',
\ 'folder': 'v',
\ 'enum_member': 'm',
\ 'constant': 'm',
\ 'struct': 't',
\ 'event': 'v',
\ 'operator': 'f',
\ 'type_parameter': 'p',
\ '<default>': 'v'
\ })

let s:LSP_INSERT_TEXT_FORMAT_PLAIN = 1
let s:LSP_INSERT_TEXT_FORMAT_SNIPPET = 2

let s:lisp_regex = '\v[a-zA-Z_\-][a-zA-Z_\-0-9]*$'

" Regular expressions for checking the characters in the line before where
" the insert cursor is. If one of these matches, we'll check for completions.
let s:should_complete_map = {
\   '<default>': '\v[a-zA-Z$_][a-zA-Z$_0-9]*$|\.$',
\   'clojure': s:lisp_regex,
\   'lisp': s:lisp_regex,
\   'typescript': '\v[a-zA-Z$_][a-zA-Z$_0-9]*$|\.$|''$|"$',
\   'rust': '\v[a-zA-Z$_][a-zA-Z$_0-9]*$|\.$|::$',
\   'cpp': '\v[a-zA-Z$_][a-zA-Z$_0-9]*$|\.$|::$|-\>$',
\}

" Regular expressions for finding the start column to replace with completion.
let s:omni_start_map = {
\   '<default>': '\v[a-zA-Z$_][a-zA-Z$_0-9]*$',
\}

" A map of exact characters for triggering LSP completions. Do not forget to
" update self.input_patterns in ale.py in updating entries in this map.
let s:trigger_character_map = {
\   '<default>': ['.'],
\   'typescript': ['.', '''', '"'],
\   'rust': ['.', '::'],
\   'cpp': ['.', '::', '->'],
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
    if empty(a:prefix)
        return ''
    endif

    let l:char_list = s:GetFiletypeValue(s:trigger_character_map, a:filetype)

    if index(l:char_list, a:prefix) >= 0
        return a:prefix
    endif

    return ''
endfunction

function! ale#completion#Filter(
\   buffer,
\   filetype,
\   suggestions,
\   prefix,
\   exact_prefix_match,
\) abort
    let l:excluded_words = ale#Var(a:buffer, 'completion_excluded_words')

    if empty(a:prefix)
        let l:filtered_suggestions = a:suggestions
    else
        let l:triggers = s:GetFiletypeValue(s:trigger_character_map, a:filetype)

        " For completing...
        "   foo.
        "       ^
        " We need to include all of the given suggestions.
        if index(l:triggers, a:prefix) >= 0 || empty(a:prefix)
            let l:filtered_suggestions = a:suggestions
        else
            let l:filtered_suggestions = []

            " Filter suggestions down to those starting with the prefix we
            " used for finding suggestions in the first place.
            "
            " Some completion tools will include suggestions which don't even
            " start with the characters we have already typed.
            for l:item in a:suggestions
                " A List of String values or a List of completion item
                " Dictionaries is accepted here.
                let l:word = type(l:item) is v:t_string ? l:item : l:item.word

                if a:exact_prefix_match
                    " Add suggestions if the word is an exact match.
                    if l:word is# a:prefix
                        call add(l:filtered_suggestions, l:item)
                    endif
                else
                    " Add suggestions if the suggestion starts with a
                    " case-insensitive match for the prefix.
                    if l:word[: len(a:prefix) - 1] is? a:prefix
                        call add(l:filtered_suggestions, l:item)
                    endif
                endif
            endfor
        endif
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

function! s:ReplaceCompletionOptions(source) abort
    " Remember the old omnifunc value, if there is one.
    " If we don't store an old one, we'll just never reset the option.
    " This will stop some random exceptions from appearing.
    if !exists('b:ale_old_omnifunc') && !empty(&l:omnifunc)
        let b:ale_old_omnifunc = &l:omnifunc
    endif

    let &l:omnifunc = 'ale#completion#AutomaticOmniFunc'

    if a:source is# 'ale-automatic'
        if !exists('b:ale_old_completeopt')
            let b:ale_old_completeopt = &l:completeopt
        endif

        let l:opt_list = split(&l:completeopt, ',')
        " The menu and noinsert options must be set, or automatic completion
        " will be annoying.
        let l:new_opt_list = ['menu', 'menuone', 'noinsert']

        " Permit some other completion options, provided users have set them.
        for l:opt in ['preview', 'popup', 'noselect']
            if index(l:opt_list, l:opt) >= 0
                call add(l:new_opt_list, l:opt)
            endif
        endfor

        let &l:completeopt = join(l:new_opt_list, ',')
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

    if exists('b:ale_old_completeopt')
        let &l:completeopt = b:ale_old_completeopt
        unlet b:ale_old_completeopt
    endif
endfunction

function! ale#completion#GetCompletionPosition() abort
    if !exists('b:ale_completion_info')
        return 0
    endif

    let l:line = b:ale_completion_info.line
    let l:column = b:ale_completion_info.column
    let l:regex = s:GetFiletypeValue(s:omni_start_map, &filetype)
    let l:up_to_column = getline(l:line)[: l:column - 2]
    let l:match = matchstr(l:up_to_column, l:regex)

    return l:column - len(l:match) - 1
endfunction

function! ale#completion#GetCompletionPositionForDeoplete(input) abort
    return match(a:input, '\k*$')
endfunction

function! ale#completion#GetCompletionResult() abort
    if exists('b:ale_completion_result')
        return b:ale_completion_result
    endif

    return v:null
endfunction

function! ale#completion#AutomaticOmniFunc(findstart, base) abort
    if a:findstart
        return ale#completion#GetCompletionPosition()
    else
        let l:result = ale#completion#GetCompletionResult()

        let l:source = get(get(b:, 'ale_completion_info', {}), 'source', '')

        if l:source is# 'ale-automatic' || l:source is# 'ale-manual'
            call s:ReplaceCompletionOptions(l:source)
        endif

        return l:result isnot v:null ? l:result : []
    endif
endfunction

function! s:OpenCompletionMenu(...) abort
    if !&l:paste
        call ale#util#FeedKeys("\<Plug>(ale_show_completion_menu)")
    endif
endfunction

function! ale#completion#Show(result) abort
    let l:source = get(get(b:, 'ale_completion_info', {}), 'source', '')

    if ale#util#Mode() isnot# 'i' && l:source isnot# 'ale-import'
        return
    endif

    " Set the list in the buffer.
    let b:ale_completion_result = a:result

    " Don't try to open the completion menu if there's nothing to show.
    if empty(b:ale_completion_result)
        if l:source is# 'ale-import'
            " If we ran completion from :ALEImport,
            " tell the user that nothing is going to happen.
            call s:message('No possible imports found.')
        endif

        return
    endif

    " Replace completion options shortly before opening the menu.
    if l:source is# 'ale-automatic' || l:source is# 'ale-manual'
        call s:ReplaceCompletionOptions(l:source)

        call timer_start(0, function('s:OpenCompletionMenu'))
    endif

    if l:source is# 'ale-callback'
        call b:CompleteCallback(b:ale_completion_result)
    endif

    if l:source is# 'ale-import'
        call ale#completion#HandleUserData(b:ale_completion_result[0])

        let l:text_changed = '' . g:ale_lint_on_text_changed

        " Check the buffer again right away, if linting is enabled.
        if g:ale_enabled
        \&& (
        \   l:text_changed is# '1'
        \   || l:text_changed is# 'always'
        \   || l:text_changed is# 'normal'
        \   || l:text_changed is# 'insert'
        \)
            call ale#Queue(0, '')
        endif
    endif
endfunction

function! ale#completion#GetAllTriggers() abort
    return deepcopy(s:trigger_character_map)
endfunction

function! ale#completion#GetCompletionKind(kind) abort
    let l:lsp_symbol = get(g:ale_lsp_types, a:kind, '')

    if !empty(l:lsp_symbol)
        return l:lsp_symbol
    endif

    return get(g:ale_tsserver_types, a:kind, '')
endfunction

function! ale#completion#GetCompletionSymbols(kind) abort
    let l:kind = ale#completion#GetCompletionKind(a:kind)
    let l:symbol = get(g:ale_completion_symbols, l:kind, '')

    if !empty(l:symbol)
        return l:symbol
    endif

    return get(g:ale_completion_symbols, '<default>', 'v')
endfunction

function! s:CompletionStillValid(request_id) abort
    let [l:line, l:column] = getpos('.')[1:2]

    return has_key(b:, 'ale_completion_info')
    \&& (
    \   ale#util#Mode() is# 'i'
    \   || b:ale_completion_info.source is# 'ale-import'
    \)
    \&& b:ale_completion_info.request_id == a:request_id
    \&& b:ale_completion_info.line == l:line
    \&& (
    \   b:ale_completion_info.column == l:column
    \   || b:ale_completion_info.source is# 'ale-omnifunc'
    \   || b:ale_completion_info.source is# 'ale-callback'
    \   || b:ale_completion_info.source is# 'ale-import'
    \)
endfunction

function! ale#completion#ParseTSServerCompletions(response) abort
    let l:names = []

    for l:suggestion in a:response.body
        let l:kind = get(l:suggestion, 'kind', '')

        if g:ale_completion_tsserver_remove_warnings == 0 || l:kind isnot# 'warning'
            call add(l:names, {
            \ 'word': l:suggestion.name,
            \ 'source': get(l:suggestion, 'source', ''),
            \})
        endif
    endfor

    return l:names
endfunction

function! ale#completion#ParseTSServerCompletionEntryDetails(response) abort
    let l:buffer = bufnr('')
    let l:results = []
    let l:names_with_details = []
    let l:info = get(b:, 'ale_completion_info', {})

    for l:suggestion in a:response.body
        let l:displayParts = []
        let l:local_name = v:null

        for l:action in get(l:suggestion, 'codeActions', [])
            call add(l:displayParts, l:action.description . ' ')
        endfor

        for l:part in l:suggestion.displayParts
            " Stop on stop on line breaks for the menu.
            if get(l:part, 'kind') is# 'lineBreak'
                break
            endif

            if get(l:part, 'kind') is# 'localName'
                let l:local_name = l:part.text
            endif

            call add(l:displayParts, l:part.text)
        endfor

        " Each one of these parts has 'kind' properties
        let l:documentationParts = []

        for l:part in get(l:suggestion, 'documentation', [])
            call add(l:documentationParts, l:part.text)
        endfor

        " See :help complete-items
        let l:result = {
        \   'word': (
        \       l:suggestion.name is# 'default'
        \       && l:suggestion.kind is# 'alias'
        \       && !empty(l:local_name)
        \           ? l:local_name
        \           : l:suggestion.name
        \   ),
        \   'kind': ale#completion#GetCompletionSymbols(l:suggestion.kind),
        \   'icase': 1,
        \   'menu': join(l:displayParts, ''),
        \   'dup': get(l:info, 'additional_edits_only', 0)
        \       ||  g:ale_completion_autoimport,
        \   'info': join(l:documentationParts, ''),
        \}
        " This flag is used to tell if this completion came from ALE or not.
        let l:user_data = {'_ale_completion_item': 1}

        if has_key(l:suggestion, 'codeActions')
            let l:user_data.code_actions = l:suggestion.codeActions
        endif

        let l:result.user_data = json_encode(l:user_data)

        " Include this item if we'll accept any items,
        " or if we only want items with additional edits, and this has them.
        if !get(l:info, 'additional_edits_only', 0)
        \|| has_key(l:user_data, 'code_actions')
            call add(l:results, l:result)
        endif
    endfor

    let l:names = getbufvar(l:buffer, 'ale_tsserver_completion_names', [])

    if !empty(l:names) && len(l:names) != len(l:results)
        let l:names_with_details = map(copy(l:results), 'v:val.word')
        let l:missing_names = filter(
        \   copy(l:names),
        \   'index(l:names_with_details, v:val.word) < 0',
        \)

        for l:name in l:missing_names
            call add(l:results, {
            \   'word': l:name.word,
            \   'kind': 'v',
            \   'icase': 1,
            \   'menu': '',
            \   'info': '',
            \   'user_data': json_encode({'_ale_completion_item': 1}),
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

        if get(l:item, 'insertTextFormat') is s:LSP_INSERT_TEXT_FORMAT_PLAIN
        \&& type(get(l:item, 'textEdit')) is v:t_dict
            let l:text = l:item.textEdit.newText
        elseif type(get(l:item, 'insertText')) is v:t_string
            let l:text = l:item.insertText
        else
            let l:text = l:item.label
        endif

        let l:word = matchstr(l:text, '\v^[^(]+')

        if empty(l:word)
            continue
        endif

        " Don't use LSP items with additional text edits when autoimport for
        " completions is turned off.
        if !empty(get(l:item, 'additionalTextEdits'))
        \&& !(
        \   get(l:info, 'additional_edits_only', 0)
        \   || g:ale_completion_autoimport
        \)
            continue
        endif

        let l:doc = get(l:item, 'documentation', '')

        if type(l:doc) is v:t_dict && has_key(l:doc, 'value')
            let l:doc = l:doc.value
        endif

        " Collapse whitespaces and line breaks into a single space.
        let l:detail = substitute(get(l:item, 'detail', ''), '\_s\+', ' ', 'g')

        let l:result = {
        \   'word': l:word,
        \   'kind': ale#completion#GetCompletionSymbols(get(l:item, 'kind', '')),
        \   'icase': 1,
        \   'menu': l:detail,
        \   'dup': get(l:info, 'additional_edits_only', 0)
        \       ||  g:ale_completion_autoimport,
        \   'info': (type(l:doc) is v:t_string ? l:doc : ''),
        \}
        " This flag is used to tell if this completion came from ALE or not.
        let l:user_data = {'_ale_completion_item': 1}

        if has_key(l:item, 'additionalTextEdits')
        \ && l:item.additionalTextEdits isnot v:null
            let l:text_changes = []

            for l:edit in l:item.additionalTextEdits
                call add(l:text_changes, {
                \ 'start': {
                \   'line': l:edit.range.start.line + 1,
                \   'offset': l:edit.range.start.character + 1,
                \ },
                \ 'end': {
                \   'line': l:edit.range.end.line + 1,
                \   'offset': l:edit.range.end.character + 1,
                \ },
                \ 'newText': l:edit.newText,
                \})
            endfor

            if !empty(l:text_changes)
                let l:user_data.code_actions = [{
                \   'description': 'completion',
                \   'changes': [
                \       {
                \           'fileName': expand('#' . l:buffer . ':p'),
                \           'textChanges': l:text_changes,
                \       },
                \   ],
                \}]
            endif
        endif

        let l:result.user_data = json_encode(l:user_data)

        " Include this item if we'll accept any items,
        " or if we only want items with additional edits, and this has them.
        if !get(l:info, 'additional_edits_only', 0)
        \|| has_key(l:user_data, 'code_actions')
            call add(l:results, l:result)
        endif
    endfor

    if has_key(l:info, 'prefix')
        let l:results = ale#completion#Filter(
        \   l:buffer,
        \   &filetype,
        \   l:results,
        \   l:info.prefix,
        \   get(l:info, 'additional_edits_only', 0),
        \)
    endif

    return l:results[: g:ale_completion_max_suggestions - 1]
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
        \   get(b:ale_completion_info, 'additional_edits_only', 0),
        \)[: g:ale_completion_max_suggestions - 1]

        " We need to remember some names for tsserver, as it doesn't send
        " details back for everything we send.
        call setbufvar(l:buffer, 'ale_tsserver_completion_names', l:names)

        if empty(l:names)
            " Response with no results now and skip making a redundant request
            " for nothing.
            call ale#completion#Show([])
        else
            let l:identifiers = []

            for l:name in l:names
                let l:identifier = {
                \   'name': l:name.word,
                \}
                let l:source = get(l:name, 'source', '')

                " Empty source results in no details for the completed item
                if !empty(l:source)
                    call extend(l:identifier, { 'source': l:source })
                endif

                call add(l:identifiers, l:identifier)
            endfor

            let b:ale_completion_info.request_id = ale#lsp#Send(
            \   b:ale_completion_info.conn_id,
            \   ale#lsp#tsserver_message#CompletionEntryDetails(
            \       l:buffer,
            \       b:ale_completion_info.line,
            \       b:ale_completion_info.column,
            \       l:identifiers,
            \   ),
            \)
        endif
    elseif l:command is# 'completionEntryDetails'
        call ale#completion#Show(
        \   ale#completion#ParseTSServerCompletionEntryDetails(a:response),
        \)
    endif
endfunction


function! ale#completion#HandleLSPResponse(conn_id, response) abort
    if !s:CompletionStillValid(get(a:response, 'id'))
        return
    endif

    call ale#completion#Show(
    \   ale#completion#ParseLSPCompletions(a:response),
    \)
endfunction

function! s:OnReady(linter, lsp_details) abort
    let l:id = a:lsp_details.connection_id

    if !ale#lsp#HasCapability(l:id, 'completion')
        return
    endif

    let l:buffer = a:lsp_details.buffer

    " If we have sent a completion request already, don't send another.
    if b:ale_completion_info.request_id
        return
    endif

    let l:Callback = a:linter.lsp is# 'tsserver'
    \   ? function('ale#completion#HandleTSServerResponse')
    \   : function('ale#completion#HandleLSPResponse')
    call ale#lsp#RegisterCallback(l:id, l:Callback)

    if a:linter.lsp is# 'tsserver'
        if get(g:, 'ale_completion_tsserver_autoimport') is 1
            execute 'echom `g:ale_completion_tsserver_autoimport` is deprecated. Use `g:ale_completion_autoimport` instead.'''
        endif

        let l:message = ale#lsp#tsserver_message#Completions(
        \   l:buffer,
        \   b:ale_completion_info.line,
        \   b:ale_completion_info.column,
        \   b:ale_completion_info.prefix,
        \   get(b:ale_completion_info, 'additional_edits_only', 0)
        \       || g:ale_completion_autoimport,
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
        \   b:ale_completion_info.column,
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

" This function can be called to check if ALE can provide completion data for
" the current buffer. 1 will be returned if there's a potential source of
" completion data ALE can use, and 0 will be returned otherwise.
function! ale#completion#CanProvideCompletions() abort
    for l:linter in ale#linter#Get(&filetype)
        if !empty(l:linter.lsp)
            return 1
        endif
    endfor

    return 0
endfunction

" This function can be used to manually trigger autocomplete, even when
" g:ale_completion_enabled is set to false
function! ale#completion#GetCompletions(...) abort
    let l:source = get(a:000, 0, '')
    let l:options = get(a:000, 1, {})

    if len(a:000) > 2
        throw 'Too many arguments!'
    endif

    let l:CompleteCallback = get(l:options, 'callback', v:null)

    if l:CompleteCallback isnot v:null
        let b:CompleteCallback = l:CompleteCallback
    endif

    if has_key(l:options, 'line') && has_key(l:options, 'column')
        " Use a provided line and column, if given.
        let l:line = l:options.line
        let l:column = l:options.column
    else
        let [l:line, l:column] = getpos('.')[1:2]
    endif

    if has_key(l:options, 'prefix')
        let l:prefix = l:options.prefix
    else
        let l:prefix = ale#completion#GetPrefix(&filetype, l:line, l:column)
    endif

    if l:source is# 'ale-automatic' && empty(l:prefix)
        return 0
    endif

    let l:line_length = len(getline('.'))

    let b:ale_completion_info = {
    \   'line': l:line,
    \   'line_length': l:line_length,
    \   'column': l:column,
    \   'prefix': l:prefix,
    \   'conn_id': 0,
    \   'request_id': 0,
    \   'source': l:source,
    \}
    unlet! b:ale_completion_result

    if has_key(l:options, 'additional_edits_only')
        let b:ale_completion_info.additional_edits_only =
        \   l:options.additional_edits_only
    endif

    let l:buffer = bufnr('')
    let l:Callback = function('s:OnReady')

    let l:started = 0

    for l:linter in ale#linter#Get(&filetype)
        if !empty(l:linter.lsp)
            if ale#lsp_linter#StartLSP(l:buffer, l:linter, l:Callback)
                let l:started = 1
            endif
        endif
    endfor

    return l:started
endfunction

function! s:message(message) abort
    call ale#util#Execute('echom ' . string(a:message))
endfunction

" This function implements the :ALEImport command.
function! ale#completion#Import() abort
    let l:word = expand('<cword>')

    if empty(l:word)
        call s:message('Nothing to complete at cursor!')

        return
    endif

    let [l:line, l:column] = getpos('.')[1:2]
    let l:column = searchpos('\V' . escape(l:word, '/\'), 'bn', l:line)[1]

    if l:column isnot 0
        let l:started = ale#completion#GetCompletions('ale-import', {
        \   'line': l:line,
        \   'column': l:column,
        \   'prefix': l:word,
        \   'additional_edits_only': 1,
        \})

        if !l:started
            call s:message('No completion providers are available.')
        endif
    endif
endfunction

function! ale#completion#OmniFunc(findstart, base) abort
    if a:findstart
        let l:started = ale#completion#GetCompletions('ale-omnifunc')

        if !l:started
            " This is the special value for cancelling completions silently.
            " See :help complete-functions
            return -3
        endif

        return ale#completion#GetCompletionPosition()
    else
        let l:result = ale#completion#GetCompletionResult()

        while l:result is v:null && !complete_check()
            sleep 2ms
            let l:result = ale#completion#GetCompletionResult()
        endwhile

        return l:result isnot v:null ? l:result : []
    endif
endfunction

function! s:TimerHandler(...) abort
    if !get(b:, 'ale_completion_enabled', g:ale_completion_enabled)
        return
    endif

    let s:timer_id = -1

    let [l:line, l:column] = getpos('.')[1:2]

    " When running the timer callback, we have to be sure that the cursor
    " hasn't moved from where it was when we requested completions by typing.
    if s:timer_pos == [l:line, l:column] && ale#util#Mode() is# 'i'
        call ale#completion#GetCompletions('ale-automatic')
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
    if !get(b:, 'ale_completion_enabled', g:ale_completion_enabled)
        return
    endif

    let s:timer_pos = getpos('.')[1:2]

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

function! ale#completion#HandleUserData(completed_item) abort
    let l:user_data_json = get(a:completed_item, 'user_data', '')
    let l:user_data = type(l:user_data_json) is v:t_dict
    \   ? l:user_data_json
    \   : ale#util#FuzzyJSONDecode(l:user_data_json, {})

    if !has_key(l:user_data, '_ale_completion_item')
        return
    endif

    let l:source = get(get(b:, 'ale_completion_info', {}), 'source', '')

    if l:source is# 'ale-automatic'
    \|| l:source is# 'ale-manual'
    \|| l:source is# 'ale-callback'
    \|| l:source is# 'ale-import'
    \|| l:source is# 'ale-omnifunc'
        for l:code_action in get(l:user_data, 'code_actions', [])
            call ale#code_action#HandleCodeAction(l:code_action, {})
        endfor
    endif

    silent doautocmd <nomodeline> User ALECompletePost
endfunction

function! ale#completion#Done() abort
    silent! pclose

    call ale#completion#RestoreCompletionOptions()

    let s:last_done_pos = getpos('.')[1:2]
endfunction

augroup ALECompletionActions
    autocmd!

    autocmd CompleteDone * call ale#completion#HandleUserData(v:completed_item)
augroup END

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
