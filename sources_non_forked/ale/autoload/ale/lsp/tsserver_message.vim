" Author: w0rp <devw0rp@gmail.com>
" Description: tsserver message implementations
"
" Messages in this movie will be returned in the format
" [is_notification, command_name, params?]
"
" Every command must begin with the string 'ts@', which will be used to
" detect the different message format for tsserver, and this string will
" be removed from the actual command name,

function! ale#lsp#tsserver_message#Open(buffer) abort
    return [1, 'ts@open', {'file': expand('#' . a:buffer . ':p')}]
endfunction

function! ale#lsp#tsserver_message#Close(buffer) abort
    return [1, 'ts@close', {'file': expand('#' . a:buffer . ':p')}]
endfunction

function! ale#lsp#tsserver_message#Change(buffer) abort
    let l:lines = getbufline(a:buffer, 1, '$')

    " We will always use a very high endLine number, so we can delete
    " lines from files. tsserver will gladly accept line numbers beyond the
    " end.
    return [1, 'ts@change', {
    \   'file': expand('#' . a:buffer . ':p'),
    \   'line': 1,
    \   'offset': 1,
    \   'endLine': 1073741824,
    \   'endOffset': 1,
    \   'insertString': join(l:lines, "\n") . "\n",
    \}]
endfunction

function! ale#lsp#tsserver_message#Geterr(buffer) abort
    return [1, 'ts@geterr', {'files': [expand('#' . a:buffer . ':p')]}]
endfunction

function! ale#lsp#tsserver_message#Completions(buffer, line, column, prefix) abort
    return [0, 'ts@completions', {
    \   'line': a:line,
    \   'offset': a:column,
    \   'file': expand('#' . a:buffer . ':p'),
    \   'prefix': a:prefix,
    \}]
endfunction

function! ale#lsp#tsserver_message#CompletionEntryDetails(buffer, line, column, entry_names) abort
    return [0, 'ts@completionEntryDetails', {
    \   'line': a:line,
    \   'offset': a:column,
    \   'file': expand('#' . a:buffer . ':p'),
    \   'entryNames': a:entry_names,
    \}]
endfunction

function! ale#lsp#tsserver_message#Definition(buffer, line, column) abort
    return [0, 'ts@definition', {
    \   'line': a:line,
    \   'offset': a:column,
    \   'file': expand('#' . a:buffer . ':p'),
    \}]
endfunction

function! ale#lsp#tsserver_message#References(buffer, line, column) abort
    return [0, 'ts@references', {
    \   'line': a:line,
    \   'offset': a:column,
    \   'file': expand('#' . a:buffer . ':p'),
    \}]
endfunction

function! ale#lsp#tsserver_message#Quickinfo(buffer, line, column) abort
    return [0, 'ts@quickinfo', {
    \   'line': a:line,
    \   'offset': a:column,
    \   'file': expand('#' . a:buffer . ':p'),
    \}]
endfunction
