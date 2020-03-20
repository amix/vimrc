" Author: w0rp <devw0rp@gmail.com>
" Description: Language Server Protocol message implementations
"
" Messages in this movie will be returned in the format
" [is_notification, method_name, params?]
"
" All functions which accept line and column arguments expect them to be 1-based
" (the same format as being returned by getpos() and friends), those then
" will be converted to 0-based as specified by LSP.
let g:ale_lsp_next_version_id = 1

" The LSP protocols demands that we send every change to a document, including
" undo, with incrementing version numbers, so we'll just use one incrementing
" ID for everything.
function! ale#lsp#message#GetNextVersionID() abort
    " Use the current ID
    let l:id = g:ale_lsp_next_version_id

    " Increment the ID variable.
    let g:ale_lsp_next_version_id += 1

    " When the ID overflows, reset it to 1. By the time we hit the initial ID
    " again, the messages will be long gone.
    if g:ale_lsp_next_version_id < 1
        let g:ale_lsp_next_version_id = 1
    endif

    return l:id
endfunction

function! ale#lsp#message#Initialize(root_path, options, capabilities) abort
    " NOTE: rootPath is deprecated in favour of rootUri
    return [0, 'initialize', {
    \   'processId': getpid(),
    \   'rootPath': a:root_path,
    \   'capabilities': a:capabilities,
    \   'initializationOptions': a:options,
    \   'rootUri': ale#path#ToURI(a:root_path),
    \}]
endfunction

function! ale#lsp#message#Initialized() abort
    return [1, 'initialized', {}]
endfunction

function! ale#lsp#message#Shutdown() abort
    return [0, 'shutdown']
endfunction

function! ale#lsp#message#Exit() abort
    return [1, 'exit']
endfunction

function! ale#lsp#message#DidOpen(buffer, language_id) abort
    let l:lines = getbufline(a:buffer, 1, '$')

    return [1, 'textDocument/didOpen', {
    \   'textDocument': {
    \       'uri': ale#path#ToURI(expand('#' . a:buffer . ':p')),
    \       'languageId': a:language_id,
    \       'version': ale#lsp#message#GetNextVersionID(),
    \       'text': join(l:lines, "\n") . "\n",
    \   },
    \}]
endfunction

function! ale#lsp#message#DidChange(buffer) abort
    let l:lines = getbufline(a:buffer, 1, '$')

    " For changes, we simply send the full text of the document to the server.
    return [1, 'textDocument/didChange', {
    \   'textDocument': {
    \       'uri': ale#path#ToURI(expand('#' . a:buffer . ':p')),
    \       'version': ale#lsp#message#GetNextVersionID(),
    \   },
    \   'contentChanges': [{'text': join(l:lines, "\n") . "\n"}]
    \}]
endfunction

function! ale#lsp#message#DidSave(buffer) abort
    return [1, 'textDocument/didSave', {
    \   'textDocument': {
    \       'uri': ale#path#ToURI(expand('#' . a:buffer . ':p')),
    \   },
    \}]
endfunction

function! ale#lsp#message#DidClose(buffer) abort
    return [1, 'textDocument/didClose', {
    \   'textDocument': {
    \       'uri': ale#path#ToURI(expand('#' . a:buffer . ':p')),
    \   },
    \}]
endfunction

let s:COMPLETION_TRIGGER_INVOKED = 1
let s:COMPLETION_TRIGGER_CHARACTER = 2

function! ale#lsp#message#Completion(buffer, line, column, trigger_character) abort
    let l:message = [0, 'textDocument/completion', {
    \   'textDocument': {
    \       'uri': ale#path#ToURI(expand('#' . a:buffer . ':p')),
    \   },
    \   'position': {'line': a:line - 1, 'character': a:column - 1},
    \}]

    if !empty(a:trigger_character)
        let l:message[2].context = {
        \   'triggerKind': s:COMPLETION_TRIGGER_CHARACTER,
        \   'triggerCharacter': a:trigger_character,
        \}
    endif

    return l:message
endfunction

function! ale#lsp#message#Definition(buffer, line, column) abort
    return [0, 'textDocument/definition', {
    \   'textDocument': {
    \       'uri': ale#path#ToURI(expand('#' . a:buffer . ':p')),
    \   },
    \   'position': {'line': a:line - 1, 'character': a:column - 1},
    \}]
endfunction

function! ale#lsp#message#TypeDefinition(buffer, line, column) abort
    return [0, 'textDocument/typeDefinition', {
    \   'textDocument': {
    \       'uri': ale#path#ToURI(expand('#' . a:buffer . ':p')),
    \   },
    \   'position': {'line': a:line - 1, 'character': a:column - 1},
    \}]
endfunction

function! ale#lsp#message#References(buffer, line, column) abort
    return [0, 'textDocument/references', {
    \   'textDocument': {
    \       'uri': ale#path#ToURI(expand('#' . a:buffer . ':p')),
    \   },
    \   'position': {'line': a:line - 1, 'character': a:column - 1},
    \   'context': {'includeDeclaration': v:false},
    \}]
endfunction

function! ale#lsp#message#Symbol(query) abort
    return [0, 'workspace/symbol', {
    \   'query': a:query,
    \}]
endfunction

function! ale#lsp#message#Hover(buffer, line, column) abort
    return [0, 'textDocument/hover', {
    \   'textDocument': {
    \       'uri': ale#path#ToURI(expand('#' . a:buffer . ':p')),
    \   },
    \   'position': {'line': a:line - 1, 'character': a:column - 1},
    \}]
endfunction

function! ale#lsp#message#DidChangeConfiguration(buffer, config) abort
    return [1, 'workspace/didChangeConfiguration', {
    \   'settings': a:config,
    \}]
endfunction

function! ale#lsp#message#Rename(buffer, line, column, new_name) abort
    return [0, 'textDocument/rename', {
    \   'textDocument': {
    \       'uri': ale#path#ToURI(expand('#' . a:buffer . ':p')),
    \   },
    \   'position': {'line': a:line - 1, 'character': a:column - 1},
    \   'newName': a:new_name,
    \}]
endfunction
