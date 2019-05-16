" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! go#lsp#message#Initialize(wd) abort
  return {
          \ 'notification': 0,
          \ 'method': 'initialize',
          \ 'params': {
            \ 'processId': getpid(),
            \ 'rootUri': go#path#ToURI(a:wd),
            \ 'capabilities': {
              \ 'workspace': {},
              \ 'textDocument': {
                \ 'hover': {
                  \ 'contentFormat': ['plaintext'],
                \ },
              \ }
            \ }
          \ }
       \ }
endfunction

function! go#lsp#message#Definition(file, line, col) abort
  return {
          \ 'notification': 0,
          \ 'method': 'textDocument/definition',
          \ 'params': {
          \   'textDocument': {
          \       'uri': go#path#ToURI(a:file)
          \   },
          \   'position': s:position(a:line, a:col)
          \ }
       \ }
endfunction

function! go#lsp#message#TypeDefinition(file, line, col) abort
  return {
          \ 'notification': 0,
          \ 'method': 'textDocument/typeDefinition',
          \ 'params': {
          \   'textDocument': {
          \       'uri': go#path#ToURI(a:file)
          \   },
          \   'position': s:position(a:line, a:col)
          \ }
       \ }
endfunction

function! go#lsp#message#DidOpen(file, content) abort
  return {
          \ 'notification': 1,
          \ 'method': 'textDocument/didOpen',
          \ 'params': {
          \     'textDocument': {
          \         'uri': go#path#ToURI(a:file),
          \         'languageId': 'go',
          \         'text': a:content,
          \     }
          \ }
       \ }
endfunction

function! go#lsp#message#DidChange(file, content) abort
  return {
          \ 'notification': 1,
          \ 'method': 'textDocument/didChange',
          \ 'params': {
          \     'textDocument': {
          \         'uri': go#path#ToURI(a:file),
          \     },
          \     'contentChanges': [
          \       {
          \         'text': a:content,
          \       }
          \     ]
          \ }
       \ }
endfunction

function! go#lsp#message#DidClose(file) abort
  return {
          \ 'notification': 1,
          \ 'method': 'textDocument/didClose',
          \ 'params': {
          \     'textDocument': {
          \         'uri': go#path#ToURI(a:file),
          \     }
          \ }
       \ }
endfunction

function! go#lsp#message#Completion(file, line, col) abort
  return {
          \ 'notification': 0,
          \ 'method': 'textDocument/completion',
          \ 'params': {
          \   'textDocument': {
          \       'uri': go#path#ToURI(a:file)
          \   },
          \   'position': s:position(a:line, a:col),
          \ }
       \ }
endfunction

function! go#lsp#message#Hover(file, line, col) abort
  return {
          \ 'notification': 0,
          \ 'method': 'textDocument/hover',
          \ 'params': {
          \   'textDocument': {
          \       'uri': go#path#ToURI(a:file)
          \   },
          \   'position': s:position(a:line, a:col),
          \ }
       \ }
endfunction

function! s:position(line, col) abort
  return {'line': a:line - 1, 'character': a:col-1}
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
