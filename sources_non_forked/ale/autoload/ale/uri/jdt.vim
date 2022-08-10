" Author: yoshi1123 <yoshi1@tutanota.com>
" Description: Functions for working with jdt:// URIs.

function! s:OpenJDTLink(root, uri, line, column, options, result) abort
    if has_key(a:result, 'error')
        " no-custom-checks
        echoerr a:result.error.message

        return
    endif

    let l:contents = a:result['result']

    if type(l:contents) is# type(v:null)
        " no-custom-checks
        echoerr 'File content not found'
    endif

    " disable autocmd when opening buffer
    autocmd! AleURISchemes
    call ale#util#Open(a:uri, a:line, a:column, a:options)
    autocmd AleURISchemes BufNewFile,BufReadPre jdt://** call ale#uri#jdt#ReadJDTLink(expand('<amatch>'))

    if !empty(getbufvar(bufnr(''), 'ale_lsp_root', ''))
        return
    endif

    let b:ale_lsp_root = a:root
    set filetype=java

    call setline(1, split(l:contents, '\n'))
    call cursor(a:line, a:column)
    normal! zz

    setlocal buftype=nofile nomodified nomodifiable readonly
endfunction

" Load new buffer with jdt:// contents and jump to line and column.
function! ale#uri#jdt#OpenJDTLink(encoded_uri, line, column, options, conn_id) abort
    let l:found_eclipselsp = v:false

    for l:linter in ale#linter#Get('java')
        if l:linter.name is# 'eclipselsp'
            let l:found_eclipselsp = v:true
        endif
    endfor

    if !l:found_eclipselsp
        throw 'eclipselsp not running'
    endif

    let l:root = a:conn_id[stridx(a:conn_id, ':')+1:]
    let l:uri = a:encoded_uri
    call ale#lsp_linter#SendRequest(
    \   bufnr(''),
    \   'eclipselsp',
    \   [0, 'java/classFileContents', {'uri': ale#util#ToURI(l:uri)}],
    \   function('s:OpenJDTLink', [l:root, l:uri, a:line, a:column, a:options])
    \)
endfunction

function! s:ReadClassFileContents(uri, result) abort
    if has_key(a:result, 'error')
        " no-custom-checks
        echoerr a:result.error.message

        return
    endif

    let l:contents = a:result['result']

    if type(l:contents) is# type(v:null)
        " no-custom-checks
        echoerr 'File content not found'
    endif

    call setline(1, split(l:contents, '\n'))

    setlocal buftype=nofile nomodified nomodifiable readonly
endfunction

" Read jdt:// contents, as part of current project, into current buffer.
function! ale#uri#jdt#ReadJDTLink(encoded_uri) abort
    if !empty(getbufvar(bufnr(''), 'ale_lsp_root', ''))
        return
    endif

    let l:linter_map = ale#lsp_linter#GetLSPLinterMap()

    for l:conn_id in keys(l:linter_map)
        if l:linter_map[l:conn_id] is# 'eclipselsp'
            let l:root = l:conn_id[stridx(l:conn_id, ':')+1:]
        endif
    endfor

    if l:root is# v:null
        throw 'eclipselsp not running'
    endif

    let l:uri = a:encoded_uri
    let b:ale_lsp_root = l:root
    set filetype=java

    call ale#lsp_linter#SendRequest(
    \   bufnr(''),
    \   'eclipselsp',
    \   [0, 'java/classFileContents', {'uri': ale#util#ToURI(l:uri)}],
    \   function('s:ReadClassFileContents', [l:uri])
    \)
endfunction
