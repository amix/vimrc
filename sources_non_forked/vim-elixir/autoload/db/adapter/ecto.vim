let s:path = expand('<sfile>:h')
let s:cmd = join(['mix', 'run', '--no-start', '--no-compile', shellescape(s:path.'/get_repos.exs')])

function! s:repo_list() abort
    return map(systemlist(s:cmd), 'split(v:val)')
endfunction

function! db#adapter#ecto#canonicalize(url) abort
    for l:item in s:repo_list()
        let l:name = get(l:item, 0)
        let l:url = get(l:item, 1)
        if !empty(l:name) && 'ecto:'.l:name ==# a:url
            return l:url
        endif
    endfor
endfunction

function! db#adapter#ecto#complete_opaque(url) abort
    return map(s:repo_list(), 'v:val[0]')
endfunction
