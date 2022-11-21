" Author: blahgeek <i@blahgeek.com>
" Description: NVCC linter for cuda files

call ale#Set('cuda_nvcc_executable', 'nvcc')
call ale#Set('cuda_nvcc_options', '-std=c++11')

function! ale_linters#cuda#nvcc#GetCommand(buffer) abort
    return '%e -cuda'
    \   . ale#Pad(ale#c#IncludeOptions(ale#c#FindLocalHeaderPaths(a:buffer)))
    \   . ale#Pad(ale#Var(a:buffer, 'cuda_nvcc_options'))
    \   . ' %s -o ' . g:ale#util#nul_file
endfunction

function! ale_linters#cuda#nvcc#HandleNVCCFormat(buffer, lines) abort
    " Look for lines like the following.
    "
    " test.cu(8): error: argument of type "void *" is incompatible with parameter of type "int *"
    let l:pattern = '\v^([^:\(\)]+):?\(?(\d+)\)?:(\d+)?:?\s*\w*\s*(error|warning): (.+)$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:item = {
        \   'lnum': str2nr(l:match[2]),
        \   'type': l:match[4] =~# 'error' ? 'E' : 'W',
        \   'text': l:match[5],
        \   'filename': fnamemodify(l:match[1], ':p'),
        \}

        if !empty(l:match[3])
            let l:item.col = str2nr(l:match[3])
        endif

        call add(l:output, l:item)
    endfor

    return l:output
endfunction

call ale#linter#Define('cuda', {
\   'name': 'nvcc',
\   'output_stream': 'stderr',
\   'executable': {b -> ale#Var(b, 'cuda_nvcc_executable')},
\   'command': function('ale_linters#cuda#nvcc#GetCommand'),
\   'callback': 'ale_linters#cuda#nvcc#HandleNVCCFormat',
\   'lint_file': 1,
\})
