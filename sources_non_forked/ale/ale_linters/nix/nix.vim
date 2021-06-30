" Author: Alistair Bill <@alibabzo>
" Author: Maximilian Bosch <maximilian@mbosch.me>
" Description: nix-instantiate linter for nix files

function! ale_linters#nix#nix#Command(buffer, output, meta) abort
    let l:version = a:output[0][22:]

    if l:version =~# '^\(2.4\|3\).*'
        return 'nix-instantiate --log-format internal-json --parse -'
    else
        return 'nix-instantiate --parse -'
    endif
endfunction

function! ale_linters#nix#nix#Handle(buffer, lines) abort
    let l:output = []

    if empty(a:lines)
        return l:output
    endif

    if a:lines[0] =~# '^@nix .*'
        for l:line in a:lines
            if l:line =~# '^@nix .*'
                let l:result = json_decode(strpart(l:line, 4))

                if has_key(l:result, 'column')
                    call add(l:output, {
                    \     'type': 'E',
                    \     'lnum': l:result.line,
                    \     'col': l:result.column,
                    \     'text': l:result.raw_msg
                    \})
                endif
            endif
        endfor
    else
        let l:pattern = '^\(.\+\): \(.\+\) at .*:\(\d\+\):\(\d\+\)$'

        for l:match in ale#util#GetMatches(a:lines, l:pattern)
            call add(l:output, {
            \   'lnum': l:match[3] + 0,
            \   'col': l:match[4] + 0,
            \   'text': l:match[1] . ': ' . substitute(l:match[2], ',$', '', ''),
            \   'type': l:match[1] =~# '^error' ? 'E' : 'W',
            \})
        endfor
    endif

    return l:output
endfunction

call ale#linter#Define('nix', {
\   'name': 'nix',
\   'output_stream': 'stderr',
\   'executable': 'nix-instantiate',
\   'command': {buffer -> ale#command#Run(
\       buffer,
\       'nix-instantiate --version',
\       function('ale_linters#nix#nix#Command')
\   )},
\   'callback': 'ale_linters#nix#nix#Handle',
\})
