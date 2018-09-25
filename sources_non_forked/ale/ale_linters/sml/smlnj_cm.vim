" Author: Jake Zimmerman <jake@zimmerman.io>
" Description: SML checking with SML/NJ Compilation Manager

function! ale_linters#sml#smlnj_cm#GetCommand(buffer) abort
    let l:cmfile = ale#handlers#sml#GetCmFile(a:buffer)

    return 'sml -m ' . l:cmfile . ' < /dev/null'
endfunction

" Using CM requires that we set "lint_file: 1", since it reads the files
" from the disk itself.
call ale#linter#Define('sml', {
\   'name': 'smlnj_cm',
\   'aliases': ['smlnj-cm'],
\   'executable_callback': 'ale#handlers#sml#GetExecutableSmlnjCm',
\   'lint_file': 1,
\   'command_callback': 'ale_linters#sml#smlnj_cm#GetCommand',
\   'callback': 'ale#handlers#sml#Handle',
\})

" vim:ts=4:sts=4:sw=4
