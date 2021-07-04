" Author: Yorick Peterse <yorick@yorickpeterse.com>
" Description: linting of Inko source code using the Inko compiler

call ale#Set('inko_inko_executable', 'inko')

function! ale_linters#inko#inko#GetCommand(buffer) abort
    let l:include = ''

    " Include the tests source directory, but only for test files.
    if expand('#' . a:buffer . ':p') =~? '\vtests[/\\]test[/\\]'
        let l:test_dir = ale#path#FindNearestDirectory(a:buffer, 'tests')

        if isdirectory(l:test_dir)
            let l:include = '--include ' . ale#Escape(l:test_dir)
        endif
    endif

    " We use %s instead of %t so the compiler determines the correct module
    " names for the file being edited. Not doing so may lead to errors in
    " certain cases.
    return '%e build --check --format=json'
    \ . ale#Pad(l:include)
    \ . ' %s'
endfunction

call ale#linter#Define('inko', {
\   'name': 'inko',
\   'executable': {b -> ale#Var(b, 'inko_inko_executable')},
\   'command': function('ale_linters#inko#inko#GetCommand'),
\   'callback': 'ale#handlers#inko#Handle',
\   'output_stream': 'stderr',
\   'lint_file': 1
\})
