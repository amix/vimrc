" Author: Bang Lee <https://github.com/Qusic>
" Description: clang linter for objc files

" Set this option to change the Clang options for warnings for ObjC.
if !exists('g:ale_objc_clang_options')
    let g:ale_objc_clang_options = '-std=c11 -Wall'
endif

function! ale_linters#objc#clang#GetCommand(buffer) abort
    " -iquote with the directory the file is in makes #include work for
    "  headers in the same directory.
    return 'clang -S -x objective-c -fsyntax-only '
    \   . '-iquote ' . ale#Escape(fnamemodify(bufname(a:buffer), ':p:h'))
    \   . ' ' . ale#Var(a:buffer, 'objc_clang_options') . ' -'
endfunction

call ale#linter#Define('objc', {
\   'name': 'clang',
\   'output_stream': 'stderr',
\   'executable': 'clang',
\   'command': function('ale_linters#objc#clang#GetCommand'),
\   'callback': 'ale#handlers#gcc#HandleGCCFormatWithIncludes',
\})
