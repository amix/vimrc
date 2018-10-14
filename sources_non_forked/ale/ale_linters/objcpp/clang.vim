" Author: Bang Lee <https://github.com/Qusic>
" Description: clang linter for objcpp files

" Set this option to change the Clang options for warnings for ObjCPP.
if !exists('g:ale_objcpp_clang_options')
    let g:ale_objcpp_clang_options = '-std=c++14 -Wall'
endif

function! ale_linters#objcpp#clang#GetCommand(buffer) abort
    " -iquote with the directory the file is in makes #include work for
    "  headers in the same directory.
    return 'clang++ -S -x objective-c++ -fsyntax-only '
    \   . '-iquote ' . ale#Escape(fnamemodify(bufname(a:buffer), ':p:h'))
    \   . ' ' . ale#Var(a:buffer, 'objcpp_clang_options') . ' -'
endfunction

call ale#linter#Define('objcpp', {
\   'name': 'clang',
\   'output_stream': 'stderr',
\   'executable': 'clang++',
\   'command_callback': 'ale_linters#objcpp#clang#GetCommand',
\   'callback': 'ale#handlers#gcc#HandleGCCFormatWithIncludes',
\})
