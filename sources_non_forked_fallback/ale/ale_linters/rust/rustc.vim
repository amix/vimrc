" Author: Daniel Schemala <istjanichtzufassen@gmail.com>
" Description: rustc for rust files

call ale#Set('rust_rustc_options', '--emit=mir -o /dev/null')

function! ale_linters#rust#rustc#RustcCommand(buffer) abort
    " Try to guess the library search path. If the project is managed by cargo,
    " it's usually <project root>/target/debug/deps/ or
    " <project root>/target/release/deps/
    let l:cargo_file = ale#path#FindNearestFile(a:buffer, 'Cargo.toml')

    if l:cargo_file isnot# ''
        let l:root = fnamemodify(l:cargo_file, ':h')
        let l:dependencies = ' -L ' . ale#Escape(ale#path#GetAbsPath(l:root, 'target/debug/deps'))
        \   . ' -L ' . ale#Escape(ale#path#GetAbsPath(l:root, 'target/release/deps'))
    else
        let l:dependencies = ''
    endif

    let l:options = ale#Var(a:buffer, 'rust_rustc_options')

    return 'rustc --error-format=json'
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \   . l:dependencies . ' -'
endfunction

call ale#linter#Define('rust', {
\   'name': 'rustc',
\   'executable': 'rustc',
\   'command': function('ale_linters#rust#rustc#RustcCommand'),
\   'callback': 'ale#handlers#rust#HandleRustErrors',
\   'output_stream': 'stderr',
\})
