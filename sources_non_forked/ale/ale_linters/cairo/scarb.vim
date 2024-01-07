" Author: 0xhyoga <0xhyoga@gmx.com>,
" Description: scarb for cairo files

function! ale_linters#cairo#scarb#GetScarbExecutable(bufnr) abort
    if ale#path#FindNearestFile(a:bufnr, 'Scarb.toml') isnot# ''
        return 'scarb'
    else
        " if there is no Scarb.toml file, we don't use scarb even if it exists,
        " so we return '', because executable('') apparently always fails
        return ''
    endif
endfunction

function! ale_linters#cairo#scarb#GetCommand(buffer, version) abort
    return 'scarb build'
endfunction

call ale#linter#Define('cairo', {
\   'name': 'scarb',
\   'executable': function('ale_linters#cairo#scarb#GetScarbExecutable'),
\   'command': {buffer -> ale#semver#RunWithVersionCheck(
\       buffer,
\       ale_linters#cairo#scarb#GetScarbExecutable(buffer),
\       '%e --version',
\       function('ale_linters#cairo#scarb#GetCommand'),
\   )},
\   'callback': 'ale#handlers#cairo#HandleCairoErrors',
\   'output_stream': 'both',
\   'lint_file': 1,
\})

