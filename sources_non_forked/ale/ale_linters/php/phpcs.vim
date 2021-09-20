" Author: jwilliams108 <https://github.com/jwilliams108>, Eric Stern <https://github.com/firehed>
" Description: phpcs for PHP files

let g:ale_php_phpcs_standard = get(g:, 'ale_php_phpcs_standard', '')

call ale#Set('php_phpcs_options', '')
call ale#Set('php_phpcs_executable', 'phpcs')
call ale#Set('php_phpcs_use_global', get(g:, 'ale_use_global_executables', 0))

function! ale_linters#php#phpcs#GetCommand(buffer) abort
    let l:standard = ale#Var(a:buffer, 'php_phpcs_standard')
    let l:standard_option = !empty(l:standard)
    \   ? '--standard=' . ale#Escape(l:standard)
    \   : ''

    return '%e -s --report=emacs --stdin-path=%s'
    \   . ale#Pad(l:standard_option)
    \   . ale#Pad(ale#Var(a:buffer, 'php_phpcs_options'))
endfunction

function! ale_linters#php#phpcs#Handle(buffer, lines) abort
    " Matches against lines like the following:
    "
    " /path/to/some-filename.php:18:3: error - Line indented incorrectly; expected 4 spaces, found 2 (Generic.WhiteSpace.ScopeIndent.IncorrectExact)
    let l:pattern = '^.*:\(\d\+\):\(\d\+\): \(.\+\) - \(.\+\) (\(.\+\)).*$'
    let l:output = []

    for l:match in ale#util#GetMatches(a:lines, l:pattern)
        let l:code = l:match[5]
        let l:text = l:match[4] . ' (' . l:code . ')'
        let l:type = l:match[3]

        call add(l:output, {
        \   'lnum': l:match[1] + 0,
        \   'col': l:match[2] + 0,
        \   'text': l:text,
        \   'type': l:type is# 'error' ? 'E' : 'W',
        \   'sub_type': 'style',
        \})
    endfor

    return l:output
endfunction

call ale#linter#Define('php', {
\   'name': 'phpcs',
\   'executable': {b -> ale#path#FindExecutable(b, 'php_phpcs', [
\       'vendor/bin/phpcs',
\       'phpcs'
\   ])},
\   'cwd': '%s:h',
\   'command': function('ale_linters#php#phpcs#GetCommand'),
\   'callback': 'ale_linters#php#phpcs#Handle',
\})
