" Author: w0rp <devw0rp@gmail.com>, KabbAmine <amine.kabb@gmail.com>
" Description: This file adds support for checking Vim code with Vint.

" This flag can be used to change enable/disable style issues.
call ale#Set('vim_vint_show_style_issues', 1)
call ale#Set('vim_vint_executable', 'vint')
let s:enable_neovim = has('nvim') ? ' --enable-neovim' : ''
let s:format = '-f "{file_path}:{line_number}:{column_number}: {severity}: {policy_name} - {description} (see {reference})"'

function! ale_linters#vim#vint#GetCommand(buffer, version) abort
    let l:can_use_no_color_flag = empty(a:version)
    \   || ale#semver#GTE(a:version, [0, 3, 7])

    let l:warning_flag = ale#Var(a:buffer, 'vim_vint_show_style_issues') ? '-s' : '-w'

    " Use the --stdin-display-name argument if supported, temp file otherwise.
    let l:stdin_or_temp = ale#semver#GTE(a:version, [0, 4, 0])
    \   ? ' --stdin-display-name %s -'
    \   : ' %t'

    return '%e'
    \   . ' ' . l:warning_flag
    \   . (l:can_use_no_color_flag ? ' --no-color' : '')
    \   . s:enable_neovim
    \   . ' ' . s:format
    \   . l:stdin_or_temp
endfunction

let s:word_regex_list = [
\   '\v^Undefined variable: ([^ ]+)',
\   '\v^Make the scope explicit like ...([^ ]+). ',
\   '\v^.*start with a capital or contain a colon: ([^ ]+)',
\   '\v.*instead of .(\=[=~]).',
\]

function! ale_linters#vim#vint#Handle(buffer, lines) abort
    let l:loclist = ale#handlers#gcc#HandleGCCFormat(a:buffer, a:lines)

    for l:item in l:loclist
        let l:match = []

        for l:regex in s:word_regex_list
            let l:match = matchlist(l:item.text, l:regex)

            if !empty(l:match)
                let l:item.end_col = l:item.col + len(l:match[1]) - 1
                break
            endif
        endfor
    endfor

    return l:loclist
endfunction

call ale#linter#Define('vim', {
\   'name': 'vint',
\   'executable': {buffer -> ale#Var(buffer, 'vim_vint_executable')},
\   'command': {buffer -> ale#semver#RunWithVersionCheck(
\       buffer,
\       ale#Var(buffer, 'vim_vint_executable'),
\       '%e --version',
\       function('ale_linters#vim#vint#GetCommand'),
\   )},
\   'callback': 'ale_linters#vim#vint#Handle',
\})
