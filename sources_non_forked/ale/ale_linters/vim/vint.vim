" Author: w0rp <devw0rp@gmail.com>, KabbAmine <amine.kabb@gmail.com>
" Description: This file adds support for checking Vim code with Vint.

" This flag can be used to change enable/disable style issues.
call ale#Set('vim_vint_show_style_issues', 1)
call ale#Set('vim_vint_executable', 'vint')
let s:enable_neovim = has('nvim') ? ' --enable-neovim' : ''
let s:format = '-f "{file_path}:{line_number}:{column_number}: {severity}: {description} (see {reference})"'

function! ale_linters#vim#vint#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'vim_vint_executable')
endfunction

function! ale_linters#vim#vint#VersionCommand(buffer) abort
    let l:executable = ale_linters#vim#vint#GetExecutable(a:buffer)

    " Check the Vint version if we haven't checked it already.
    return !ale#semver#HasVersion(l:executable)
    \   ? ale#Escape(l:executable) . ' --version'
    \   : ''
endfunction

function! ale_linters#vim#vint#GetCommand(buffer, version_output) abort
    let l:executable = ale_linters#vim#vint#GetExecutable(a:buffer)
    let l:version = ale#semver#GetVersion(l:executable, a:version_output)

    let l:can_use_no_color_flag = empty(l:version)
    \   || ale#semver#GTE(l:version, [0, 3, 7])

    let l:warning_flag = ale#Var(a:buffer, 'vim_vint_show_style_issues') ? '-s' : '-w'

    return ale#Escape(l:executable)
    \   . ' ' . l:warning_flag
    \   . (l:can_use_no_color_flag ? ' --no-color' : '')
    \   . s:enable_neovim
    \   . ' ' . s:format
    \   . ' %t'
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
\   'executable_callback': 'ale_linters#vim#vint#GetExecutable',
\   'command_chain': [
\       {'callback': 'ale_linters#vim#vint#VersionCommand', 'output_stream': 'stderr'},
\       {'callback': 'ale_linters#vim#vint#GetCommand', 'output_stream': 'stdout'},
\   ],
\   'callback': 'ale_linters#vim#vint#Handle',
\})
