" Author: Jeffrey Lau - https://github.com/zoonfafer
" Description: Vim Language Server integration for ALE

call ale#Set('vim_vimls_executable', 'vim-language-server')
call ale#Set('vim_vimls_use_global', get(g:, 'ale_use_global_executables', 0))
call ale#Set('vim_vimls_config', {})

function! ale_linters#vim#vimls#GetProjectRoot(buffer) abort
    let l:trigger_file_candidates = [
    \   '.vimrc',
    \   'init.vim',
    \]

    for l:candidate in l:trigger_file_candidates
        let l:trigger_file = fnamemodify(bufname(a:buffer), ':t')

        if l:trigger_file is# l:candidate
            return fnamemodify(
            \   bufname(a:buffer),
            \   ':h',
            \)
        endif
    endfor

    let l:trigger_dir_candidates = [
    \   'autoload',
    \   'plugin',
    \   '.git',
    \]

    let l:path_upwards = ale#path#Upwards(fnamemodify(bufname(a:buffer), ':p:h'))

    for l:path in l:path_upwards
        for l:candidate in l:trigger_dir_candidates
            let l:trigger_dir = ale#path#Simplify(
            \   l:path . '/' . l:candidate,
            \)

            if isdirectory(l:trigger_dir)
                return fnamemodify(
                \   l:trigger_dir,
                \   ':p:h:h',
                \)
            endif
        endfor
    endfor

    return ''
endfunction

call ale#linter#Define('vim', {
\   'name': 'vimls',
\   'lsp': 'stdio',
\   'lsp_config': {b -> ale#Var(b, 'vim_vimls_config')},
\   'executable': {b -> ale#path#FindExecutable(b, 'vim_vimls', [
\       'node_modules/.bin/vim-language-server',
\   ])},
\   'command': '%e --stdio',
\   'language': 'vim',
\   'project_root': function('ale_linters#vim#vimls#GetProjectRoot'),
\})
