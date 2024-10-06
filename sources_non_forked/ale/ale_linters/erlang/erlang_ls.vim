" Author: Dmitri Vereshchagin <dmitri.vereshchagin@gmail.com>
" Description: LSP linter for Erlang files

call ale#Set('erlang_erlang_ls_executable', 'erlang_ls')
call ale#Set('erlang_erlang_ls_log_dir', '')
call ale#Set('erlang_erlang_ls_log_level', 'info')

function! s:GetCommand(buffer) abort
    let l:log_dir = ale#Var(a:buffer, 'erlang_erlang_ls_log_dir')
    let l:log_level = ale#Var(a:buffer, 'erlang_erlang_ls_log_level')

    let l:command = '%e'

    if !empty(l:log_dir)
        let l:command .= ' --log-dir=' . ale#Escape(l:log_dir)
    endif

    let l:command .= ' --log-level=' . ale#Escape(l:log_level)

    return l:command
endfunction

function! s:FindProjectRoot(buffer) abort
    let l:markers = [
    \   '_checkouts/',
    \   '_build/',
    \   'deps/',
    \   'erlang_ls.config',
    \   'rebar.lock',
    \   'erlang.mk',
    \]

    " This is a way to find Erlang/OTP root (the one that is managed
    " by kerl or asdf).  Useful if :ALEGoToDefinition takes us there.
    let l:markers += ['.kerl_config']

    for l:marker in l:markers
        let l:path = l:marker[-1:] is# '/'
        \   ? ale#path#FindNearestDirectory(a:buffer, l:marker)
        \   : ale#path#FindNearestFile(a:buffer, l:marker)

        if !empty(l:path)
            return ale#path#Dirname(l:path)
        endif
    endfor

    return ''
endfunction

call ale#linter#Define('erlang', {
\   'name': 'erlang_ls',
\   'executable': {b -> ale#Var(b, 'erlang_erlang_ls_executable')},
\   'command': function('s:GetCommand'),
\   'lsp': 'stdio',
\   'project_root': function('s:FindProjectRoot'),
\})
