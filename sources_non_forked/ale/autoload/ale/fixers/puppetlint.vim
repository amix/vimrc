" Author: Alexander Olofsson <alexander.olofsson@liu.se>
" Description: puppet-lint fixer

if !exists('g:ale_puppet_puppetlint_executable')
    let g:ale_puppet_puppetlint_executable = 'puppet-lint'
endif
if !exists('g:ale_puppet_puppetlint_options')
    let g:ale_puppet_puppetlint_options = ''
endif

function! ale#fixers#puppetlint#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'puppet_puppetlint_executable')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' ' . ale#Var(a:buffer, 'puppet_puppetlint_options')
    \       . ' --fix'
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
