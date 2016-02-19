"============================================================================
"File:        tsc.vim
"Description: TypeScript syntax checker
"Maintainer:  Bill Casarin <bill@casarin.ca>
"
"============================================================================

if exists('g:loaded_syntastic_typescript_tsc_checker')
    finish
endif
let g:loaded_syntastic_typescript_tsc_checker = 1

if !exists('g:syntastic_typescript_tsc_sort')
    let g:syntastic_typescript_tsc_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_typescript_tsc_IsAvailable() dict
    if !executable(self.getExec())
        return 0
    endif

    let version_output = split(syntastic#util#system(self.getExecEscaped() . ' --version'), '\n', 1)
    let ver = filter(copy(version_output), 'v:val =~# ''\m\<Version ''')
    let parsed_ver = len(ver) ? syntastic#util#parseVersion(ver[0], '\v<Version \zs\d+(\.\d+)\ze') : []

    if len(parsed_ver)
        call self.setVersion(parsed_ver)
        let s:tsc_new = syntastic#util#versionIsAtLeast(parsed_ver, [1, 5])
    else
        call syntastic#log#ndebug(g:_SYNTASTIC_DEBUG_LOCLIST, 'checker output:', version_output)
        call syntastic#log#error("checker typescript/tsc: can't parse version string (abnormal termination?)")
        let s:tsc_new = -1
    endif

    return s:tsc_new >= 0
endfunction

function! SyntaxCheckers_typescript_tsc_GetLocList() dict
    let makeprg = self.makeprgBuild({
        \ 'args': '--module commonjs',
        \ 'args_after': (s:tsc_new ? '--noEmit' : '--out ' . syntastic#util#DevNull()) })

    let errorformat =
        \ '%E%f %#(%l\,%c): error %m,' .
        \ '%E%f %#(%l\,%c): %m,' .
        \ '%Eerror %m,' .
        \ '%C%\s%\+%m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'postprocess': ['guards'],
        \ 'defaults': {'bufnr': bufnr('')} })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'typescript',
    \ 'name': 'tsc'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
