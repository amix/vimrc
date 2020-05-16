" Vim syntastic plugin
" Language:     Rust
" Maintainer:   Julien Levesy <jlevesy@gmail.com>
"
" See for details on how to add an external Syntastic checker:
" https://github.com/scrooloose/syntastic/wiki/Syntax-Checker-Guide#external

if exists("g:loaded_syntastic_rust_cargo_checker")
    finish
endif

let g:loaded_syntastic_rust_cargo_checker = 1

" Force syntastic to call cargo without a specific file name
let g:syntastic_rust_cargo_fname = ""

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_rust_cargo_IsAvailable() dict
    if exists("*syntastic#util#getVersion")
        echom "rust.vim: version of Syntastic is too old. Needs to be at least 3.7.0."
        return v:false
    endif

    return executable(self.getExec()) &&
                \ syntastic#util#versionIsAtLeast(self.getVersion(), [0, 16, 0])
endfunction

function! SyntaxCheckers_rust_cargo_GetLocList() dict
    let makeprg = self.makeprgBuild({ "args": "check" })
    let l:root_cargo_toml = cargo#nearestRootCargo(0)
    let l:nearest_cargo_toml = cargo#nearestCargo(0)
    let b:rust_recent_root_cargo_toml = l:root_cargo_toml
    let b:rust_recent_nearest_cargo_toml = l:nearest_cargo_toml

    " All pathname prints are relative to the Cargo.toml of the workspace, if
    " there is a workspace, otherwise they are relative to the Cargo.toml of
    " the single crate. Where to actually execute under these varying
    " circumtances 'cargo' is determined here, and controlled by
    " configuration.

    if rust#GetConfigVar('rust_cargo_avoid_whole_workspace', 1)
        if l:root_cargo_toml !=# l:nearest_cargo_toml
            let makeprg = "cd " . fnamemodify(l:nearest_cargo_toml, ":p:h")
                        \ . " && " . makeprg
        endif
    else
        let makeprg = "cd " . fnamemodify(l:root_cargo_toml, ":p:h")
                    \ . " && " . makeprg
    endif

    let l:check_all_targets = rust#GetConfigVar('rust_cargo_check_all_targets', 0)
    let l:check_all_features = rust#GetConfigVar('rust_cargo_check_all_features', 0)
    let l:check_examples = rust#GetConfigVar('rust_cargo_check_examples', 0)
    let l:check_tests = rust#GetConfigVar('rust_cargo_check_tests', 0)
    let l:check_benches = rust#GetConfigVar('rust_cargo_check_benches', 0)

    let makeprg = makeprg. ' '
                \  . (l:check_all_targets ? ' --all-targets' : '')
                \  . (l:check_all_features ? ' --all-features' : '')
                \  . (l:check_benches ? ' --benches' : '')
                \  . (l:check_examples ? ' --examples' : '')
                \  . (l:check_tests ? ' --tests' : '')

    " Ignored patterns, and blank lines
    let errorformat  =
                \ '%-G,' .
                \ '%-Gerror: aborting %.%#,' .
                \ '%-Gerror: Could not compile %.%#,'

    " Meaningful lines (errors, notes, warnings, contextual information)
    let errorformat .=
                \ '%Eerror: %m,' .
                \ '%Eerror[E%n]: %m,' .
                \ '%Wwarning: %m,' .
                \ '%Inote: %m,' .
                \ '%C %#--> %f:%l:%c'

    return SyntasticMake({
                \ 'makeprg': makeprg,
                \ 'cwd': fnamemodify(l:root_cargo_toml, ":p:h:."),
                \ 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
            \ 'filetype': 'rust',
            \ 'name': 'cargo'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sw=4 sts=4 ts=8:
