" ============================================================================
" File:        vcs.vim
" Description: NERDTree plugin that provides a command to open on the root of
"              a version control system repository.
" Maintainer:  Phil Runninger
" License:     This program is free software. It comes without any warranty,
"              to the extent permitted by applicable law. You can redistribute
"              it and/or modify it under the terms of the Do What The Fuck You
"              Want To Public License, Version 2, as published by Sam Hocevar.
"              See http://sam.zoy.org/wtfpl/COPYING for more details.
"
" ============================================================================
command! -n=? -complete=dir -bar NERDTreeVCS :call <SID>CreateTabTreeVCS('<args>')
command! -n=? -complete=dir -bar NERDTreeToggleVCS :call <SID>ToggleTabTreeVCS('<args>')

" FUNCTION: s:CreateTabTreeVCS(a:name) {{{1
function! s:CreateTabTreeVCS(name)
    let l:path = g:NERDTreeCreator._pathForString(a:name)
    let l:path = s:FindParentVCSRoot(l:path)
    call g:NERDTreeCreator.createTabTree(empty(l:path) ? "" : l:path._str())
endfunction

" FUNCTION: s:ToggleTabTreeVCS(a:name) {{{1
" Behaves the same as ToggleTabTree except roots directory at VCS root
function! s:ToggleTabTreeVCS(name)
    let l:path = g:NERDTreeCreator._pathForString(a:name)
    let l:path = s:FindParentVCSRoot(l:path)
    call g:NERDTreeCreator.toggleTabTree(empty(l:path) ? "" : l:path._str())
endfunction

" FUNCTION: s:FindParentVCSRoot(a:path) {{{1
" Finds the root version control system folder of the given path. If a:path is
" not part of a repository, return the original path.
function! s:FindParentVCSRoot(path)
    let l:path = a:path
    while !empty(l:path) &&
        \ l:path._str() !~ '^\(\a:\\\|\/\)$' &&
        \ !isdirectory(l:path._str() . '/.git') &&
        \ !isdirectory(l:path._str() . '/.svn') &&
        \ !isdirectory(l:path._str() . '/.hg') &&
        \ !isdirectory(l:path._str() . '/.bzr') &&
        \ !isdirectory(l:path._str() . '/_darcs')
        let l:path = l:path.getParent()
    endwhile
    return (empty(l:path) || l:path._str() =~ '^\(\a:\\\|\/\)$') ? a:path : l:path
endfunction

