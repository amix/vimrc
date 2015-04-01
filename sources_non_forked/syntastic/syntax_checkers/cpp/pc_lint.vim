"============================================================================
"File:        pc_lint.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Steve Bragg <steve at empresseffects dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists("g:loaded_syntastic_cpp_pc_lint_checker")
    finish
endif
let g:loaded_syntastic_cpp_pc_lint_checker = 1

runtime! syntax_checkers/c/*.vim

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'cpp',
    \ 'name': 'pc_lint',
    \ 'exec': 'lint-nt',
    \ 'redirect': 'c/pc_lint'})

" vim: set sw=4 sts=4 et fdm=marker:
