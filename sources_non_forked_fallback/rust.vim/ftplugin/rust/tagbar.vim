"
" Support for Tagbar -- https://github.com/majutsushi/tagbar
"
if !exists(':Tagbar') || rust#tags#IsUCtags()
    finish
endif

" vint: -ProhibitAbbreviationOption
let s:save_cpo = &cpo
set cpo&vim
" vint: +ProhibitAbbreviationOption

if !exists('g:tagbar_type_rust')
    let g:tagbar_type_rust = {
                \ 'ctagstype' : 'rust',
                \ 'kinds' : [
                \'T:types',
                \'f:functions',
                \'g:enumerations',
                \'s:structures',
                \'m:modules',
                \'c:constants',
                \'t:traits',
                \'i:trait implementations',
                \ ]
                \ }
endif

" In case you've updated/customized your ~/.ctags and prefer to use it.
if !get(g:, 'rust_use_custom_ctags_defs', 0)
    let g:tagbar_type_rust.deffile = expand('<sfile>:p:h:h:h') . '/ctags/rust.ctags'
endif

" vint: -ProhibitAbbreviationOption
let &cpo = s:save_cpo
unlet s:save_cpo
" vint: +ProhibitAbbreviationOption


" vim: set et sw=4 sts=4 ts=8:
