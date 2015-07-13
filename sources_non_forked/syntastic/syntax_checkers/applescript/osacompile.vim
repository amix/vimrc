"==============================================================================
"       FileName: applescript.vim
"           Desc: Syntax checking plugin for syntastic.vim
"         Author: Zhao Cai
"          Email: caizhaoff@gmail.com
"        Version: 0.2.1
"   Date Created: Thu 09 Sep 2011 10:30:09 AM EST
"  Last Modified: Fri 09 Dec 2011 01:10:24 PM EST
"
"        History: 0.1.0 - working, but it will run the script everytime to check
"                 syntax. Should use osacompile but strangely it does not give
"                 errors.
"
"                 0.2.0 - switch to osacompile, it gives less errors compared
"                 with osascript.
"
"                 0.2.1 - remove g:syntastic_applescript_tempfile. use
"                 tempname() instead.
"
"        License: This program is free software. It comes without any
"        warranty, to the extent permitted by applicable law. You can
"        redistribute it and/or modify it under the terms of the Do What The
"        Fuck You Want To Public License, Version 2, as published by Sam
"        Hocevar.  See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_applescript_osacompile_checker')
    finish
endif
let g:loaded_syntastic_applescript_osacompile_checker = 1

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_applescript_osacompile_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args_after': '-o ' . tempname() . '.scpt' })
    let errorformat = '%f:%l:%m'
    return SyntasticMake({ 'makeprg': makeprg, 'errorformat': errorformat })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'applescript',
    \ 'name': 'osacompile' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
