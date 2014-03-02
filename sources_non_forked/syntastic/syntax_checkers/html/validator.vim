"============================================================================
"File:        validator.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  LCD 47 <lcd047 at gmail dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================
"
" For detail;s about validator see: http://about.validator.nu/
"
" Checker options:
"
" - g:syntastic_html_validator_api (string; default: 'http://validator.nu/')
"   URL of the service to use for checking; leave it to the default to run the
"   checks against http://validator.nu/, or set it to 'http://localhost:8888/'
"   if you're running a local service as per http://about.validator.nu/#src
"
" - g:syntastic_html_validator_parser (string; default: empty)
"   parser to use; legal values are: xml, xmldtd, html, html5, html4, html4tr;
"   set it to 'html5' to check HTML5 files;  see the wiki for reference:
"   http://wiki.whatwg.org/wiki/Validator.nu_Common_Input_Parameters#parser
"
" - g:syntastic_html_validator_nsfilter (string; default: empty)
"   sets the nsfilter for the parser; see the wiki for details:
"   http://wiki.whatwg.org/wiki/Validator.nu_Common_Input_Parameters#nsfilter

if exists("g:loaded_syntastic_html_validator_checker")
    finish
endif
let g:loaded_syntastic_html_validator_checker=1

if !exists('g:syntastic_html_validator_api')
    let g:syntastic_html_validator_api = 'http://validator.nu/'
endif

if !exists('g:syntastic_html_validator_parser')
    let g:syntastic_html_validator_parser = ''
endif

if !exists('g:syntastic_html_validator_nsfilter')
    let g:syntastic_html_validator_nsfilter = ''
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_html_validator_GetLocList() dict
    let fname = syntastic#util#shexpand('%')
    let makeprg = self.getExecEscaped() . ' -s --compressed -F out=gnu -F asciiquotes=yes' .
        \ (g:syntastic_html_validator_parser != '' ? ' -F parser=' . g:syntastic_html_validator_parser : '') .
        \ (g:syntastic_html_validator_nsfilter != '' ? ' -F nsfilter=' . g:syntastic_html_validator_nsfilter : '') .
        \ ' -F doc=@' . fname . '\;type=text/html\;filename=' . fname . ' ' . g:syntastic_html_validator_api

    let errorformat =
        \ '%E"%f":%l: %trror: %m,' .
        \ '%E"%f":%l-%\d%\+: %trror: %m,' .
        \ '%E"%f":%l%\%.%c: %trror: %m,' .
        \ '%E"%f":%l%\%.%c-%\d%\+%\%.%\d%\+: %trror: %m,' .
        \ '%E"%f":%l: %trror fatal: %m,' .
        \ '%E"%f":%l-%\d%\+: %trror fatal: %m,' .
        \ '%E"%f":%l%\%.%c: %trror fatal: %m,' .
        \ '%E"%f":%l%\%.%c-%\d%\+%\%.%\d%\+: %trror fatal: %m,' .
        \ '%W"%f":%l: info %tarning: %m,' .
        \ '%W"%f":%l-%\d%\+: info %tarning: %m,' .
        \ '%W"%f":%l%\%.%c: info %tarning: %m,' .
        \ '%W"%f":%l%\%.%c-%\d%\+%\%.%\d%\+: info %tarning: %m'

    return SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'preprocess': 'validator',
        \ 'returns': [0] })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'html',
    \ 'name': 'validator',
    \ 'exec': 'curl' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
