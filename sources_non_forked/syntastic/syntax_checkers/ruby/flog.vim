"============================================================================
"File:        flog.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Tim Carry <tim at pixelastic dot com>
"License:     This program is free software. It comes without any warranty,
"             to the extent permitted by applicable law. You can redistribute
"             it and/or modify it under the terms of the Do What The Fuck You
"             Want To Public License, Version 2, as published by Sam Hocevar.
"             See http://sam.zoy.org/wtfpl/COPYING for more details.
"
"============================================================================

if exists('g:loaded_syntastic_ruby_flog_checker')
    finish
endif
let g:loaded_syntastic_ruby_flog_checker = 1

if !exists('g:syntastic_ruby_flog_threshold_warning')
    let g:syntastic_ruby_flog_threshold_warning = 45
endif

if !exists('g:syntastic_ruby_flog_threshold_error')
    let g:syntastic_ruby_flog_threshold_error = 90
endif

if !exists('g:syntastic_ruby_flog_sort')
    let g:syntastic_ruby_flog_sort = 1
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_ruby_flog_GetLocList() dict
    let makeprg = self.makeprgBuild({ 'args': '-a' })

    " Example output:
    "   93.25: MyClass::my_method my_file:42
    let errorformat = '%\m%\s%#%m: %.%# %f:%l'

    let loclist = SyntasticMake({
        \ 'makeprg': makeprg,
        \ 'errorformat': errorformat,
        \ 'subtype': 'Style' })

    let trail_w = syntastic#util#float2str(g:syntastic_ruby_flog_threshold_warning) . ')'
    let trail_e = syntastic#util#float2str(g:syntastic_ruby_flog_threshold_error) . ')'
    for e in loclist
      let score = syntastic#util#str2float(e['text'])

      let e['text'] = 'Complexity is too high (' . syntastic#util#float2str(score) . '/'
      if score < g:syntastic_ruby_flog_threshold_warning
          let e['valid'] = 0
      elseif score < g:syntastic_ruby_flog_threshold_error
          let e['type'] = 'W'
          let e['text'] .= trail_w
      else
          let e['type'] = 'E'
          let e['text'] .= trail_e
      endif
    endfor

    return loclist
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'ruby',
    \ 'name': 'flog'})

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
