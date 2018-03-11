" Vim syntax file
" Language: Vue.js
" Maintainer: Eduardo San Martin Morote

if exists("b:current_syntax")
  finish
endif

runtime! syntax/html.vim
unlet! b:current_syntax

""
" Get the pattern for a HTML {name} attribute with {value}.
function! s:attr(name, value)
  return a:name . '=\("\|''\)[^\1]*' . a:value . '[^\1]*\1'
endfunction

""
" Check whether a syntax file for a given {language} exists.
function! s:syntax_available(language)
  return !empty(globpath(&runtimepath, 'syntax/' . a:language . '.vim'))
endfunction

""
" Register {language} for a given {tag}. If [attr_override] is given and not
" empty, it will be used for the attribute pattern.
function! s:register_language(language, tag, ...)
  let attr_override = a:0 ? a:1 : ''
  let attr = !empty(attr_override) ? attr_override : s:attr('lang', a:language)

  if s:syntax_available(a:language)
    execute 'syntax include @' . a:language . ' syntax/' . a:language . '.vim'
    unlet! b:current_syntax
    execute 'syntax region vue_' . a:language
          \ 'keepend'
          \ 'start=/<' . a:tag . ' \_[^>]*' . attr . '\_[^>]*>/'
          \ 'end="</' . a:tag . '>"me=s-1'
          \ 'contains=@' . a:language . ',vueSurroundingTag'
          \ 'fold'
  endif
endfunction

if !exists("g:vue_disable_pre_processors") || !g:vue_disable_pre_processors
  call s:register_language('pug', 'template', s:attr('lang', '\%(pug\|jade\)'))
  call s:register_language('slm', 'template')
  call s:register_language('handlebars', 'template')
  call s:register_language('haml', 'template')
  call s:register_language('typescript', 'script', '\%(lang=\("\|''\)[^\1]*\(ts\|typescript\)[^\1]*\1\|ts\)')
  call s:register_language('coffee', 'script')
  call s:register_language('stylus', 'style')
  call s:register_language('sass', 'style')
  call s:register_language('scss', 'style')
  call s:register_language('less', 'style')
endif

syn region  vueSurroundingTag   contained start=+<\(script\|style\|template\)+ end=+>+ fold contains=htmlTagN,htmlString,htmlArg,htmlValue,htmlTagError,htmlEvent
syn keyword htmlSpecialTagName  contained template
syn keyword htmlArg             contained scoped ts
syn match   htmlArg "[@v:][-:.0-9_a-z]*\>" contained

let b:current_syntax = "vue"
