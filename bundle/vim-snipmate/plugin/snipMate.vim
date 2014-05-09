" File:          snipMate.vim
" Description:   snipMate.vim implements some of TextMate's snippets features in
"                Vim. A snippet is a piece of often-typed text that you can
"                insert into your document using a trigger word followed by a "<tab>".
"
"                For more help see snipMate.txt; you can do this by using:
"                :helptags ~/.vim/doc
"                :h SnipMate

if exists('loaded_snips') || &cp || version < 700
	finish
endif
let loaded_snips = 1
if !exists('snips_author') | let snips_author = 'Me' | endif
" save and reset 'cpo'
let s:save_cpo = &cpo
set cpo&vim

try
	call funcref#Function('')
catch /.*/
	echoe "you're missing vim-addon-mw-utils. See install instructions at ".expand('<sfile>:h:h').'/README.md'
endtry

if (!exists('g:snipMateSources'))
  let g:snipMateSources = {}
  " default source: get snippets based on runtimepath:
  let g:snipMateSources['default'] = funcref#Function('snipMate#DefaultPool')
endif

au BufRead,BufNewFile *.snippet set ft=snippet
au FileType snippet setl noet nospell

au BufRead,BufNewFile *.snippets set ft=snippets
au FileType snippets setl noet nospell fdm=expr fde=getline(v:lnum)!~'^\\t\\\\|^$'?'>1':1

inoremap <silent> <Plug>snipMateNextOrTrigger  <C-R>=snipMate#TriggerSnippet()<CR>
snoremap <silent> <Plug>snipMateNextOrTrigger  <Esc>a<C-R>=snipMate#TriggerSnippet()<CR>
inoremap <silent> <Plug>snipMateTrigger        <C-R>=snipMate#TriggerSnippet(1)<CR>
inoremap <silent> <Plug>snipMateBack           <C-R>=snipMate#BackwardsSnippet()<CR>
snoremap <silent> <Plug>snipMateBack           <Esc>a<C-R>=snipMate#BackwardsSnippet()<CR>
inoremap <silent> <Plug>snipMateShow           <C-R>=snipMate#ShowAvailableSnips()<CR>
xnoremap <silent> <Plug>snipMateVisual         :<C-U>call <SID>grab_visual()<CR>i

" config which can be overridden (shared lines)
if !exists('g:snipMate')
  let g:snipMate = {}
endif
let s:snipMate = g:snipMate

let s:snipMate['get_snippets'] = get(s:snipMate, 'get_snippets', funcref#Function("snipMate#GetSnippets"))

" old snippets_dir: function returning list of paths which is used to read
" snippets. You can replace it with your own implementation. Defaults to all
" directories in &rtp/snippets/*
let s:snipMate['snippet_dirs'] = get(s:snipMate, 'snippet_dirs', funcref#Function('return split(&runtimepath,",")'))
if type(s:snipMate['snippet_dirs']) == type([])
	call map(s:snipMate['snippet_dirs'], 'expand(v:val)')
endif

" _ is default scope added always
"
" &ft honors multiple filetypes and syntax such as in set ft=html.javascript syntax=FOO
let s:snipMate['get_scopes'] = get(s:snipMate, 'get_scopes', funcref#Function('return split(&ft,"\\.")+[&syntax, "_"]'))

" dummy for compatibility - will be removed
" moving to autoload to improve loading speed and debugging
fun! TriggerSnippet()
	echoe "replace TriggerSnippet by snipMate#TriggerSnippet, please!"
	return snipMate#TriggerSnippet()
endf
fun! BackwardSnippet()
	echoe "replace BackwardSnippet by snipMate#BackwardsSnippet, please!"
	return snipMate#BackwardsSnippet()
endf

" Modified from Luc Hermitte's function on StackOverflow
" <http://stackoverflow.com/a/1534347>
function! s:grab_visual()
	let a_save = @a
	try
		normal! gv"ad
		let b:snipmate_content_visual = @a
	finally
		let @a = a_save
	endtry
endfunction

" restore 'cpo'
let &cpo = s:save_cpo

" vim:noet:sw=4:ts=4:ft=vim
