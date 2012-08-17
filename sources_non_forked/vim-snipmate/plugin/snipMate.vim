" File:          snipMate.vim
" Author:        Michael Sanders
" Version:       0.84
" Description:   snipMate.vim implements some of TextMate's snippets features in
"                Vim. A snippet is a piece of often-typed text that you can
"                insert into your document using a trigger word followed by a "<tab>".
"
"                For more help see snipMate.txt; you can do this by using:
"                :helptags ~/.vim/doc
"                :h snipMate.txt

if exists('loaded_snips') || &cp || version < 700
	finish
endif
let loaded_snips = 1
if !exists('snips_author') | let snips_author = 'Me' | endif

try
	call funcref#Function('')
catch /.*/
	echoe "you're missing vim-addon-mw-utils. See install instructions at ".expand('<sfile>:h:h').'/README.rst'
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

" _ is default scope added always
"
" &ft honors multiple filetypes and syntax such as in set ft=html.javascript syntax=FOO
let s:snipMate['get_scopes'] = get(s:snipMate, 'get_scopes', funcref#Function('return split(&ft,"\\.")+[&syntax, "_"]'))

if !exists('snippets_dir')
	let snippets_dir = substitute(globpath(&rtp, 'snippets/'), "\n", ',', 'g')
endif

" Processes a single-snippet file; optionally add the name of the parent
" directory for a snippet with multiple matches.
fun! s:ProcessFile(file, ft, ...)
	let keyword = fnamemodify(a:file, ':t:r')
	if keyword  == '' | return | endif
	try
		let text = join(readfile(a:file), "\n")
	catch /E484/
		echom "Error in snipMate.vim: couldn't read file: ".a:file
	endtry
	return a:0 ? MakeSnip(a:ft, a:1, text, keyword)
			\  : MakeSnip(a:ft, keyword, text)
endf

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

" vim:noet:sw=4:ts=4:ft=vim
