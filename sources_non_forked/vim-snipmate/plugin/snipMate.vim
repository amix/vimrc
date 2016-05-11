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

" Save and reset 'cpo'
let s:save_cpo = &cpo
set cpo&vim

try
	call funcref#Function('')
catch /.*/
	echoe "you're missing vim-addon-mw-utils. See install instructions at ".expand('<sfile>:h:h').'/README.md'
endtry

if (!exists('g:snipMateSources'))
  let g:snipMateSources = {}
  " Default source: get snippets based on runtimepath
  let g:snipMateSources['default'] = funcref#Function('snipMate#DefaultPool')
endif

augroup SnipMateDetect
	au BufRead,BufNewFile *.snippet,*.snippets setlocal filetype=snippets
	au FileType snippets if expand('<afile>:e') =~# 'snippet$'
				\ | setlocal syntax=snippet
				\ | else
					\ | setlocal syntax=snippets
					\ | endif
augroup END

inoremap <silent> <Plug>snipMateNextOrTrigger  <C-R>=snipMate#TriggerSnippet()<CR>
snoremap <silent> <Plug>snipMateNextOrTrigger  <Esc>a<C-R>=snipMate#TriggerSnippet()<CR>
inoremap <silent> <Plug>snipMateTrigger        <C-R>=snipMate#TriggerSnippet(1)<CR>
inoremap <silent> <Plug>snipMateBack           <C-R>=snipMate#BackwardsSnippet()<CR>
snoremap <silent> <Plug>snipMateBack           <Esc>a<C-R>=snipMate#BackwardsSnippet()<CR>
inoremap <silent> <Plug>snipMateShow           <C-R>=snipMate#ShowAvailableSnips()<CR>
xnoremap <silent> <Plug>snipMateVisual         :<C-U>call <SID>grab_visual()<CR>gv"_c

" config variables
if !exists('g:snips_author')
	let g:snips_author = 'Me'
endif
if !exists('g:snipMate')
	let g:snipMate = {}
endif

" SnipMate inserts this string when no snippet expansion can be done
let g:snipMate['no_match_completion_feedkeys_chars'] =
			\ get(g:snipMate, 'no_match_completion_feedkeys_chars', "\t")

" Add default scope aliases, without overriding user settings
let g:snipMate.scope_aliases = get(g:snipMate, 'scope_aliases', {})
if exists('g:snipMate_no_default_aliases')
	echom 'The g:snipMate_no_default_aliases option has been renamed.'
				\ 'See :h snipMate-options.'
endif
if (!exists('g:snipMate_no_default_aliases') || !g:snipMate_no_default_aliases)
			\ && (!exists('g:snipMate.no_default_aliases')
				\ || !g:snipMate.no_default_aliases)
	let g:snipMate.scope_aliases.objc =
				\ get(g:snipMate.scope_aliases, 'objc', 'c')
	let g:snipMate.scope_aliases.cpp =
				\ get(g:snipMate.scope_aliases, 'cpp', 'c')
	let g:snipMate.scope_aliases.cu =
				\ get(g:snipMate.scope_aliases, 'cu', 'c')
	let g:snipMate.scope_aliases.xhtml =
				\ get(g:snipMate.scope_aliases, 'xhtml', 'html')
	let g:snipMate.scope_aliases.html =
				\ get(g:snipMate.scope_aliases, 'html', 'javascript')
	let g:snipMate.scope_aliases.php =
				\ get(g:snipMate.scope_aliases, 'php', 'php,html,javascript')
	let g:snipMate.scope_aliases.ur =
				\ get(g:snipMate.scope_aliases, 'ur', 'html,javascript')
	let g:snipMate.scope_aliases.mxml =
				\ get(g:snipMate.scope_aliases, 'mxml', 'actionscript')
	let g:snipMate.scope_aliases.eruby =
				\ get(g:snipMate.scope_aliases, 'eruby', 'eruby-rails,html')
	let g:snipMate.scope_aliases.scss =
				\ get(g:snipMate.scope_aliases, 'scss', 'css')
	let g:snipMate.scope_aliases.less =
				\ get(g:snipMate.scope_aliases, 'less', 'css')
endif

let g:snipMate['get_snippets'] = get(g:snipMate, 'get_snippets', funcref#Function("snipMate#GetSnippets"))

" List of paths where snippets/ dirs are located
let g:snipMate['snippet_dirs'] = get(g:snipMate, 'snippet_dirs', split(&rtp, ','))
if type(g:snipMate['snippet_dirs']) != type([])
	echohl WarningMsg
	echom "g:snipMate['snippet_dirs'] must be a List"
	echohl None
endif

" _ is default scope added always
"
" &ft honors multiple filetypes and syntax such as in set ft=html.javascript syntax=FOO
let g:snipMate['get_scopes'] = get(g:snipMate, 'get_scopes', funcref#Function('return split(&ft,"\\.")+[&syntax, "_"]'))

" Modified from Luc Hermitte's function on StackOverflow
" <http://stackoverflow.com/a/1534347>
function! s:grab_visual() abort
	let a_save = @a
	try
		normal! gv"ay
		let b:snipmate_visual = @a
	finally
		let @a = a_save
	endtry
endfunction

" TODO: Allow specifying an arbitrary snippets file
function! s:load_scopes(bang, ...) abort
	let gb = a:bang ? g: : b:
	let gb.snipMate = get(gb, 'snipMate', {})
	let gb.snipMate.scope_aliases = get(gb.snipMate, 'scope_aliases', {})
	let gb.snipMate.scope_aliases['_'] = join(split(get(gb.snipMate.scope_aliases, '_', ''), ',') + a:000, ',')
endfunction

command! -bang -bar -nargs=+ SnipMateLoadScope
			\ call s:load_scopes(<bang>0, <f-args>)

" Edit snippet files
command! SnipMateOpenSnippetFiles call snipMate#OpenSnippetFiles()

" restore 'cpo'
let &cpo = s:save_cpo

" vim:noet:sw=4:ts=4:ft=vim
