" =============================================================================
" File:          autoload/ctrlp/autoignore.vim
" Description:   Auto-ignore Extension
" Author:        Ludovic Chabant <github.com/ludovicchabant>
" =============================================================================


" Global Settings {{{

if exists('g:ctrlp_autoignore_loaded') && g:ctrlp_autoignore_loaded
		\ && !g:ctrlp_autoignore_debug
	finish
endif
let g:ctrlp_autoignore_loaded = 1

if !exists('g:ctrlp_autoignore_debug')
	let g:ctrlp_autoignore_debug = 0
endif

if !exists('g:ctrlp_autoignore_trace')
	let g:ctrlp_autoignore_trace = 0
endif

" }}}

" Initialization {{{

if !exists('g:ctrlp_custom_ignore')
	let g:ctrlp_custom_ignore = {}
endif
let g:ctrlp_custom_ignore['func'] = 'ctrlp#autoignore#ignore'
let g:ctrlp_custom_ignore['func-init'] = 'ctrlp#autoignore#ignore_init'
let g:ctrlp_custom_ignore['func-close'] = 'ctrlp#autoignore#ignore_close'

if !exists('g:ctrlp_root_markers')
	let g:ctrlp_root_markers = []
endif
call add(g:ctrlp_root_markers, '.ctrlpignore')

" }}}

" Internals {{{

function! s:trace(message) abort
    if g:ctrlp_autoignore_trace
        echom "ctrlp_autoignore: " . a:message
    endif
endfunction

let s:proj_cache = {}
let s:active_cwd = ''
let s:active_cwd_len = 0
let s:active_patterns = []
let s:changed_wildignore = 0
let s:prev_wildignore = ''

function! s:load_project_patterns(root_dir) abort
	let l:ign_path = a:root_dir . '/.ctrlpignore'
	if !filereadable(l:ign_path)
		call s:trace("No pattern file at: " . l:ign_path)
		return []
	endif
	let l:cursyntax = 'regexp'
	let l:knownsyntaxes = ['regexp', 'wildignore']
	let l:patterns = []
	let l:lines = readfile(l:ign_path)
	for line in l:lines
		" Comment line?
		if match(line, '\v^\s*$') >= 0 || match(line, '\v^\s*#') >= 0
			continue
		endif
		" Syntax change?
		let l:matches = matchlist(line, '\v^syntax:\s?(\w+)\s*$')
		if len(l:matches) > 0
			let l:cursyntax = l:matches[1]
			if index(l:knownsyntaxes, l:cursyntax) < 0
				echoerr "ctrlp_autoignore: Unknown syntax '".l:cursyntax."' in: ".l:ign_path
			endif
			continue
		endif
		" Patterns!
		let l:matches = matchlist(line, '\v^((dir|file|link)\:)?(.*)')
		let l:mtype = l:matches[2]
		let l:mpat = l:matches[3]
		call add(l:patterns, {'syn': l:cursyntax, 'type': l:mtype, 'pat': l:mpat})
	endfor
	call s:trace("Loaded " . len(l:patterns) . " patterns from: " . l:ign_path)
	return l:patterns
endfunction

function! s:get_project_patterns(root_dir) abort
	let l:ign_path = a:root_dir . '/.ctrlpignore'
	let l:ign_mtime = getftime(l:ign_path)
	let l:patterns = get(s:proj_cache, a:root_dir)
	if type(l:patterns) == type({})
		" Check that these patterns are still valid.
		if l:ign_mtime < 0
			" File got deleted! :(
			let l:patterns['pats'] = []
			return l:patterns['pats']
		elseif l:ign_mtime <= l:patterns['mtime']
			" File hasn't changed! :)
			return l:patterns['pats']
		endif
	endif

	call s:trace("Loading patterns for project: " . a:root_dir)
	let l:loaded = s:load_project_patterns(a:root_dir)
	let s:proj_cache[a:root_dir] = {
	\'mtime': localtime(),
	\'pats': l:loaded}
	return l:loaded
endfunction

" The custom ignore function that CtrlP will be using in addition to
" normal pattern-based matching.
function! ctrlp#autoignore#ignore(item, type) abort
	let l:cnv_item = tr(strpart(a:item, s:active_cwd_len), "\\", "/")
	for pat in s:active_patterns
		if pat['syn'] != 'regexp'
			continue
		endif
		if pat['type'] == '' || pat['type'] == a:type
			if match(l:cnv_item, pat['pat']) >= 0
				call s:trace("Ignoring ".l:cnv_item." because of ".pat['pat'])
				return 1
			endif
		endif
	endfor
	return 0
endfunction

function! ctrlp#autoignore#ignore_init() abort
	let l:root = getcwd()
	let s:active_cwd = l:root
	" len+1 is for including the next separator after the root.
	let s:active_cwd_len = len(l:root) + 1
	let s:active_patterns = s:get_project_patterns(l:root)
	call s:trace("Got ".len(s:active_patterns)." patterns for ".l:root)

	let s:changed_wildignore = 0
	let s:prev_wildignore = &wildignore
	for pat in s:active_patterns
		if pat['syn'] == 'wildignore'
			execute 'set wildignore+='.pat['pat']
			let s:changed_wildignore = 1
		endif
	endfor
	if s:changed_wildignore
		call s:trace("Set wildignore to ".&wildignore)
	endif
endfunction

function! ctrlp#autoignore#ignore_close() abort
	if s:changed_wildignore
		execute 'set wildignore='.s:prev_wildignore
		let s:prev_wildignore = ''
		call s:trace("Set wildignore back to ".&wildignore)
	endif
endfunction

" List patterns for a given project's root.
function! ctrlp#autoignore#get_patterns(root_dir) abort
	let l:patterns = s:get_project_patterns(a:root_dir)
	for pat in l:patterns
		let l:prefix = pat['type'] == '' ? '(all)' : pat['type']
		echom l:prefix . ':' . pat['pat']
	endfor
endfunction

" }}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
