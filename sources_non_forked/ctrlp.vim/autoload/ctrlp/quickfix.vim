" =============================================================================
" File:          autoload/ctrlp/quickfix.vim
" Description:   Quickfix extension
" Author:        Kien Nguyen <github.com/kien>
" =============================================================================

" Init {{{1
if exists('g:loaded_ctrlp_quickfix') && g:loaded_ctrlp_quickfix
	fini
en
let g:loaded_ctrlp_quickfix = 1

cal add(g:ctrlp_ext_vars, {
	\ 'init': 'ctrlp#quickfix#init()',
	\ 'accept': 'ctrlp#quickfix#accept',
	\ 'lname': 'quickfix',
	\ 'sname': 'qfx',
	\ 'type': 'line',
	\ 'sort': 0,
	\ 'nolim': 1,
	\ })

let s:id = g:ctrlp_builtins + len(g:ctrlp_ext_vars)

fu! s:lineout(dict)
	retu printf('%s|%d:%d| %s', bufname(a:dict['bufnr']), a:dict['lnum'],
		\ a:dict['col'], matchstr(a:dict['text'], '\s*\zs.*\S'))
endf
" Utilities {{{1
fu! s:syntax()
	if !ctrlp#nosy()
		cal ctrlp#hicheck('CtrlPqfLineCol', 'Search')
		sy match CtrlPqfLineCol '|\zs\d\+:\d\+\ze|'
	en
endf
" Public {{{1
fu! ctrlp#quickfix#init()
	cal s:syntax()
	retu map(getqflist(), 's:lineout(v:val)')
endf

fu! ctrlp#quickfix#accept(mode, str)
	let vals = matchlist(a:str, '^\([^|]\+\ze\)|\(\d\+\):\(\d\+\)|')
	if vals == [] || vals[1] == '' | retu | en
	cal ctrlp#acceptfile(a:mode, vals[1])
	let cur_pos = getpos('.')[1:2]
	if cur_pos != [1, 1] && cur_pos != map(vals[2:3], 'str2nr(v:val)')
		mark '
	en
	cal cursor(vals[2], vals[3])
	sil! norm! zvzz
endf

fu! ctrlp#quickfix#id()
	retu s:id
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
