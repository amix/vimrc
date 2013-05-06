" =============================================================================
" File:          autoload/ctrlp/rtscript.vim
" Description:   Runtime scripts extension
" Author:        Kien Nguyen <github.com/kien>
" =============================================================================

" Init {{{1
if exists('g:loaded_ctrlp_rtscript') && g:loaded_ctrlp_rtscript
	fini
en
let [g:loaded_ctrlp_rtscript, g:ctrlp_newrts] = [1, 0]

cal add(g:ctrlp_ext_vars, {
	\ 'init': 'ctrlp#rtscript#init(s:caching)',
	\ 'accept': 'ctrlp#acceptfile',
	\ 'lname': 'runtime scripts',
	\ 'sname': 'rts',
	\ 'type': 'path',
	\ 'opmul': 1,
	\ })

let s:id = g:ctrlp_builtins + len(g:ctrlp_ext_vars)

let s:filecounts = {}
" Utilities {{{1
fu! s:nocache()
	retu g:ctrlp_newrts ||
		\ !s:caching || ( s:caching > 1 && get(s:filecounts, s:cwd) < s:caching )
endf
" Public {{{1
fu! ctrlp#rtscript#init(caching)
	let [s:caching, s:cwd] = [a:caching, getcwd()]
	if s:nocache() ||
		\ !( exists('g:ctrlp_rtscache') && g:ctrlp_rtscache[0] == &rtp )
		sil! cal ctrlp#progress('Indexing...')
		let entries = split(globpath(ctrlp#utils#fnesc(&rtp, 'g'), '**/*.*'), "\n")
		cal filter(entries, 'count(entries, v:val) == 1')
		let [entries, echoed] = [ctrlp#dirnfile(entries)[1], 1]
	el
		let [entries, results] = g:ctrlp_rtscache[2:3]
	en
	if s:nocache() ||
		\ !( exists('g:ctrlp_rtscache') && g:ctrlp_rtscache[:1] == [&rtp, s:cwd] )
		if !exists('echoed')
			sil! cal ctrlp#progress('Processing...')
		en
		let results = map(copy(entries), 'fnamemodify(v:val, '':.'')')
	en
	let [g:ctrlp_rtscache, g:ctrlp_newrts] = [[&rtp, s:cwd, entries, results], 0]
	cal extend(s:filecounts, { s:cwd : len(results) })
	retu results
endf

fu! ctrlp#rtscript#id()
	retu s:id
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
