" =============================================================================
" File:          autoload/ctrlp/mixed.vim
" Description:   Mixing Files + MRU + Buffers
" Author:        Kien Nguyen <github.com/kien>
" =============================================================================

" Init {{{1
if exists('g:loaded_ctrlp_mixed') && g:loaded_ctrlp_mixed
	fini
en
let [g:loaded_ctrlp_mixed, g:ctrlp_newmix] = [1, 0]

cal add(g:ctrlp_ext_vars, {
	\ 'init': 'ctrlp#mixed#init(s:compare_lim)',
	\ 'accept': 'ctrlp#acceptfile',
	\ 'lname': 'fil + mru + buf',
	\ 'sname': 'mix',
	\ 'type': 'path',
	\ 'opmul': 1,
	\ 'specinput': 1,
	\ })

let s:id = g:ctrlp_builtins + len(g:ctrlp_ext_vars)
" Utilities {{{1
fu! s:newcache(cwd)
	if g:ctrlp_newmix || !has_key(g:ctrlp_allmixes, 'data') | retu 1 | en
	retu g:ctrlp_allmixes['cwd'] != a:cwd
		\ || g:ctrlp_allmixes['filtime'] < getftime(ctrlp#utils#cachefile())
		\ || g:ctrlp_allmixes['mrutime'] < getftime(ctrlp#mrufiles#cachefile())
		\ || g:ctrlp_allmixes['bufs'] < len(ctrlp#mrufiles#bufs())
endf

fu! s:getnewmix(cwd, clim)
	if g:ctrlp_newmix
		cal ctrlp#mrufiles#refresh('raw')
		let g:ctrlp_newcache = 1
	en
	let g:ctrlp_lines = copy(ctrlp#files())
	cal ctrlp#progress('Mixing...')
	let mrufs = copy(ctrlp#mrufiles#list('raw'))
	if exists('+ssl') && &ssl
		cal map(mrufs, 'tr(v:val, "\\", "/")')
	en
	let allbufs = map(ctrlp#buffers(), 'fnamemodify(v:val, ":p")')
	let [bufs, ubufs] = [[], []]
	for each in allbufs
		cal add(filereadable(each) ? bufs : ubufs, each)
	endfo
	let mrufs = bufs + filter(mrufs, 'index(bufs, v:val) < 0')
	if len(mrufs) > len(g:ctrlp_lines)
		cal filter(mrufs, 'stridx(v:val, a:cwd)')
	el
		let cwd_mrufs = filter(copy(mrufs), '!stridx(v:val, a:cwd)')
		let cwd_mrufs = ctrlp#rmbasedir(cwd_mrufs)
		for each in cwd_mrufs
			let id = index(g:ctrlp_lines, each)
			if id >= 0 | cal remove(g:ctrlp_lines, id) | en
		endfo
	en
	let mrufs += ubufs
	cal map(mrufs, 'fnamemodify(v:val, ":.")')
	let g:ctrlp_lines = len(mrufs) > len(g:ctrlp_lines)
		\ ? g:ctrlp_lines + mrufs : mrufs + g:ctrlp_lines
	if len(g:ctrlp_lines) <= a:clim
		cal sort(g:ctrlp_lines, 'ctrlp#complen')
	en
	let g:ctrlp_allmixes = { 'filtime': getftime(ctrlp#utils#cachefile()),
		\ 'mrutime': getftime(ctrlp#mrufiles#cachefile()), 'cwd': a:cwd,
		\ 'bufs': len(ctrlp#mrufiles#bufs()), 'data': g:ctrlp_lines }
endf
" Public {{{1
fu! ctrlp#mixed#init(clim)
	let cwd = getcwd()
	if s:newcache(cwd)
		cal s:getnewmix(cwd, a:clim)
	el
		let g:ctrlp_lines = g:ctrlp_allmixes['data']
	en
	let g:ctrlp_newmix = 0
	retu g:ctrlp_lines
endf

fu! ctrlp#mixed#id()
	retu s:id
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
