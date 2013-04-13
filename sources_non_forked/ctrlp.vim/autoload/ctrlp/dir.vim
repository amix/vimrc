" =============================================================================
" File:          autoload/ctrlp/dir.vim
" Description:   Directory extension
" Author:        Kien Nguyen <github.com/kien>
" =============================================================================

" Init {{{1
if exists('g:loaded_ctrlp_dir') && g:loaded_ctrlp_dir
	fini
en
let [g:loaded_ctrlp_dir, g:ctrlp_newdir] = [1, 0]

let s:ars = ['s:maxdepth', 's:maxfiles', 's:compare_lim', 's:glob', 's:caching']

cal add(g:ctrlp_ext_vars, {
	\ 'init': 'ctrlp#dir#init('.join(s:ars, ', ').')',
	\ 'accept': 'ctrlp#dir#accept',
	\ 'lname': 'dirs',
	\ 'sname': 'dir',
	\ 'type': 'path',
	\ 'specinput': 1,
	\ })

let s:id = g:ctrlp_builtins + len(g:ctrlp_ext_vars)

let s:dircounts = {}
" Utilities {{{1
fu! s:globdirs(dirs, depth)
	let entries = split(globpath(a:dirs, s:glob), "\n")
	let [dirs, depth] = [ctrlp#dirnfile(entries)[0], a:depth + 1]
	cal extend(g:ctrlp_alldirs, dirs)
	let nr = len(g:ctrlp_alldirs)
	if !empty(dirs) && !s:max(nr, s:maxfiles) && depth <= s:maxdepth
		sil! cal ctrlp#progress(nr)
		cal map(dirs, 'ctrlp#utils#fnesc(v:val, "g", ",")')
		cal s:globdirs(join(dirs, ','), depth)
	en
endf

fu! s:max(len, max)
	retu a:max && a:len > a:max
endf

fu! s:nocache()
	retu !s:caching || ( s:caching > 1 && get(s:dircounts, s:cwd) < s:caching )
endf
" Public {{{1
fu! ctrlp#dir#init(...)
	let s:cwd = getcwd()
	for each in range(len(s:ars))
		let {s:ars[each]} = a:{each + 1}
	endfo
	let cadir = ctrlp#utils#cachedir().ctrlp#utils#lash().'dir'
	let cafile = cadir.ctrlp#utils#lash().ctrlp#utils#cachefile('dir')
	if g:ctrlp_newdir || s:nocache() || !filereadable(cafile)
		let [s:initcwd, g:ctrlp_alldirs] = [s:cwd, []]
		if !ctrlp#igncwd(s:cwd)
			cal s:globdirs(ctrlp#utils#fnesc(s:cwd, 'g', ','), 0)
		en
		cal ctrlp#rmbasedir(g:ctrlp_alldirs)
		if len(g:ctrlp_alldirs) <= s:compare_lim
			cal sort(g:ctrlp_alldirs, 'ctrlp#complen')
		en
		cal ctrlp#utils#writecache(g:ctrlp_alldirs, cadir, cafile)
		let g:ctrlp_newdir = 0
	el
		if !( exists('s:initcwd') && s:initcwd == s:cwd )
			let s:initcwd = s:cwd
			let g:ctrlp_alldirs = ctrlp#utils#readfile(cafile)
		en
	en
	cal extend(s:dircounts, { s:cwd : len(g:ctrlp_alldirs) })
	retu g:ctrlp_alldirs
endf

fu! ctrlp#dir#accept(mode, str)
	let path = a:mode == 'h' ? getcwd() : s:cwd.ctrlp#utils#lash().a:str
	if a:mode =~ 't\|v\|h'
		cal ctrlp#exit()
	en
	cal ctrlp#setdir(path, a:mode =~ 't\|h' ? 'chd!' : 'lc!')
	if a:mode == 'e'
		sil! cal ctrlp#statusline()
		cal ctrlp#setlines(s:id)
		cal ctrlp#recordhist()
		cal ctrlp#prtclear()
	en
endf

fu! ctrlp#dir#id()
	retu s:id
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
