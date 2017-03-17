" =============================================================================
" File:          autoload/ctrlp/mrufiles.vim
" Description:   Most Recently Used Files extension
" Author:        Kien Nguyen <github.com/kien>
" =============================================================================

" Static variables {{{1
let [s:mrbs, s:mrufs] = [[], []]
let s:mruf_map_string = '!stridx(v:val, cwd) ? strpart(v:val, idx) : v:val'

fu! ctrlp#mrufiles#opts()
	let [pref, opts] = ['g:ctrlp_mruf_', {
		\ 'max': ['s:max', 250],
		\ 'include': ['s:in', ''],
		\ 'exclude': ['s:ex', ''],
		\ 'case_sensitive': ['s:cseno', 1],
		\ 'relative': ['s:re', 0],
		\ 'save_on_update': ['s:soup', 1],
		\ 'map_string': ['g:ctrlp_mruf_map_string', s:mruf_map_string],
		\ }]
	for [ke, va] in items(opts)
		let [{va[0]}, {pref.ke}] = [pref.ke, exists(pref.ke) ? {pref.ke} : va[1]]
	endfo
endf
cal ctrlp#mrufiles#opts()
" Utilities {{{1
fu! s:excl(fn)
	retu !empty({s:ex}) && a:fn =~# {s:ex}
endf

fu! s:mergelists()
	let diskmrufs = ctrlp#utils#readfile(ctrlp#mrufiles#cachefile())
	cal filter(diskmrufs, 'index(s:mrufs, v:val) < 0')
	let mrufs = s:mrufs + diskmrufs
	retu s:chop(mrufs)
endf

fu! s:chop(mrufs)
	if len(a:mrufs) > {s:max} | cal remove(a:mrufs, {s:max}, -1) | en
	retu a:mrufs
endf

fu! s:reformat(mrufs, ...)
	let cwd = getcwd()
	let cwd .= cwd !~ '[\/]$' ? ctrlp#utils#lash() : ''
	if {s:re}
		let cwd = exists('+ssl') ? tr(cwd, '/', '\') : cwd
		cal filter(a:mrufs, '!stridx(v:val, cwd)')
	en
	if a:0 && a:1 == 'raw' | retu a:mrufs | en
	let idx = strlen(cwd)
	if exists('+ssl') && &ssl
		let cwd = tr(cwd, '\', '/')
		cal map(a:mrufs, 'tr(v:val, "\\", "/")')
	en
	retu map(a:mrufs, g:ctrlp_mruf_map_string)
endf

fu! s:record(bufnr)
	if s:locked | retu | en
	let bufnr = a:bufnr + 0
	let bufname = bufname(bufnr)
	if bufnr > 0 && !empty(bufname)
		cal filter(s:mrbs, 'v:val != bufnr')
		cal insert(s:mrbs, bufnr)
		cal s:addtomrufs(bufname)
	en
endf

fu! s:addtomrufs(fname)
	let fn = fnamemodify(a:fname, get(g:, 'ctrlp_tilde_homedir', 0) ? ':p:~' : ':p')
	let fn = exists('+ssl') ? tr(fn, '/', '\') : fn
	let abs_fn = fnamemodify(fn,':p')
	if ( !empty({s:in}) && fn !~# {s:in} ) || ( !empty({s:ex}) && fn =~# {s:ex} )
		\ || !empty(getbufvar('^' . abs_fn . '$', '&bt')) || !filereadable(abs_fn)
		retu
	en
	let idx = index(s:mrufs, fn, 0, !{s:cseno})
	if idx
		cal filter(s:mrufs, 'v:val !='.( {s:cseno} ? '#' : '?' ).' fn')
		cal insert(s:mrufs, fn)
		if {s:soup} && idx < 0
			cal s:savetofile(s:mergelists())
		en
	en
endf

fu! s:savetofile(mrufs)
	cal ctrlp#utils#writecache(a:mrufs, s:cadir, s:cafile)
endf
" Public {{{1
fu! ctrlp#mrufiles#refresh(...)
	let mrufs = s:mergelists()
	cal filter(mrufs, '!empty(ctrlp#utils#glob(v:val, 1)) && !s:excl(v:val)')
	if exists('+ssl')
		cal map(mrufs, 'tr(v:val, "/", "\\")')
		cal map(s:mrufs, 'tr(v:val, "/", "\\")')
		let cond = 'count(mrufs, v:val, !{s:cseno}) == 1'
		cal filter(mrufs, cond)
		cal filter(s:mrufs, cond)
	en
	cal s:savetofile(mrufs)
	retu a:0 && a:1 == 'raw' ? [] : s:reformat(mrufs)
endf

fu! ctrlp#mrufiles#remove(files)
	let mrufs = []
	if a:files != []
		let mrufs = s:mergelists()
		let cond = 'index(a:files, v:val, 0, !{s:cseno}) < 0'
		cal filter(mrufs, cond)
		cal filter(s:mrufs, cond)
	en
	cal s:savetofile(mrufs)
	retu s:reformat(mrufs)
endf

fu! ctrlp#mrufiles#add(fn)
	if !empty(a:fn)
		cal s:addtomrufs(a:fn)
	en
endf

fu! ctrlp#mrufiles#list(...)
	retu a:0 ? a:1 == 'raw' ? s:reformat(s:mergelists(), a:1) : 0
		\ : s:reformat(s:mergelists())
endf

fu! ctrlp#mrufiles#bufs()
	retu s:mrbs
endf

fu! ctrlp#mrufiles#tgrel()
	let {s:re} = !{s:re}
endf

fu! ctrlp#mrufiles#cachefile()
	if !exists('s:cadir') || !exists('s:cafile')
		let s:cadir = ctrlp#utils#cachedir().ctrlp#utils#lash().'mru'
		let s:cafile = s:cadir.ctrlp#utils#lash().'cache.txt'
	en
	retu s:cafile
endf

fu! ctrlp#mrufiles#init()
	if !has('autocmd') | retu | en
	let s:locked = 0
	aug CtrlPMRUF
		au!
		au BufWinEnter,BufWinLeave,BufWritePost * cal s:record(expand('<abuf>', 1))
		au QuickFixCmdPre  *vimgrep* let s:locked = 1
		au QuickFixCmdPost *vimgrep* let s:locked = 0
		au VimLeavePre * cal s:savetofile(s:mergelists())
	aug END
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
