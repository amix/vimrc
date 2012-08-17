" =============================================================================
" File:          autoload/ctrlp/mrufiles.vim
" Description:   Most Recently Used Files extension
" Author:        Kien Nguyen <github.com/kien>
" =============================================================================

" Static variables {{{1
let [s:mrbs, s:mrufs] = [[], []]

fu! ctrlp#mrufiles#opts()
	let [pref, opts] = ['g:ctrlp_mruf_', {
		\ 'max': ['s:max', 250],
		\ 'include': ['s:in', ''],
		\ 'exclude': ['s:ex', ''],
		\ 'case_sensitive': ['s:cseno', 1],
		\ 'relative': ['s:re', 0],
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

fu! s:reformat(mrufs)
	if {s:re}
		let cwd = exists('+ssl') ? tr(getcwd(), '/', '\') : getcwd()
		cal filter(a:mrufs, '!stridx(v:val, cwd)')
	en
	retu map(a:mrufs, 'fnamemodify(v:val, ":.")')
endf

fu! s:record(bufnr)
	if s:locked | retu | en
	let bufnr = a:bufnr + 0
	if bufnr <= 0 | retu | en
	let bufname = bufname(bufnr)
	if empty(bufname) | retu | en
	let fn = fnamemodify(bufname, ':p')
	let fn = exists('+ssl') ? tr(fn, '/', '\') : fn
	cal filter(s:mrbs, 'v:val != bufnr')
	cal insert(s:mrbs, bufnr)
	if ( !empty({s:in}) && fn !~# {s:in} ) || ( !empty({s:ex}) && fn =~# {s:ex} )
		\ || !empty(&bt) || !filereadable(fn) | retu
	en
	cal filter(s:mrufs, 'v:val !='.( {s:cseno} ? '#' : '?' ).' fn')
	cal insert(s:mrufs, fn)
endf

fu! s:savetofile(mrufs)
	cal ctrlp#utils#writecache(a:mrufs, s:cadir, s:cafile)
endf
" Public {{{1
fu! ctrlp#mrufiles#refresh(...)
	let s:mrufs = s:mergelists()
	cal filter(s:mrufs, '!empty(ctrlp#utils#glob(v:val, 1)) && !s:excl(v:val)')
	if exists('+ssl')
		cal map(s:mrufs, 'tr(v:val, "/", "\\")')
		cal filter(s:mrufs, 'count(s:mrufs, v:val) == 1')
	en
	cal s:savetofile(s:mrufs)
	retu a:0 && a:1 == 'raw' ? [] : s:reformat(copy(s:mrufs))
endf

fu! ctrlp#mrufiles#remove(files)
	let s:mrufs = []
	if a:files != []
		let s:mrufs = s:mergelists()
		cal filter(s:mrufs, 'index(a:files, v:val, 0, '.(!{s:cseno}).') < 0')
	en
	cal s:savetofile(s:mrufs)
	retu s:reformat(copy(s:mrufs))
endf

fu! ctrlp#mrufiles#list(...)
	retu a:0 ? a:1 == 'raw' ? s:mergelists() : 0 : s:reformat(s:mergelists())
endf

fu! ctrlp#mrufiles#bufs()
	retu s:mrbs
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
		au BufAdd,BufEnter,BufLeave,BufUnload * cal s:record(expand('<abuf>', 1))
		au QuickFixCmdPre  *vimgrep* let s:locked = 1
		au QuickFixCmdPost *vimgrep* let s:locked = 0
		au VimLeavePre * cal s:savetofile(s:mergelists())
	aug END
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
