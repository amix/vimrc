" =============================================================================
" File:          autoload/ctrlp/bookmarkdir.vim
" Description:   Bookmarked directories extension
" Author:        Kien Nguyen <github.com/kien>
" =============================================================================

" Init {{{1
if exists('g:loaded_ctrlp_bookmarkdir') && g:loaded_ctrlp_bookmarkdir
	fini
en
let g:loaded_ctrlp_bookmarkdir = 1

cal add(g:ctrlp_ext_vars, {
	\ 'init': 'ctrlp#bookmarkdir#init()',
	\ 'accept': 'ctrlp#bookmarkdir#accept',
	\ 'lname': 'bookmarked dirs',
	\ 'sname': 'bkd',
	\ 'type': 'tabs',
	\ 'opmul': 1,
	\ 'nolim': 1,
	\ 'wipe': 'ctrlp#bookmarkdir#remove',
	\ })

let s:id = g:ctrlp_builtins + len(g:ctrlp_ext_vars)
" Utilities {{{1
fu! s:getinput(str, ...)
	echoh Identifier
	cal inputsave()
	let input = call('input', a:0 ? [a:str] + a:000 : [a:str])
	cal inputrestore()
	echoh None
	retu input
endf

fu! s:cachefile()
	if !exists('s:cadir') || !exists('s:cafile')
		let s:cadir = ctrlp#utils#cachedir().ctrlp#utils#lash().'bkd'
		let s:cafile = s:cadir.ctrlp#utils#lash().'cache.txt'
	en
	retu s:cafile
endf

fu! s:writecache(lines)
	cal ctrlp#utils#writecache(a:lines, s:cadir, s:cafile)
endf

fu! s:getbookmarks()
	retu ctrlp#utils#readfile(s:cachefile())
endf

fu! s:savebookmark(name, cwd)
	let cwds = exists('+ssl') ? [tr(a:cwd, '\', '/'), tr(a:cwd, '/', '\')] : [a:cwd]
	let entries = filter(s:getbookmarks(), 'index(cwds, s:parts(v:val)[1]) < 0')
	cal s:writecache(insert(entries, a:name.'	'.a:cwd))
endf

fu! s:setentries()
	let time = getftime(s:cachefile())
	if !( exists('s:bookmarks') && time == s:bookmarks[0] )
		let s:bookmarks = [time, s:getbookmarks()]
	en
endf

fu! s:parts(str)
	let mlist = matchlist(a:str, '\v([^\t]+)\t(.*)$')
	retu mlist != [] ? mlist[1:2] : ['', '']
endf

fu! s:process(entries, type)
	retu map(a:entries, 's:modify(v:val, a:type)')
endf

fu! s:modify(entry, type)
	let [name, dir] = s:parts(a:entry)
	let dir = fnamemodify(dir, a:type)
	retu name.'	'.( dir == '' ? '.' : dir )
endf

fu! s:msg(name, cwd)
	redr
	echoh Identifier | echon 'Bookmarked ' | echoh Constant
	echon a:name.' ' | echoh Directory | echon a:cwd
	echoh None
endf

fu! s:syntax()
	if !ctrlp#nosy()
		cal ctrlp#hicheck('CtrlPBookmark', 'Identifier')
		cal ctrlp#hicheck('CtrlPTabExtra', 'Comment')
		sy match CtrlPBookmark '^> [^\t]\+' contains=CtrlPLinePre
		sy match CtrlPTabExtra '\zs\t.*\ze$'
	en
endf
" Public {{{1
fu! ctrlp#bookmarkdir#init()
	cal s:setentries()
	cal s:syntax()
	retu s:process(copy(s:bookmarks[1]), ':.')
endf

fu! ctrlp#bookmarkdir#accept(mode, str)
	let parts = s:parts(s:modify(a:str, ':p'))
	cal call('s:savebookmark', parts)
	if a:mode =~ 't\|v\|h'
		cal ctrlp#exit()
	en
	cal ctrlp#setdir(parts[1], a:mode =~ 't\|h' ? 'chd!' : 'lc!')
	if a:mode == 'e'
		cal ctrlp#switchtype(0)
		cal ctrlp#recordhist()
		cal ctrlp#prtclear()
	en
endf

fu! ctrlp#bookmarkdir#add(dir, ...)
	let str = 'Directory to bookmark: '
	let cwd = a:dir != '' ? a:dir : s:getinput(str, getcwd(), 'dir')
	if cwd == '' | retu | en
	let cwd = fnamemodify(cwd, ':p')
	let name = a:0 && a:1 != '' ? a:1 : s:getinput('Bookmark as: ', cwd)
	if name == '' | retu | en
	let name = tr(name, '	', ' ')
	cal s:savebookmark(name, cwd)
	cal s:msg(name, cwd)
endf

fu! ctrlp#bookmarkdir#remove(entries)
	cal s:process(a:entries, ':p')
	cal s:writecache(a:entries == [] ? [] :
		\ filter(s:getbookmarks(), 'index(a:entries, v:val) < 0'))
	cal s:setentries()
	retu s:process(copy(s:bookmarks[1]), ':.')
endf

fu! ctrlp#bookmarkdir#id()
	retu s:id
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
