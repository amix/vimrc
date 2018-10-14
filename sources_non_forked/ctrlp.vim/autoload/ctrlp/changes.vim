" =============================================================================
" File:          autoload/ctrlp/changes.vim
" Description:   Change list extension
" Author:        Kien Nguyen <github.com/kien>
" =============================================================================

" Init {{{1
if exists('g:loaded_ctrlp_changes') && g:loaded_ctrlp_changes
	fini
en
let g:loaded_ctrlp_changes = 1

cal add(g:ctrlp_ext_vars, {
	\ 'init': 'ctrlp#changes#init(s:bufnr, s:crbufnr)',
	\ 'accept': 'ctrlp#changes#accept',
	\ 'lname': 'changes',
	\ 'sname': 'chs',
	\ 'exit': 'ctrlp#changes#exit()',
	\ 'type': 'tabe',
	\ 'sort': 0,
	\ 'nolim': 1,
	\ })

let s:id = g:ctrlp_builtins + len(g:ctrlp_ext_vars)
" Utilities {{{1
fu! s:changelist(bufnr)
	sil! exe 'noa hid b' a:bufnr
	redi => result
	sil! changes
	redi END
	retu map(split(result, "\n")[1:], 'tr(v:val, "	", " ")')
endf

fu! s:process(clines, ...)
	let [clines, evas] = [[], []]
	for each in a:clines
		let parts = matchlist(each, '\v^.\s*\d+\s+(\d+)\s+(\d+)\s(.*)$')
		if !empty(parts)
			if parts[3] == '' | let parts[3] = ' ' | en
			cal add(clines, parts[3].'	|'.a:1.':'.a:2.'|'.parts[1].':'.parts[2].'|')
		en
	endfo
	retu reverse(filter(clines, 'count(clines, v:val) == 1'))
endf

fu! s:syntax()
	if !ctrlp#nosy()
		cal ctrlp#hicheck('CtrlPBufName', 'Directory')
		cal ctrlp#hicheck('CtrlPTabExtra', 'Comment')
		sy match CtrlPBufName '\t|\d\+:\zs[^|]\+\ze|\d\+:\d\+|$'
		sy match CtrlPTabExtra '\zs\t.*\ze$' contains=CtrlPBufName
	en
endf
" Public {{{1
fu! ctrlp#changes#init(original_bufnr, bufnr)
	let bufnr = exists('s:bufnr') ? s:bufnr : a:bufnr
	let bufs = exists('s:clmode') && s:clmode ? ctrlp#buffers('id') : [bufnr]
	cal filter(bufs, 'v:val > 0')
	let [swb, &swb] = [&swb, '']
	let lines = []
	for each in bufs
		let bname = bufname(each)
		let fnamet = fnamemodify(bname == '' ? '[No Name]' : bname, ':t')
		cal extend(lines, s:process(s:changelist(each), each, fnamet))
	endfo
	sil! exe 'noa hid b' a:original_bufnr
	let &swb = swb
	cal ctrlp#syntax()
	cal s:syntax()
	retu lines
endf

fu! ctrlp#changes#accept(mode, str)
	let info = matchlist(a:str, '\t|\(\d\+\):[^|]\+|\(\d\+\):\(\d\+\)|$')
	let bufnr = str2nr(get(info, 1))
	if bufnr
		cal ctrlp#acceptfile(a:mode, bufnr)
		cal cursor(get(info, 2), get(info, 3))
		sil! norm! zvzz
	en
endf

fu! ctrlp#changes#cmd(mode, ...)
	let s:clmode = a:mode
	if a:0 && !empty(a:1)
		let s:clmode = 0
		let bname = a:1 =~# '^%$\|^#\d*$' ? expand(a:1) : a:1
		let s:bufnr = bufnr('^'.fnamemodify(bname, ':p').'$')
	en
	retu s:id
endf

fu! ctrlp#changes#exit()
	unl! s:clmode s:bufnr
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
