" =============================================================================
" File:          autoload/ctrlp/undo.vim
" Description:   Undo extension
" Author:        Kien Nguyen <github.com/kien>
" =============================================================================

" Init {{{1
if ( exists('g:loaded_ctrlp_undo') && g:loaded_ctrlp_undo )
	fini
en
let g:loaded_ctrlp_undo = 1

cal add(g:ctrlp_ext_vars, {
	\ 'init': 'ctrlp#undo#init()',
	\ 'accept': 'ctrlp#undo#accept',
	\ 'lname': 'undo',
	\ 'sname': 'udo',
	\ 'enter': 'ctrlp#undo#enter()',
	\ 'exit': 'ctrlp#undo#exit()',
	\ 'type': 'line',
	\ 'sort': 0,
	\ 'nolim': 1,
	\ })

let s:id = g:ctrlp_builtins + len(g:ctrlp_ext_vars)

let s:text = map(['second', 'seconds', 'minutes', 'hours', 'days', 'weeks',
	\ 'months', 'years'], '" ".v:val." ago"')
" Utilities {{{1
fu! s:getundo()
	if exists('*undotree')
		\ && ( v:version > 703 || ( v:version == 703 && has('patch005') ) )
		retu [1, undotree()]
	el
		redi => result
		sil! undol
		redi END
		retu [0, split(result, "\n")[1:]]
	en
endf

fu! s:flatten(tree, cur)
	let flatdict = {}
	for each in a:tree
		let saved = has_key(each, 'save') ? 'saved' : ''
		let current = each['seq'] == a:cur ? 'current' : ''
		cal extend(flatdict, { each['seq'] : [each['time'], saved, current] })
		if has_key(each, 'alt')
			cal extend(flatdict, s:flatten(each['alt'], a:cur))
		en
	endfo
	retu flatdict
endf

fu! s:elapsed(nr)
	let [text, time] = [s:text, localtime() - a:nr]
	let mins = time / 60
	let hrs  = time / 3600
	let days = time / 86400
	let wks  = time / 604800
	let mons = time / 2592000
	let yrs  = time / 31536000
	if yrs > 1
		retu yrs.text[7]
	elsei mons > 1
		retu mons.text[6]
	elsei wks > 1
		retu wks.text[5]
	elsei days > 1
		retu days.text[4]
	elsei hrs > 1
		retu hrs.text[3]
	elsei mins > 1
		retu mins.text[2]
	elsei time == 1
		retu time.text[0]
	elsei time < 120
		retu time.text[1]
	en
endf

fu! s:syntax()
	if ctrlp#nosy() | retu | en
	for [ke, va] in items({'T': 'Directory', 'Br': 'Comment', 'Nr': 'String',
		\ 'Sv': 'Comment', 'Po': 'Title'})
		cal ctrlp#hicheck('CtrlPUndo'.ke, va)
	endfo
	sy match CtrlPUndoT '\v\d+ \zs[^ ]+\ze|\d+:\d+:\d+'
	sy match CtrlPUndoBr '\[\|\]'
	sy match CtrlPUndoNr '\[\d\+\]' contains=CtrlPUndoBr
	sy match CtrlPUndoSv 'saved'
	sy match CtrlPUndoPo 'current'
endf

fu! s:dict2list(dict)
	for ke in keys(a:dict)
		let a:dict[ke][0] = s:elapsed(a:dict[ke][0])
	endfo
	retu map(keys(a:dict), 'eval(''[v:val, a:dict[v:val]]'')')
endf

fu! s:compval(...)
	retu a:2[0] - a:1[0]
endf

fu! s:format(...)
	let saved = !empty(a:1[1][1]) ? ' '.a:1[1][1] : ''
	let current = !empty(a:1[1][2]) ? ' '.a:1[1][2] : ''
	retu a:1[1][0].' ['.a:1[0].']'.saved.current
endf

fu! s:formatul(...)
	let parts = matchlist(a:1,
		\ '\v^\s+(\d+)\s+\d+\s+([^ ]+\s?[^ ]+|\d+\s\w+\s\w+)(\s*\d*)$')
	retu parts == [] ? '----'
		\ : parts[2].' ['.parts[1].']'.( parts[3] != '' ? ' saved' : '' )
endf
" Public {{{1
fu! ctrlp#undo#init()
	let entries = s:undos[0] ? s:undos[1]['entries'] : s:undos[1]
	if empty(entries) | retu [] | en
	if !exists('s:lines')
		if s:undos[0]
			let entries = s:dict2list(s:flatten(entries, s:undos[1]['seq_cur']))
			let s:lines = map(sort(entries, 's:compval'), 's:format(v:val)')
		el
			let s:lines = map(reverse(entries), 's:formatul(v:val)')
		en
	en
	cal s:syntax()
	retu s:lines
endf

fu! ctrlp#undo#accept(mode, str)
	let undon = matchstr(a:str, '\[\zs\d\+\ze\]')
	if empty(undon) | retu | en
	cal ctrlp#exit()
	exe 'u' undon
endf

fu! ctrlp#undo#id()
	retu s:id
endf

fu! ctrlp#undo#enter()
	let s:undos = s:getundo()
endf

fu! ctrlp#undo#exit()
	unl! s:lines
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
