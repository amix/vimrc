" =============================================================================
" File:          autoload/ctrlp.vim
" Description:   Fuzzy file, buffer, mru, tag, etc finder.
" Author:        Kien Nguyen <github.com/kien>
" Version:       1.78
" =============================================================================

" ** Static variables {{{1
fu! s:ignore() "{{{2
	let igdirs = [
		\ '\.git$',
		\ '\.hg$',
		\ '\.svn$',
		\ '_darcs$',
		\ '\.bzr$',
		\ '\.cdv$',
		\ '\~\.dep$',
		\ '\~\.dot$',
		\ '\~\.nib$',
		\ '\~\.plst$',
		\ '\.pc$',
		\ '_MTN$',
		\ '<blib$',
		\ '<CVS$',
		\ '<RCS$',
		\ '<SCCS$',
		\ '_sgbak$',
		\ '<autom4te\.cache$',
		\ '<cover_db$',
		\ '_build$',
		\ ]
	let igfiles = [
		\ '\~$',
		\ '#.+#$',
		\ '[._].*\.swp$',
		\ 'core\.\d+$',
		\ '\.exe$',
		\ '\.so$',
		\ '\.bak$',
		\ '\.png$',
		\ '\.jpg$',
		\ '\.gif$',
		\ '\.zip$',
		\ '\.rar$',
		\ '\.tar\.gz$',
		\ ]
	retu {
		\ 'dir': '\v'.join(igdirs, '|'),
		\ 'file': '\v'.join(igfiles, '|'),
		\ }
endf "}}}2
" Options
let [s:pref, s:opts, s:new_opts] = ['g:ctrlp_', {
	\ 'arg_map':               ['s:argmap', 0],
	\ 'buffer_func':           ['s:buffunc', {}],
	\ 'by_filename':           ['s:byfname', 0],
	\ 'custom_ignore':         ['s:usrign', s:ignore()],
	\ 'default_input':         ['s:deftxt', 0],
	\ 'dont_split':            ['s:nosplit', 'netrw'],
	\ 'dotfiles':              ['s:dotfiles', 1],
	\ 'extensions':            ['s:extensions', []],
	\ 'follow_symlinks':       ['s:folsym', 0],
	\ 'highlight_match':       ['s:mathi', [1, 'CtrlPMatch']],
	\ 'jump_to_buffer':        ['s:jmptobuf', 2],
	\ 'lazy_update':           ['s:lazy', 0],
	\ 'match_func':            ['s:matcher', {}],
	\ 'match_window_bottom':   ['s:mwbottom', 1],
	\ 'match_window_reversed': ['s:mwreverse', 1],
	\ 'max_depth':             ['s:maxdepth', 40],
	\ 'max_files':             ['s:maxfiles', 10000],
	\ 'max_height':            ['s:mxheight', 10],
	\ 'max_history':           ['s:maxhst', exists('+hi') ? &hi : 20],
	\ 'mruf_default_order':    ['s:mrudef', 0],
	\ 'open_func':             ['s:openfunc', {}],
	\ 'open_multi':            ['s:opmul', '1v'],
	\ 'open_new_file':         ['s:newfop', 'v'],
	\ 'prompt_mappings':       ['s:urprtmaps', 0],
	\ 'regexp_search':         ['s:regexp', 0],
	\ 'root_markers':          ['s:rmarkers', []],
	\ 'split_window':          ['s:splitwin', 0],
	\ 'status_func':           ['s:status', {}],
	\ 'tabpage_position':      ['s:tabpage', 'ac'],
	\ 'use_caching':           ['s:caching', 1],
	\ 'use_migemo':            ['s:migemo', 0],
	\ 'user_command':          ['s:usrcmd', ''],
	\ 'working_path_mode':     ['s:pathmode', 'rc'],
	\ }, {
	\ 'open_multiple_files':   's:opmul',
	\ 'regexp':                's:regexp',
	\ 'reuse_window':          's:nosplit',
	\ 'switch_buffer':         's:jmptobuf',
	\ }]

" Global options
let s:glbs = { 'magic': 1, 'to': 1, 'tm': 0, 'sb': 1, 'hls': 0, 'im': 0,
	\ 'report': 9999, 'sc': 0, 'ss': 0, 'siso': 0, 'mfd': 200, 'mouse': 'n',
	\ 'gcr': 'a:blinkon0', 'ic': 1, 'lmap': '', 'mousef': 0, 'imd': 1 }

" Keymaps
let [s:lcmap, s:prtmaps] = ['nn <buffer> <silent>', {
	\ 'PrtBS()':              ['<bs>', '<c-]>'],
	\ 'PrtDelete()':          ['<del>'],
	\ 'PrtDeleteWord()':      ['<c-w>'],
	\ 'PrtClear()':           ['<c-u>'],
	\ 'PrtSelectMove("j")':   ['<c-j>', '<down>'],
	\ 'PrtSelectMove("k")':   ['<c-k>', '<up>'],
	\ 'PrtSelectMove("t")':   ['<Home>', '<kHome>'],
	\ 'PrtSelectMove("b")':   ['<End>', '<kEnd>'],
	\ 'PrtSelectMove("u")':   ['<PageUp>', '<kPageUp>'],
	\ 'PrtSelectMove("d")':   ['<PageDown>', '<kPageDown>'],
	\ 'PrtHistory(-1)':       ['<c-n>'],
	\ 'PrtHistory(1)':        ['<c-p>'],
	\ 'AcceptSelection("e")': ['<cr>', '<2-LeftMouse>'],
	\ 'AcceptSelection("h")': ['<c-x>', '<c-cr>', '<c-s>'],
	\ 'AcceptSelection("t")': ['<c-t>'],
	\ 'AcceptSelection("v")': ['<c-v>', '<RightMouse>'],
	\ 'ToggleFocus()':        ['<s-tab>'],
	\ 'ToggleRegex()':        ['<c-r>'],
	\ 'ToggleByFname()':      ['<c-d>'],
	\ 'ToggleType(1)':        ['<c-f>', '<c-up>'],
	\ 'ToggleType(-1)':       ['<c-b>', '<c-down>'],
	\ 'PrtExpandDir()':       ['<tab>'],
	\ 'PrtInsert("c")':       ['<MiddleMouse>', '<insert>'],
	\ 'PrtInsert()':          ['<c-\>'],
	\ 'PrtCurStart()':        ['<c-a>'],
	\ 'PrtCurEnd()':          ['<c-e>'],
	\ 'PrtCurLeft()':         ['<c-h>', '<left>', '<c-^>'],
	\ 'PrtCurRight()':        ['<c-l>', '<right>'],
	\ 'PrtClearCache()':      ['<F5>'],
	\ 'PrtDeleteEnt()':       ['<F7>'],
	\ 'CreateNewFile()':      ['<c-y>'],
	\ 'MarkToOpen()':         ['<c-z>'],
	\ 'OpenMulti()':          ['<c-o>'],
	\ 'PrtExit()':            ['<esc>', '<c-c>', '<c-g>'],
	\ }]

if !has('gui_running') && ( has('win32') || has('win64') )
	cal add(s:prtmaps['PrtBS()'], remove(s:prtmaps['PrtCurLeft()'], 0))
en

let s:lash = ctrlp#utils#lash()

let s:compare_lim = 3000

let s:ficounts = {}

" Regexp
let s:fpats = {
	\ '^\(\\|\)\|\(\\|\)$': '\\|',
	\ '^\\\(zs\|ze\|<\|>\)': '^\\\(zs\|ze\|<\|>\)',
	\ '^\S\*$': '\*',
	\ '^\S\\?$': '\\?',
	\ }

" Specials
let s:prtunmaps = [
	\ 'PrtBS()',
	\ 'PrtDelete()',
	\ 'PrtDeleteWord()',
	\ 'PrtClear()',
	\ 'PrtCurStart()',
	\ 'PrtCurEnd()',
	\ 'PrtCurLeft()',
	\ 'PrtCurRight()',
	\ 'PrtHistory(-1)',
	\ 'PrtHistory(1)',
	\ 'PrtInsert("c")',
	\ 'PrtInsert()',
	\ ]

" Keypad
let s:kprange = {
	\ 'Plus': '+',
	\ 'Minus': '-',
	\ 'Divide': '/',
	\ 'Multiply': '*',
	\ 'Point': '.',
	\ }

" Highlight groups
let s:hlgrps = {
	\ 'NoEntries': 'Error',
	\ 'Mode1': 'Character',
	\ 'Mode2': 'LineNr',
	\ 'Stats': 'Function',
	\ 'Match': 'Identifier',
	\ 'PrtBase': 'Comment',
	\ 'PrtText': 'Normal',
	\ 'PrtCursor': 'Constant',
	\ }

fu! s:opts() "{{{2
	" Options
	unl! s:usrign s:usrcmd s:urprtmaps
	for each in ['byfname', 'regexp', 'extensions'] | if exists('s:'.each)
		let {each} = s:{each}
	en | endfo
	for [ke, va] in items(s:opts)
		let {va[0]} = exists(s:pref.ke) ? {s:pref.ke} : va[1]
	endfo
	unl va
	for [ke, va] in items(s:new_opts)
		let {va} = {exists(s:pref.ke) ? s:pref.ke : va}
	endfo
	for each in ['byfname', 'regexp'] | if exists(each)
		let s:{each} = {each}
	en | endfo
	if !exists('g:ctrlp_newcache') | let g:ctrlp_newcache = 0 | en
	let s:maxdepth = min([s:maxdepth, 100])
	let s:mxheight = max([s:mxheight, 1])
	let s:glob = s:dotfiles ? '.*\|*' : '*'
	let s:igntype = empty(s:usrign) ? -1 : type(s:usrign)
	if s:lazy
		cal extend(s:glbs, { 'ut': ( s:lazy > 1 ? s:lazy : 250 ) })
	en
	" Extensions
	if !( exists('extensions') && extensions == s:extensions )
		for each in s:extensions
			exe 'ru autoload/ctrlp/'.each.'.vim'
		endfo
	en
	" Keymaps
	if type(s:urprtmaps) == 4
		cal extend(s:prtmaps, s:urprtmaps)
	en
endf
"}}}1
" * Open & Close {{{1
fu! s:Open()
	let s:ermsg = v:errmsg
	cal s:log(1)
	cal s:getenv()
	cal s:execextvar('enter')
	sil! exe 'keepa' ( s:mwbottom ? 'bo' : 'to' ) '1new ControlP'
	cal s:buffunc(1)
	let [s:bufnr, s:prompt, s:winw] = [bufnr('%'), ['', '', ''], winwidth(0)]
	abc <buffer>
	if !exists('s:hstry')
		let hst = filereadable(s:gethistloc()[1]) ? s:gethistdata() : ['']
		let s:hstry = empty(hst) || !s:maxhst ? [''] : hst
	en
	for [ke, va] in items(s:glbs) | if exists('+'.ke)
		sil! exe 'let s:glb_'.ke.' = &'.ke.' | let &'.ke.' = '.string(va)
	en | endfo
	if s:opmul != '0' && has('signs')
		sign define ctrlpmark text=+> texthl=Search
	en
	cal s:setupblank()
endf

fu! s:Close()
	cal s:buffunc(0)
	try | bun!
	cat | clo! | endt
	cal s:unmarksigns()
	for key in keys(s:glbs) | if exists('+'.key)
		sil! exe 'let &'.key.' = s:glb_'.key
	en | endfo
	if exists('s:glb_acd') | let &acd = s:glb_acd | en
	let g:ctrlp_lines = []
	if s:winres[1] >= &lines && s:winres[2] == winnr('$')
		exe s:winres[0]
	en
	unl! s:focus s:hisidx s:hstgot s:marked s:statypes s:cline s:init s:savestr
		\ s:mrbs
	cal ctrlp#recordhist()
	cal s:execextvar('exit')
	cal s:log(0)
	let v:errmsg = s:ermsg
	ec
endf
" * Clear caches {{{1
fu! ctrlp#clr(...)
	let [s:matches, g:ctrlp_new{ a:0 ? a:1 : 'cache' }] = [1, 1]
endf

fu! ctrlp#clra()
	let cache_dir = ctrlp#utils#cachedir()
	if isdirectory(cache_dir)
		let cache_files = split(s:glbpath(cache_dir, '**', 1), "\n")
		let eval = '!isdirectory(v:val) && fnamemodify(v:val, ":t") !~'
			\ . ' ''\v^<cache>[.a-z]+$|\.log$'''
		sil! cal map(filter(cache_files, eval), 'delete(v:val)')
	en
	cal ctrlp#clr()
endf

fu! ctrlp#reset()
	cal s:opts()
	cal s:autocmds()
	cal ctrlp#utils#opts()
	cal s:execextvar('opts')
endf
" * Files {{{1
fu! ctrlp#files()
	let cafile = ctrlp#utils#cachefile()
	if g:ctrlp_newcache || !filereadable(cafile) || s:nocache()
		let [lscmd, s:initcwd, g:ctrlp_allfiles] = [s:lsCmd(), s:dyncwd, []]
		" Get the list of files
		if empty(lscmd)
			cal s:GlobPath(s:dyncwd, 0)
		el
			sil! cal ctrlp#progress('Indexing...')
			try | cal s:UserCmd(lscmd)
			cat | retu [] | endt
		en
		" Remove base directory
		cal ctrlp#rmbasedir(g:ctrlp_allfiles)
		if len(g:ctrlp_allfiles) <= s:compare_lim
			cal sort(g:ctrlp_allfiles, 'ctrlp#complen')
		en
		cal s:writecache(cafile)
	el
		if !( exists('s:initcwd') && s:initcwd == s:dyncwd )
			let s:initcwd = s:dyncwd
			let g:ctrlp_allfiles = ctrlp#utils#readfile(cafile)
		en
	en
	cal extend(s:ficounts, { s:dyncwd : len(g:ctrlp_allfiles) })
	retu g:ctrlp_allfiles
endf

fu! s:GlobPath(dirs, depth)
	let entries = split(globpath(a:dirs, s:glob), "\n")
	let [dnf, depth] = [ctrlp#dirnfile(entries), a:depth + 1]
	cal extend(g:ctrlp_allfiles, dnf[1])
	if !empty(dnf[0]) && !s:maxf(len(g:ctrlp_allfiles)) && depth <= s:maxdepth
		sil! cal ctrlp#progress(len(g:ctrlp_allfiles), 1)
		cal s:GlobPath(join(dnf[0], ','), depth)
	en
endf

fu! s:UserCmd(lscmd)
	let [path, lscmd] = [s:dyncwd, a:lscmd]
	if exists('+ssl') && &ssl
		let [ssl, &ssl, path] = [&ssl, 0, tr(path, '/', '\')]
	en
	if has('win32') || has('win64')
		let lscmd = substitute(lscmd, '\v(^|&&\s*)\zscd (/d)@!', 'cd /d ', '')
	en
	let path = exists('*shellescape') ? shellescape(path) : path
	let g:ctrlp_allfiles = split(system(printf(lscmd, path)), "\n")
	if exists('+ssl') && exists('ssl')
		let &ssl = ssl
		cal map(g:ctrlp_allfiles, 'tr(v:val, "\\", "/")')
	en
	if exists('s:vcscmd') && s:vcscmd
		cal map(g:ctrlp_allfiles, 'tr(v:val, "/", "\\")')
	en
endf

fu! s:lsCmd()
	let cmd = s:usrcmd
	if type(cmd) == 1
		retu cmd
	elsei type(cmd) == 3 && len(cmd) >= 2 && cmd[:1] != ['', '']
		if s:findroot(s:dyncwd, cmd[0], 0, 1) == []
			retu len(cmd) == 3 ? cmd[2] : ''
		en
		let s:vcscmd = s:lash == '\' ? 1 : 0
		retu cmd[1]
	elsei type(cmd) == 4 && has_key(cmd, 'types')
		let [markrs, cmdtypes] = [[], values(cmd['types'])]
		for pair in cmdtypes
			cal add(markrs, pair[0])
		endfo
		let fndroot = s:findroot(s:dyncwd, markrs, 0, 1)
		if fndroot == []
			retu has_key(cmd, 'fallback') ? cmd['fallback'] : ''
		en
		for pair in cmdtypes
			if pair[0] == fndroot[0] | brea | en
		endfo
		let s:vcscmd = s:lash == '\' ? 1 : 0
		retu pair[1]
	en
endf
" - Buffers {{{1
fu! ctrlp#buffers(...)
	let ids = sort(filter(range(1, bufnr('$')), 'empty(getbufvar(v:val, "&bt"))'
		\ .' && getbufvar(v:val, "&bl") && strlen(bufname(v:val))'), 's:compmreb')
	retu a:0 && a:1 == 'id' ? ids : map(ids, 'fnamemodify(bufname(v:val), ":.")')
endf
" * MatchedItems() {{{1
fu! s:MatchIt(items, pat, limit, exc)
	let [lines, id] = [[], 0]
	let pat = s:byfname ?
		\ map(split(a:pat, '^[^;]\+\zs;', 1), 's:martcs.v:val') : s:martcs.a:pat
	for item in a:items
		let id += 1
		try | if !( s:ispath && item == a:exc ) && call(s:mfunc, [item, pat]) >= 0
			cal add(lines, item)
		en | cat | brea | endt
		if a:limit > 0 && len(lines) >= a:limit | brea | en
	endfo
	let s:mdata = [s:dyncwd, s:itemtype, s:regexp, s:sublist(a:items, id, -1)]
	retu lines
endf

fu! s:MatchedItems(items, pat, limit)
	let exc = exists('s:crfilerel') ? s:crfilerel : ''
	let items = s:narrowable() ? s:matched + s:mdata[3] : a:items
	if s:matcher != {}
		let argms = [items, a:pat, a:limit, s:mmode(), s:ispath, exc, s:regexp]
		let lines = call(s:matcher['match'], argms)
	el
		let lines = s:MatchIt(items, a:pat, a:limit, exc)
	en
	let s:matches = len(lines)
	retu lines
endf

fu! s:SplitPattern(str)
	let str = a:str
	if s:migemo && s:regexp && len(str) > 0 && executable('cmigemo')
		let str = s:migemo(str)
	en
	let s:savestr = str
	if s:regexp
		let pat = s:regexfilter(str)
	el
		let lst = split(str, '\zs')
		if exists('+ssl') && !&ssl
			cal map(lst, 'escape(v:val, ''\'')')
		en
		for each in ['^', '$', '.']
			cal map(lst, 'escape(v:val, each)')
		endfo
	en
	if exists('lst')
		let pat = ''
		if !empty(lst)
			if s:byfname && index(lst, ';') > 0
				let fbar = index(lst, ';')
				let lst_1 = s:sublist(lst, 0, fbar - 1)
				let lst_2 = len(lst) - 1 > fbar ? s:sublist(lst, fbar + 1, -1) : ['']
				let pat = s:buildpat(lst_1).';'.s:buildpat(lst_2)
			el
				let pat = s:buildpat(lst)
			en
		en
	en
	retu escape(pat, '~')
endf
" * BuildPrompt() {{{1
fu! s:Render(lines, pat)
	let [&ma, lines, s:height] = [1, a:lines, min([len(a:lines), s:winh])]
	let pat = s:byfname ? split(a:pat, '^[^;]\+\zs;', 1)[0] : a:pat
	" Setup the match window
	sil! exe '%d _ | res' s:height
	" Print the new items
	if empty(lines)
		let [s:matched, s:lines] = [[], []]
		cal setline(1, ' == NO ENTRIES ==')
		setl noma nocul
		cal s:unmarksigns()
		if s:dohighlight() | cal clearmatches() | en
		retu
	en
	let s:matched = copy(lines)
	" Sorting
	if !s:nosort()
		let s:compat = s:martcs.pat
		cal sort(lines, 's:mixedsort')
		unl s:compat
	en
	if s:mwreverse | cal reverse(lines) | en
	let s:lines = copy(lines)
	cal map(lines, 's:formatline(v:val)')
	cal setline(1, lines)
	setl noma cul
	exe 'keepj norm!' ( s:mwreverse ? 'G' : 'gg' ).'1|'
	cal s:unmarksigns()
	cal s:remarksigns()
	if exists('s:cline') && s:nolim != 1
		cal cursor(s:cline, 1)
	en
	" Highlighting
	if s:dohighlight()
		cal s:highlight(pat, s:mathi[1])
	en
endf

fu! s:Update(str)
	" Get the previous string if existed
	let oldstr = exists('s:savestr') ? s:savestr : ''
	" Get the new string sans tail
	let str = s:sanstail(a:str)
	" Stop if the string's unchanged
	if str == oldstr && !empty(str) && !exists('s:force') | retu | en
	let s:martcs = &scs && str =~ '\u' ? '\C' : ''
	let pat = s:matcher == {} ? s:SplitPattern(str) : str
	let lines = s:nolim == 1 && empty(str) ? copy(g:ctrlp_lines)
		\ : s:MatchedItems(g:ctrlp_lines, pat, s:winh)
	cal s:Render(lines, pat)
endf

fu! s:ForceUpdate()
	let [estr, prt] = ['"\', copy(s:prompt)]
	cal map(prt, 'escape(v:val, estr)')
	sil! cal s:Update(join(prt, ''))
endf

fu! s:BuildPrompt(upd, ...)
	let base = ( s:regexp ? 'r' : '>' ).( s:byfname ? 'd' : '>' ).'> '
	let [estr, prt] = ['"\', copy(s:prompt)]
	cal map(prt, 'escape(v:val, estr)')
	let str = join(prt, '')
	let lazy = empty(str) || exists('s:force') || !has('autocmd') ? 0 : s:lazy
	if a:upd && !lazy && ( s:matches || s:regexp
		\ || str =~ '\(\\\(<\|>\)\|[*|]\)\|\(\\\:\([^:]\|\\:\)*$\)' )
		sil! cal s:Update(str)
	en
	sil! cal ctrlp#statusline()
	" Toggling
	let [hiactive, hicursor, base] = a:0 && !a:1
		\ ? ['CtrlPPrtBase', 'CtrlPPrtBase', tr(base, '>', '-')]
		\ : ['CtrlPPrtText', 'CtrlPPrtCursor', base]
	let hibase = 'CtrlPPrtBase'
	" Build it
	redr
	exe 'echoh' hibase '| echon "'.base.'"
		\ | echoh' hiactive '| echon "'.prt[0].'"
		\ | echoh' hicursor '| echon "'.prt[1].'"
		\ | echoh' hiactive '| echon "'.prt[2].'" | echoh None'
	" Append the cursor at the end
	if empty(prt[1]) && !( a:0 && !a:1 )
		exe 'echoh' hibase '| echon "_" | echoh None'
	en
endf
" - SetDefTxt() {{{1
fu! s:SetDefTxt()
	if s:deftxt == '0' || ( s:deftxt == 1 && !s:ispath ) | retu | en
	let txt = s:deftxt
	if !type(txt)
		let txt = txt && !stridx(s:crfpath, s:dyncwd)
			\ ? ctrlp#rmbasedir([s:crfpath])[0] : ''
		let txt = txt != '' ? txt.s:lash(s:crfpath) : ''
	el
		let txt = expand(txt, 1)
	en
	let s:prompt[0] = txt
endf
" ** Prt Actions {{{1
" Editing {{{2
fu! s:PrtClear()
	unl! s:hstgot
	let [s:prompt, s:matches] = [['', '', ''], 1]
	cal s:BuildPrompt(1)
endf

fu! s:PrtAdd(char)
	unl! s:hstgot
	let s:act_add = 1
	let s:prompt[0] .= a:char
	cal s:BuildPrompt(1)
	unl s:act_add
endf

fu! s:PrtBS()
	unl! s:hstgot
	let [s:prompt[0], s:matches] = [substitute(s:prompt[0], '.$', '', ''), 1]
	cal s:BuildPrompt(1)
endf

fu! s:PrtDelete()
	unl! s:hstgot
	let [prt, s:matches] = [s:prompt, 1]
	let prt[1] = matchstr(prt[2], '^.')
	let prt[2] = substitute(prt[2], '^.', '', '')
	cal s:BuildPrompt(1)
endf

fu! s:PrtDeleteWord()
	unl! s:hstgot
	let [str, s:matches] = [s:prompt[0], 1]
	let str = str =~ '\W\w\+$' ? matchstr(str, '^.\+\W\ze\w\+$')
		\ : str =~ '\w\W\+$' ? matchstr(str, '^.\+\w\ze\W\+$')
		\ : str =~ '\s\+$' ? matchstr(str, '^.*\S\ze\s\+$')
		\ : str =~ '\v^(\S+|\s+)$' ? '' : str
	let s:prompt[0] = str
	cal s:BuildPrompt(1)
endf

fu! s:PrtInsert(...)
	let type = !a:0 ? '' : a:1
	if !a:0
		let type = s:insertstr()
		if type == 'cancel' | retu | en
	en
	if type ==# 'r'
		let regcont = s:getregs()
		if regcont < 0 | retu | en
	en
	unl! s:hstgot
	let s:act_add = 1
	let s:prompt[0] .= type ==# 'w' ? s:crword
		\ : type ==# 'f' ? s:crgfile
		\ : type ==# 's' ? s:regisfilter('/')
		\ : type ==# 'v' ? s:crvisual
		\ : type ==# 'c' ? s:regisfilter('+')
		\ : type ==# 'r' ? regcont : ''
	cal s:BuildPrompt(1)
	unl s:act_add
endf

fu! s:PrtExpandDir()
	let str = s:prompt[0]
	if str =~ '\v^\@(cd|lc[hd]?|chd)\s.+' && s:spi
		let hasat = split(str, '\v^\@(cd|lc[hd]?|chd)\s*\zs')
		let str = get(hasat, 1, '')
	en
	if str == '' | retu | en
	unl! s:hstgot
	let s:act_add = 1
	let [base, seed] = s:headntail(str)
	let dirs = s:dircompl(base, seed)
	if len(dirs) == 1
		let str = dirs[0]
	elsei len(dirs) > 1
		let str .= s:findcommon(dirs, str)
	en
	let s:prompt[0] = exists('hasat') ? hasat[0].str : str
	cal s:BuildPrompt(1)
	unl s:act_add
endf
" Movement {{{2
fu! s:PrtCurLeft()
	let prt = s:prompt
	if !empty(prt[0])
		let s:prompt = [substitute(prt[0], '.$', '', ''), matchstr(prt[0], '.$'),
			\ prt[1] . prt[2]]
	en
	cal s:BuildPrompt(0)
endf

fu! s:PrtCurRight()
	let prt = s:prompt
	let s:prompt = [prt[0] . prt[1], matchstr(prt[2], '^.'),
		\ substitute(prt[2], '^.', '', '')]
	cal s:BuildPrompt(0)
endf

fu! s:PrtCurStart()
	let str = join(s:prompt, '')
	let s:prompt = ['', matchstr(str, '^.'), substitute(str, '^.', '', '')]
	cal s:BuildPrompt(0)
endf

fu! s:PrtCurEnd()
	let s:prompt = [join(s:prompt, ''), '', '']
	cal s:BuildPrompt(0)
endf

fu! s:PrtSelectMove(dir)
	let wht = winheight(0)
	let dirs = {'t': 'gg','b': 'G','j': 'j','k': 'k','u': wht.'k','d': wht.'j'}
	exe 'keepj norm!' dirs[a:dir]
	if s:nolim != 1 | let s:cline = line('.') | en
	if line('$') > winheight(0) | cal s:BuildPrompt(0, s:Focus()) | en
endf

fu! s:PrtSelectJump(char, ...)
	let lines = copy(s:lines)
	if a:0
		cal map(lines, 'split(v:val, ''[\/]\ze[^\/]\+$'')[-1]')
	en
	" Cycle through matches, use s:jmpchr to store last jump
	let chr = escape(a:char, '.~')
	if match(lines, '\c^'.chr) >= 0
		" If not exists or does but not for the same char
		let pos = match(lines, '\c^'.chr)
		if !exists('s:jmpchr') || ( exists('s:jmpchr') && s:jmpchr[0] != chr )
			let [jmpln, s:jmpchr] = [pos, [chr, pos]]
		elsei exists('s:jmpchr') && s:jmpchr[0] == chr
			" Start of lines
			if s:jmpchr[1] == -1 | let s:jmpchr[1] = pos | en
			let npos = match(lines, '\c^'.chr, s:jmpchr[1] + 1)
			let [jmpln, s:jmpchr] = [npos == -1 ? pos : npos, [chr, npos]]
		en
		keepj exe jmpln + 1
		if s:nolim != 1 | let s:cline = line('.') | en
		if line('$') > winheight(0) | cal s:BuildPrompt(0, s:Focus()) | en
	en
endf
" Misc {{{2
fu! s:PrtClearCache()
	if s:itemtype == 0
		cal ctrlp#clr()
	elsei s:itemtype > 2
		cal ctrlp#clr(s:statypes[s:itemtype][1])
	en
	if s:itemtype == 2
		let g:ctrlp_lines = ctrlp#mrufiles#refresh()
	el
		cal ctrlp#setlines()
	en
	let s:force = 1
	cal s:BuildPrompt(1)
	unl s:force
endf

fu! s:PrtDeleteEnt()
	if s:itemtype == 2
		cal s:PrtDeleteMRU()
	elsei type(s:getextvar('wipe')) == 1
		cal s:delent(s:getextvar('wipe'))
	en
endf

fu! s:PrtDeleteMRU()
	if s:itemtype == 2
		cal s:delent('ctrlp#mrufiles#remove')
	en
endf

fu! s:PrtExit()
	if !has('autocmd') | cal s:Close() | en
	exe ( winnr('$') == 1 ? 'bw!' : 'winc p' )
endf

fu! s:PrtHistory(...)
	if !s:maxhst | retu | en
	let [str, hst, s:matches] = [join(s:prompt, ''), s:hstry, 1]
	" Save to history if not saved before
	let [hst[0], hslen] = [exists('s:hstgot') ? hst[0] : str, len(hst)]
	let idx = exists('s:hisidx') ? s:hisidx + a:1 : a:1
	" Limit idx within 0 and hslen
	let idx = idx < 0 ? 0 : idx >= hslen ? hslen > 1 ? hslen - 1 : 0 : idx
	let s:prompt = [hst[idx], '', '']
	let [s:hisidx, s:hstgot, s:force] = [idx, 1, 1]
	cal s:BuildPrompt(1)
	unl s:force
endf
"}}}1
" * MapKeys() {{{1
fu! s:MapKeys(...)
	" Normal keys
	let pfunc = a:0 && !a:1 ? 'PrtSelectJump' : 'PrtAdd'
	let dojmp = s:byfname && a:0 && !a:1 ? ', 1' : ''
	let pcmd = "nn \<buffer> \<silent> \<k%s> :\<c-u>cal \<SID>%s(\"%s\"%s)\<cr>"
	let cmd = substitute(pcmd, 'k%s', 'char-%d', '')
	for each in range(32, 126)
		exe printf(cmd, each, pfunc, escape(nr2char(each), '"|\'), dojmp)
	endfo
	for each in range(0, 9)
		exe printf(pcmd, each, pfunc, each, dojmp)
	endfo
	for [ke, va] in items(s:kprange)
		exe printf(pcmd, ke, pfunc, va, dojmp)
	endfo
	" Special keys
	if a:0 < 2
		cal call('s:MapSpecs', a:0 && !a:1 ? [1] : [])
	en
endf

fu! s:MapSpecs(...)
	" Correct arrow keys in terminal
	if ( has('termresponse') && v:termresponse =~ "\<ESC>" )
		\ || &term =~? '\vxterm|<k?vt|gnome|screen|linux|ansi'
		for each in ['\A <up>','\B <down>','\C <right>','\D <left>']
			exe s:lcmap.' <esc>['.each
		endfo
	en
	if a:0
		for ke in s:prtunmaps | for kp in s:prtmaps[ke]
			exe s:lcmap kp '<Nop>'
		endfo | endfo
	el
		for [ke, va] in items(s:prtmaps) | for kp in va
			exe s:lcmap kp ':<c-u>cal <SID>'.ke.'<cr>'
		endfo | endfo
	en
endf
" * Toggling {{{1
fu! s:Focus()
	retu !exists('s:focus') ? 1 : s:focus
endf

fu! s:ToggleFocus()
	let s:focus = !exists('s:focus') || s:focus ? 0 : 1
	cal s:MapKeys(s:focus)
	cal s:BuildPrompt(0, s:focus)
endf

fu! s:ToggleRegex()
	let s:regexp = s:regexp ? 0 : 1
	cal s:PrtSwitcher()
endf

fu! s:ToggleByFname()
	if s:ispath
		let s:byfname = s:byfname ? 0 : 1
		let s:mfunc = s:mfunc()
		cal s:MapKeys(s:Focus(), 1)
		cal s:PrtSwitcher()
	en
endf

fu! s:ToggleType(dir)
	let max = len(g:ctrlp_ext_vars) + 2
	let next = s:walker(max, s:itemtype, a:dir)
	cal ctrlp#syntax()
	cal ctrlp#setlines(next)
	cal s:PrtSwitcher()
endf

fu! s:PrtSwitcher()
	let [s:force, s:matches] = [1, 1]
	cal s:BuildPrompt(1, s:Focus())
	unl s:force
endf
" - SetWD() {{{1
fu! s:SetWD(args)
	let [s:crfilerel, s:dyncwd] = [fnamemodify(s:crfile, ':.'), getcwd()]
	let pmode = has_key(a:args, 'mode') ? a:args['mode'] : s:wpmode
	if has_key(a:args, 'dir') && a:args['dir'] != ''
		cal ctrlp#setdir(a:args['dir']) | retu
	en
	if s:crfile =~ '^.\+://' | retu | en
	if pmode =~ 'c' || ( !type(pmode) && pmode )
		if exists('+acd') | let [s:glb_acd, &acd] = [&acd, 0] | en
		cal ctrlp#setdir(s:crfpath)
	en
	if pmode =~ 'r' || pmode == 2
		let markers = ['.git', '.hg', '.svn', '.bzr', '_darcs']
		let spath = pmode =~ 'd' ? s:dyncwd : pmode =~ 'w' ? s:cwd : s:crfpath
		if type(s:rmarkers) == 3 && !empty(s:rmarkers)
			if s:findroot(spath, s:rmarkers, 0, 0) != [] | retu | en
			cal filter(markers, 'index(s:rmarkers, v:val) < 0')
		en
		cal s:findroot(spath, markers, 0, 0)
	en
endf
" * AcceptSelection() {{{1
fu! ctrlp#acceptfile(mode, line, ...)
	let [md, filpath] = [a:mode, fnamemodify(a:line, ':p')]
	cal s:PrtExit()
	let [bufnr, tail] = [bufnr('^'.filpath.'$'), s:tail()]
	let j2l = a:0 ? a:1 : str2nr(matchstr(tail, '^ +\D*\zs\d\+\ze\D*'))
	if s:jmptobuf && bufnr > 0 && md =~ 'e\|t'
		\ && !( md == 'e' && bufnr == bufnr('%') )
		let [jmpb, bufwinnr] = [1, bufwinnr(bufnr)]
		let buftab = s:jmptobuf > 1 ? s:buftab(bufnr, md) : [0, 0]
	en
	" Switch to existing buffer or open new one
	if exists('jmpb') && bufwinnr > 0 && md != 't'
		exe bufwinnr.'winc w'
		if j2l | cal ctrlp#j2l(j2l) | en
	elsei exists('jmpb') && buftab[0]
		exe 'tabn' buftab[0]
		exe buftab[1].'winc w'
		if j2l | cal ctrlp#j2l(j2l) | en
	el
		" Determine the command to use
		let useb = bufnr > 0 && buflisted(bufnr) && empty(tail)
		let cmd =
			\ md == 't' || s:splitwin == 1 ? ( useb ? 'tab sb' : 'tabe' ) :
			\ md == 'h' || s:splitwin == 2 ? ( useb ? 'sb' : 'new' ) :
			\ md == 'v' || s:splitwin == 3 ? ( useb ? 'vert sb' : 'vne' ) :
			\ call('ctrlp#normcmd', useb ? ['b', 'bo vert sb'] : ['e'])
		" Reset &switchbuf option
		let [swb, &swb] = [&swb, '']
		" Open new window/buffer
		let args = [cmd, useb ? bufnr : filpath, a:0 ? ' +'.a:1 : tail, useb, j2l]
		cal call('s:openfile', args)
		let &swb = swb
	en
endf

fu! s:SpecInputs(str)
	if a:str =~ '\v^(\.\.([\/]\.\.)*[\/]?[.\/]*)$' && s:spi
		let cwd = s:dyncwd
		cal ctrlp#setdir(a:str =~ '^\.\.\.*$' ?
			\ '../'.repeat('../', strlen(a:str) - 2) : a:str)
		if cwd != s:dyncwd | cal ctrlp#setlines() | en
		cal s:PrtClear()
		retu 1
	elsei a:str == s:lash && s:spi
		cal s:SetWD({ 'mode': 'rd' })
		cal ctrlp#setlines()
		cal s:PrtClear()
		retu 1
	elsei a:str =~ '^@.\+' && s:spi
		retu s:at(a:str)
	elsei a:str == '?'
		cal s:PrtExit()
		let hlpwin = &columns > 159 ? '| vert res 80' : ''
		sil! exe 'bo vert h ctrlp-mappings' hlpwin '| norm! 0'
		retu 1
	en
	retu 0
endf

fu! s:AcceptSelection(mode)
	if a:mode != 'e' && s:OpenMulti(a:mode) != -1 | retu | en
	let str = join(s:prompt, '')
	if a:mode == 'e' | if s:SpecInputs(str) | retu | en | en
	" Get the selected line
	let line = !empty(s:lines) ? s:lines[line('.') - 1] : ''
	if a:mode != 'e' && !s:itemtype && line == ''
		\ && str !~ '\v^(\.\.([\/]\.\.)*[\/]?[.\/]*|/|\\|\?|\@.+)$'
		cal s:CreateNewFile(a:mode) | retu
	en
	if empty(line) | retu | en
	" Do something with it
	if s:openfunc != {} && has_key(s:openfunc, s:ctype)
		let actfunc = s:openfunc[s:ctype]
	el
		let actfunc = s:itemtype < 3 ? 'ctrlp#acceptfile' : s:getextvar('accept')
	en
	cal call(actfunc, [a:mode, line])
endf
" - CreateNewFile() {{{1
fu! s:CreateNewFile(...)
	let [md, str] = ['', join(s:prompt, '')]
	if empty(str) | retu | en
	if s:argmap && !a:0
		" Get the extra argument
		let md = s:argmaps(md, 1)
		if md == 'cancel' | retu | en
	en
	let str = s:sanstail(str)
	let [base, fname] = s:headntail(str)
	if fname =~ '^[\/]$' | retu | en
	if exists('s:marked') && len(s:marked)
		" Use the first marked file's path
		let path = fnamemodify(values(s:marked)[0], ':p:h')
		let base = path.s:lash(path).base
		let str = fnamemodify(base.s:lash.fname, ':.')
	en
	if base != '' | if isdirectory(ctrlp#utils#mkdir(base))
		let optyp = str | en | el | let optyp = fname
	en
	if !exists('optyp') | retu | en
	let [filpath, tail] = [fnamemodify(optyp, ':p'), s:tail()]
	if !stridx(filpath, s:dyncwd) | cal s:insertcache(str) | en
	cal s:PrtExit()
	let cmd = md == 'r' ? ctrlp#normcmd('e') :
		\ s:newfop =~ '1\|t' || ( a:0 && a:1 == 't' ) || md == 't' ? 'tabe' :
		\ s:newfop =~ '2\|h' || ( a:0 && a:1 == 'h' ) || md == 'h' ? 'new' :
		\ s:newfop =~ '3\|v' || ( a:0 && a:1 == 'v' ) || md == 'v' ? 'vne' :
		\ ctrlp#normcmd('e')
	cal s:openfile(cmd, filpath, tail)
endf
" * OpenMulti() {{{1
fu! s:MarkToOpen()
	if s:bufnr <= 0 || s:opmul == '0'
		\ || ( s:itemtype > 2 && s:getextvar('opmul') != 1 )
		retu
	en
	let line = !empty(s:lines) ? s:lines[line('.') - 1] : ''
	if empty(line) | retu | en
	let filpath = s:ispath ? fnamemodify(line, ':p') : line
	if exists('s:marked') && s:dictindex(s:marked, filpath) > 0
		" Unmark and remove the file from s:marked
		let key = s:dictindex(s:marked, filpath)
		cal remove(s:marked, key)
		if empty(s:marked) | unl s:marked | en
		if has('signs')
			exe 'sign unplace' key 'buffer='.s:bufnr
		en
	el
		" Add to s:marked and place a new sign
		if exists('s:marked')
			let vac = s:vacantdict(s:marked)
			let key = empty(vac) ? len(s:marked) + 1 : vac[0]
			let s:marked = extend(s:marked, { key : filpath })
		el
			let [key, s:marked] = [1, { 1 : filpath }]
		en
		if has('signs')
			exe 'sign place' key 'line='.line('.').' name=ctrlpmark buffer='.s:bufnr
		en
	en
	sil! cal ctrlp#statusline()
endf

fu! s:OpenMulti(...)
	let has_marked = exists('s:marked')
	if ( !has_marked && a:0 ) || s:opmul == '0' || !s:ispath
		\ || ( s:itemtype > 2 && s:getextvar('opmul') != 1 )
		retu -1
	en
	" Get the options
	let [nr, md] = [matchstr(s:opmul, '\d\+'), matchstr(s:opmul, '[thvi]')]
	let [ur, jf] = [s:opmul =~ 'r', s:opmul =~ 'j']
	let md = a:0 ? a:1 : ( md == '' ? 'v' : md )
	let nopt = exists('g:ctrlp_open_multiple_files')
	if !has_marked
		let line = !empty(s:lines) ? s:lines[line('.') - 1] : ''
		if line == '' | retu | en
		let marked = { 1 : fnamemodify(line, ':p') }
		let [nr, ur, jf, nopt] = ['1', 0, 0, 1]
	en
	if ( s:argmap || !has_marked ) && !a:0
		let md = s:argmaps(md, !has_marked ? 2 : 0)
		if md == 'c'
			cal s:unmarksigns()
			unl! s:marked
			cal s:BuildPrompt(0)
		elsei !has_marked && md == 'a'
			let [s:marked, key] = [{}, 1]
			for line in s:lines
				let s:marked = extend(s:marked, { key : fnamemodify(line, ':p') })
				let key += 1
			endfo
			cal s:remarksigns()
			retu s:BuildPrompt(0)
		elsei !has_marked && md == 'x'
			retu call(s:openfunc[s:ctype], [md, line])
		en
		if md =~ '\v^c(ancel)?$' | retu | en
		let nr = nr == '0' ? ( nopt ? '' : '1' ) : nr
		let ur = !has_marked && md == 'r' ? 1 : ur
	en
	let mkd = values(has_marked ? s:marked : marked)
	cal s:sanstail(join(s:prompt, ''))
	cal s:PrtExit()
	if nr == '0' || md == 'i'
		retu map(mkd, "s:openfile('bad', fnamemodify(v:val, ':.'), '')")
	en
	let [tail, fnesc] = [s:tail(), exists('*fnameescape') && v:version > 701]
	let [emptytail, bufnr] = [empty(tail), bufnr('^'.mkd[0].'$')]
	let useb = bufnr > 0 && buflisted(bufnr) && emptytail
	" Move to a replaceable window
	let ncmd = ( useb ? ['b', 'bo vert sb'] : ['e', 'bo vne'] )
		\ + ( ur ? [] : ['ignruw'] )
	let fst = call('ctrlp#normcmd', ncmd)
	" Check if the current window has a replaceable buffer
	let repabl = !( md == 't' && !ur ) && empty(bufname('%')) && empty(&l:ft)
	" Commands for the rest of the files
	let [ic, cmds] = [1, { 'v': ['vert sb', 'vne'], 'h': ['sb', 'new'],
		\ 't': ['tab sb', 'tabe'] }]
	let [swb, &swb] = [&swb, '']
	if md == 't' && ctrlp#tabcount() < tabpagenr()
		let s:tabct = ctrlp#tabcount()
	en
	" Open the files
	for va in mkd
		let bufnr = bufnr('^'.va.'$')
		if bufnr < 0 && getftype(va) == '' | con | en
		let useb = bufnr > 0 && buflisted(bufnr) && emptytail
		let snd = md != '' && has_key(cmds, md) ?
			\ ( useb ? cmds[md][0] : cmds[md][1] ) : ( useb ? 'vert sb' : 'vne' )
		let cmd = ic == 1 && ( !( !ur && fst =~ '^[eb]$' ) || repabl ) ? fst : snd
		let conds = [( nr != '' && nr > 1 && nr < ic ) || ( nr == '' && ic > 1 ),
			\ nr != '' && nr < ic]
		if conds[nopt]
			if bufnr <= 0 | if fnesc
				cal s:openfile('bad', fnamemodify(va, ':.'), '')
			el
				cal s:openfile(cmd, va, tail) | sil! hid clo!
			en | en
		el
			cal s:openfile(cmd, useb ? bufnr : va, tail) | let ic += 1
			if jf | if ic == 2
				let crpos = [tabpagenr(), winnr()]
			el
				let crpos[0] += tabpagenr() <= crpos[0]
				let crpos[1] += winnr() == crpos[1]
			en | en
		en
	endfo
	if jf && exists('crpos') && ic > 2
		exe ( md == 't' ? 'tabn '.crpos[0] : crpos[1].'winc w' )
	en
	let &swb = swb
	unl! s:tabct
endf
" ** Helper functions {{{1
" Sorting {{{2
fu! ctrlp#complen(...)
	" By length
	let [len1, len2] = [strlen(a:1), strlen(a:2)]
	retu len1 == len2 ? 0 : len1 > len2 ? 1 : -1
endf

fu! s:compmatlen(...)
	" By match length
	let mln1 = s:shortest(s:matchlens(a:1, s:compat))
	let mln2 = s:shortest(s:matchlens(a:2, s:compat))
	retu mln1 == mln2 ? 0 : mln1 > mln2 ? 1 : -1
endf

fu! s:comptime(...)
	" By last modified time
	let [time1, time2] = [getftime(a:1), getftime(a:2)]
	retu time1 == time2 ? 0 : time1 < time2 ? 1 : -1
endf

fu! s:compmreb(...)
	" By last entered time (bufnr)
	let [id1, id2] = [index(s:mrbs, a:1), index(s:mrbs, a:2)]
	retu id1 == id2 ? 0 : id1 > id2 ? 1 : -1
endf

fu! s:compmref(...)
	" By last entered time (MRU)
	let [id1, id2] = [index(g:ctrlp_lines, a:1), index(g:ctrlp_lines, a:2)]
	retu id1 == id2 ? 0 : id1 > id2 ? 1 : -1
endf

fu! s:comparent(...)
	" By same parent dir
	if !stridx(s:crfpath, s:dyncwd)
		let [as1, as2] = [s:dyncwd.s:lash().a:1, s:dyncwd.s:lash().a:2]
		let [loc1, loc2] = [s:getparent(as1), s:getparent(as2)]
		if loc1 == s:crfpath && loc2 != s:crfpath | retu -1 | en
		if loc2 == s:crfpath && loc1 != s:crfpath | retu 1  | en
		retu 0
	en
	retu 0
endf

fu! s:compfnlen(...)
	" By filename length
	let len1 = strlen(split(a:1, s:lash)[-1])
	let len2 = strlen(split(a:2, s:lash)[-1])
	retu len1 == len2 ? 0 : len1 > len2 ? 1 : -1
endf

fu! s:matchlens(str, pat, ...)
	if empty(a:pat) || index(['^', '$'], a:pat) >= 0 | retu {} | en
	let st   = a:0 ? a:1 : 0
	let lens = a:0 >= 2 ? a:2 : {}
	let nr   = a:0 >= 3 ? a:3 : 0
	if nr > 20 | retu {} | en
	if match(a:str, a:pat, st) >= 0
		let [mst, mnd] = [matchstr(a:str, a:pat, st), matchend(a:str, a:pat, st)]
		let lens = extend(lens, { nr : [strlen(mst), mst] })
		let lens = s:matchlens(a:str, a:pat, mnd, lens, nr + 1)
	en
	retu lens
endf

fu! s:shortest(lens)
	retu min(map(values(a:lens), 'v:val[0]'))
endf

fu! s:mixedsort(...)
	let [cln, cml] = [ctrlp#complen(a:1, a:2), s:compmatlen(a:1, a:2)]
	if s:ispath
		let ms = []
		if s:height < 21
			let ms += [s:compfnlen(a:1, a:2)]
			if s:itemtype !~ '^[12]$' | let ms += [s:comptime(a:1, a:2)] | en
			if !s:itemtype | let ms += [s:comparent(a:1, a:2)] | en
		en
		if s:itemtype =~ '^[12]$'
			let ms += [s:compmref(a:1, a:2)]
			let cln = cml ? cln : 0
		en
		let ms += [cml, 0, 0, 0]
		let mp = call('s:multipliers', ms[:3])
		retu cln + ms[0] * mp[0] + ms[1] * mp[1] + ms[2] * mp[2] + ms[3] * mp[3]
	en
	retu cln + cml * 2
endf

fu! s:multipliers(...)
	let mp0 = !a:1 ? 0 : 2
	let mp1 = !a:2 ? 0 : 1 + ( !mp0 ? 1 : mp0 )
	let mp2 = !a:3 ? 0 : 1 + ( !( mp0 + mp1 ) ? 1 : ( mp0 + mp1 ) )
	let mp3 = !a:4 ? 0 : 1 + ( !( mp0 + mp1 + mp2 ) ? 1 : ( mp0 + mp1 + mp2 ) )
	retu [mp0, mp1, mp2, mp3]
endf

fu! s:compval(...)
	retu a:1 - a:2
endf
" Statusline {{{2
fu! ctrlp#statusline()
	if !exists('s:statypes')
		let s:statypes = [
			\ ['files', 'fil'],
			\ ['buffers', 'buf'],
			\ ['mru files', 'mru'],
			\ ]
		if !empty(g:ctrlp_ext_vars)
			cal map(copy(g:ctrlp_ext_vars),
				\ 'add(s:statypes, [ v:val["lname"], v:val["sname"] ])')
		en
	en
	let tps = s:statypes
	let max = len(tps) - 1
	let nxt = tps[s:walker(max, s:itemtype,  1)][1]
	let prv = tps[s:walker(max, s:itemtype, -1)][1]
	let s:ctype = tps[s:itemtype][0]
	let focus   = s:Focus() ? 'prt'  : 'win'
	let byfname = s:byfname ? 'file' : 'path'
	let marked  = s:opmul != '0' ?
		\ exists('s:marked') ? ' <'.s:dismrk().'>' : ' <->' : ''
	if s:status != {}
		let args = [focus, byfname, s:regexp, prv, s:ctype, nxt, marked]
		let &l:stl = call(s:status['main'], args)
	el
		let item    = '%#CtrlPMode1# '.s:ctype.' %*'
		let focus   = '%#CtrlPMode2# '.focus.' %*'
		let byfname = '%#CtrlPMode1# '.byfname.' %*'
		let regex   = s:regexp  ? '%#CtrlPMode2# regex %*' : ''
		let slider  = ' <'.prv.'>={'.item.'}=<'.nxt.'>'
		let dir     = ' %=%<%#CtrlPMode2# %{getcwd()} %*'
		let &l:stl  = focus.byfname.regex.slider.marked.dir
	en
endf

fu! s:dismrk()
	retu has('signs') ? len(s:marked) :
		\ '%<'.join(values(map(copy(s:marked), 'split(v:val, "[\\/]")[-1]')), ', ')
endf

fu! ctrlp#progress(enum, ...)
	if has('macunix') || has('mac') | sl 1m | en
	let txt = a:0 ? '(press ctrl-c to abort)' : ''
	let &l:stl = s:status != {} ? call(s:status['prog'], [a:enum])
		\ : '%#CtrlPStats# '.a:enum.' %* '.txt.'%=%<%#CtrlPMode2# %{getcwd()} %*'
	redraws
endf
" *** Paths {{{2
" Line formatting {{{3
fu! s:formatline(str)
	let cond = s:ispath && ( s:winw - 4 ) < s:strwidth(a:str)
	retu '> '.( cond ? s:pathshorten(a:str) : a:str )
endf

fu! s:pathshorten(str)
	retu matchstr(a:str, '^.\{9}').'...'
		\ .matchstr(a:str, '.\{'.( s:winw - 16 ).'}$')
endf
" Directory completion {{{3
fu! s:dircompl(be, sd)
	if a:sd == '' | retu [] | en
	let [be, sd] = a:be == '' ? [s:dyncwd, a:sd] : [a:be, a:be.s:lash(a:be).a:sd]
	let dirs = ctrlp#rmbasedir(split(globpath(be, a:sd.'*/'), "\n"))
	cal filter(dirs, '!match(v:val, escape(sd, ''~$.\''))'
		\ . ' && v:val !~ ''\v(^|[\/])\.{1,2}[\/]$''')
	retu dirs
endf

fu! s:findcommon(items, seed)
	let [items, id, cmn, ic] = [copy(a:items), strlen(a:seed), '', 0]
	cal map(items, 'strpart(v:val, id)')
	for char in split(items[0], '\zs')
		for item in items[1:]
			if item[ic] != char | let brk = 1 | brea | en
		endfo
		if exists('brk') | brea | en
		let cmn .= char
		let ic += 1
	endfo
	retu cmn
endf
" Misc {{{3
fu! s:headntail(str)
	let parts = split(a:str, '[\/]\ze[^\/]\+[\/:]\?$')
	retu len(parts) == 1 ? ['', parts[0]] : len(parts) == 2 ? parts : []
endf

fu! s:lash(...)
	retu ( a:0 ? a:1 : s:dyncwd ) !~ '[\/]$' ? s:lash : ''
endf

fu! s:ispathitem()
	retu s:itemtype < 3 || ( s:itemtype > 2 && s:getextvar('type') == 'path' )
endf

fu! ctrlp#dirnfile(entries)
	let [items, cwd] = [[[], []], s:dyncwd.s:lash()]
	for each in a:entries
		let etype = getftype(each)
		if s:igntype >= 0 && s:usrign(each, etype) | con | en
		if etype == 'dir'
			if s:dotfiles | if each !~ '[\/]\.\{1,2}$'
				cal add(items[0], each)
			en | el
				cal add(items[0], each)
			en
		elsei etype == 'link'
			if s:folsym
				let isfile = !isdirectory(each)
				if s:folsym == 2 || !s:samerootsyml(each, isfile, cwd)
					cal add(items[isfile], each)
				en
			en
		elsei etype == 'file'
			cal add(items[1], each)
		en
	endfo
	retu items
endf

fu! s:usrign(item, type)
	retu s:igntype == 1 ? a:item =~ s:usrign
		\ : s:igntype == 4 && has_key(s:usrign, a:type) && s:usrign[a:type] != ''
		\ ? a:item =~ s:usrign[a:type] : 0
endf

fu! s:samerootsyml(each, isfile, cwd)
	let resolve = fnamemodify(resolve(a:each), ':p:h')
	let resolve .= s:lash(resolve)
	retu !( stridx(resolve, a:cwd) && ( stridx(a:cwd, resolve) || a:isfile ) )
endf

fu! ctrlp#rmbasedir(items)
	if a:items != [] && !stridx(a:items[0], s:dyncwd)
		let idx = strlen(s:dyncwd) + ( s:dyncwd !~ '[\/]$' )
		retu map(a:items, 'strpart(v:val, idx)')
	en
	retu a:items
endf
" Working directory {{{3
fu! s:getparent(item)
	let parent = substitute(a:item, '[\/][^\/]\+[\/:]\?$', '', '')
	if parent == '' || parent !~ '[\/]'
		let parent .= s:lash
	en
	retu parent
endf

fu! s:findroot(curr, mark, depth, type)
	let [depth, fnd] = [a:depth + 1, 0]
	if type(a:mark) == 1
		let fnd = s:glbpath(a:curr, a:mark, 1) != ''
	elsei type(a:mark) == 3
		for markr in a:mark
			if s:glbpath(a:curr, markr, 1) != '' | let fnd = 1 | brea | en
		endfo
	en
	if fnd
		if !a:type | cal ctrlp#setdir(a:curr) | en
		retu [exists('markr') ? markr : a:mark, a:curr]
	elsei depth > s:maxdepth
		cal ctrlp#setdir(s:cwd)
	el
		let parent = s:getparent(a:curr)
		if parent != a:curr
			retu s:findroot(parent, a:mark, depth, a:type)
		en
	en
	retu []
endf

fu! ctrlp#setdir(path, ...)
	let cmd = a:0 ? a:1 : 'lc!'
	sil! exe cmd ctrlp#fnesc(a:path)
	let [s:crfilerel, s:dyncwd] = [fnamemodify(s:crfile, ':.'), getcwd()]
endf
" Fallbacks {{{3
fu! s:glbpath(...)
	let cond = v:version > 702 || ( v:version == 702 && has('patch051') )
	retu call('globpath', cond ? a:000 : a:000[:1])
endf

fu! ctrlp#fnesc(path)
	retu exists('*fnameescape') ? fnameescape(a:path) : escape(a:path, " %#*?|<\"\n")
endf

fu! ctrlp#setlcdir()
	if exists('*haslocaldir')
		cal ctrlp#setdir(getcwd(), haslocaldir() ? 'lc!' : 'cd!')
	en
endf
" Highlighting {{{2
fu! ctrlp#syntax()
	if ctrlp#nosy() | retu | en
	for [ke, va] in items(s:hlgrps) | cal ctrlp#hicheck('CtrlP'.ke, va) | endfo
	if !hlexists('CtrlPLinePre')
		\ && synIDattr(synIDtrans(hlID('Normal')), 'bg') !~ '^-1$\|^$'
		sil! exe 'hi CtrlPLinePre '.( has("gui_running") ? 'gui' : 'cterm' ).'fg=bg'
	en
	sy match CtrlPNoEntries '^ == NO ENTRIES ==$'
	if hlexists('CtrlPLinePre')
		sy match CtrlPLinePre '^>'
	en
endf

fu! s:highlight(pat, grp)
	if s:matcher != {} | retu | en
	cal clearmatches()
	if !empty(a:pat) && s:ispath
		let pat = s:regexp ? substitute(a:pat, '\\\@<!\^', '^> \\zs', 'g') : a:pat
		if s:byfname
			let pat = substitute(pat, '\[\^\(.\{-}\)\]\\{-}', '[^\\/\1]\\{-}', 'g')
			let pat = substitute(pat, '\$\@<!$', '\\ze[^\\/]*$', 'g')
		en
		cal matchadd(a:grp, ( s:martcs == '' ? '\c' : '\C' ).pat)
		cal matchadd('CtrlPLinePre', '^>')
	en
endf

fu! s:dohighlight()
	retu s:mathi[0] && exists('*clearmatches') && !ctrlp#nosy()
endf
" Prompt history {{{2
fu! s:gethistloc()
	let utilcadir = ctrlp#utils#cachedir()
	let cache_dir = utilcadir.s:lash(utilcadir).'hist'
	retu [cache_dir, cache_dir.s:lash(cache_dir).'cache.txt']
endf

fu! s:gethistdata()
	retu ctrlp#utils#readfile(s:gethistloc()[1])
endf

fu! ctrlp#recordhist()
	let str = join(s:prompt, '')
	if empty(str) || !s:maxhst | retu | en
	let hst = s:hstry
	if len(hst) > 1 && hst[1] == str | retu | en
	cal extend(hst, [str], 1)
	if len(hst) > s:maxhst | cal remove(hst, s:maxhst, -1) | en
	cal ctrlp#utils#writecache(hst, s:gethistloc()[0], s:gethistloc()[1])
endf
" Signs {{{2
fu! s:unmarksigns()
	if !s:dosigns() | retu | en
	for key in keys(s:marked)
		exe 'sign unplace' key 'buffer='.s:bufnr
	endfo
endf

fu! s:remarksigns()
	if !s:dosigns() | retu | en
	for ic in range(1, len(s:lines))
		let line = s:ispath ? fnamemodify(s:lines[ic - 1], ':p') : s:lines[ic - 1]
		let key = s:dictindex(s:marked, line)
		if key > 0
			exe 'sign place' key 'line='.ic.' name=ctrlpmark buffer='.s:bufnr
		en
	endfo
endf

fu! s:dosigns()
	retu exists('s:marked') && s:bufnr > 0 && s:opmul != '0' && has('signs')
endf
" Lists & Dictionaries {{{2
fu! s:dictindex(dict, expr)
	for key in keys(a:dict)
		if a:dict[key] == a:expr | retu key | en
	endfo
	retu -1
endf

fu! s:vacantdict(dict)
	retu filter(range(1, max(keys(a:dict))), '!has_key(a:dict, v:val)')
endf

fu! s:sublist(l, s, e)
	retu v:version > 701 ? a:l[(a:s):(a:e)] : s:sublist7071(a:l, a:s, a:e)
endf

fu! s:sublist7071(l, s, e)
	let [newlist, id, ae] = [[], a:s, a:e == -1 ? len(a:l) - 1 : a:e]
	wh id <= ae
		cal add(newlist, get(a:l, id))
		let id += 1
	endw
	retu newlist
endf
" Buffers {{{2
fu! s:buftab(bufnr, md)
	for tabnr in range(1, tabpagenr('$'))
		if tabpagenr() == tabnr && a:md == 't' | con | en
		let buflist = tabpagebuflist(tabnr)
		if index(buflist, a:bufnr) >= 0
			for winnr in range(1, tabpagewinnr(tabnr, '$'))
				if buflist[winnr - 1] == a:bufnr | retu [tabnr, winnr] | en
			endfo
		en
	endfo
	retu [0, 0]
endf

fu! ctrlp#normcmd(cmd, ...)
	if a:0 < 2 && s:nosplit() | retu a:cmd | en
	let norwins = filter(range(1, winnr('$')),
		\ 'empty(getbufvar(winbufnr(v:val), "&bt"))')
	for each in norwins
		let bufnr = winbufnr(each)
		if empty(bufname(bufnr)) && empty(getbufvar(bufnr, '&ft'))
			let fstemp = each | brea
		en
	endfo
	let norwin = empty(norwins) ? 0 : norwins[0]
	if norwin
		if index(norwins, winnr()) < 0
			exe ( exists('fstemp') ? fstemp : norwin ).'winc w'
		en
		retu a:cmd
	en
	retu a:0 ? a:1 : 'bo vne'
endf

fu! s:nosplit()
	retu !empty(s:nosplit) && match([bufname('%'), &l:ft, &l:bt], s:nosplit) >= 0
endf

fu! s:setupblank()
	setl noswf nonu nobl nowrap nolist nospell nocuc wfh
	setl fdc=0 fdl=99 tw=0 bt=nofile bh=unload
	if v:version > 702
		setl nornu noudf cc=0
	en
endf

fu! s:leavepre()
	if exists('s:bufnr') && s:bufnr == bufnr('%') | bw! | en
	if !( exists('g:ctrlp_clear_cache_on_exit') && !g:ctrlp_clear_cache_on_exit )
		\ && !( has('clientserver') && len(split(serverlist(), "\n")) > 1 )
		cal ctrlp#clra()
	en
endf

fu! s:checkbuf()
	if !exists('s:init') && exists('s:bufnr') && s:bufnr > 0
		exe s:bufnr.'bw!'
	en
endf

fu! s:iscmdwin()
	let ermsg = v:errmsg
	sil! noa winc p
	sil! noa winc p
	let [v:errmsg, ermsg] = [ermsg, v:errmsg]
	retu ermsg =~ '^E11:'
endf
" Arguments {{{2
fu! s:at(str)
	if a:str =~ '\v^\@(cd|lc[hd]?|chd).*'
		let str = substitute(a:str, '\v^\@(cd|lc[hd]?|chd)\s*', '', '')
		if str == '' | retu 1 | en
		let str = str =~ '^%:.\+' ? fnamemodify(s:crfile, str[1:]) : str
		let path = fnamemodify(expand(str, 1), ':p')
		if isdirectory(path)
			if path != s:dyncwd
				cal ctrlp#setdir(path)
				cal ctrlp#setlines()
			en
			cal ctrlp#recordhist()
			cal s:PrtClear()
		en
		retu 1
	en
	retu 0
endf

fu! s:tail()
	if exists('s:optail') && !empty('s:optail')
		let tailpref = s:optail !~ '^\s*+' ? ' +' : ' '
		retu tailpref.s:optail
	en
	retu ''
endf

fu! s:sanstail(str)
	let str = s:spi ?
		\ substitute(a:str, '^\(@.*$\|\\\\\ze@\|\.\.\zs[.\/]\+$\)', '', 'g') : a:str
	let [str, pat] = [substitute(str, '\\\\', '\', 'g'), '\([^:]\|\\:\)*$']
	unl! s:optail
	if str =~ '\\\@<!:'.pat
		let s:optail = matchstr(str, '\\\@<!:\zs'.pat)
		let str = substitute(str, '\\\@<!:'.pat, '', '')
	en
	retu substitute(str, '\\\ze:', '', 'g')
endf

fu! s:argmaps(md, i)
	let roh = [
		\ ['Open Multiple Files', '/h[i]dden/[c]lear', ['i', 'c']],
		\ ['Create a New File', '/[r]eplace', ['r']],
		\ ['Open Selected', '/[r]eplace/h[i]dden? Mark [a]ll', ['r', 'i', 'a']],
		\ ]
	if a:i == 2 && s:openfunc != {} && has_key(s:openfunc, s:ctype)
		let roh[2][1] = '/[r]eplace/h[i]dden/e[x]ternal? Mark [a]ll'
		let roh[2][2] = ['r', 'i', 'x', 'a']
	en
	let str = roh[a:i][0].': [t]ab/[v]ertical/[h]orizontal'.roh[a:i][1].'? '
	retu s:choices(str, ['t', 'v', 'h'] + roh[a:i][2], 's:argmaps', [a:md, a:i])
endf

fu! s:insertstr()
	let str = 'Insert: c[w]ord/c[f]ile/[s]earch/[v]isual/[c]lipboard/[r]egister? '
	retu s:choices(str, ['w', 'f', 's', 'v', 'c', 'r'], 's:insertstr', [])
endf

fu! s:textdialog(str)
	redr | echoh MoreMsg | echon a:str | echoh None
	retu nr2char(getchar())
endf

fu! s:choices(str, choices, func, args)
	let char = s:textdialog(a:str)
	if index(a:choices, char) >= 0
		retu char
	elsei char =~# "\\v\<Esc>|\<C-c>|\<C-g>|\<C-u>|\<C-w>|\<C-[>"
		cal s:BuildPrompt(0)
		retu 'cancel'
	elsei char =~# "\<CR>" && a:args != []
		retu a:args[0]
	en
	retu call(a:func, a:args)
endf

fu! s:getregs()
	let char = s:textdialog('Insert from register: ')
	if char =~# "\\v\<Esc>|\<C-c>|\<C-g>|\<C-u>|\<C-w>|\<C-[>"
		cal s:BuildPrompt(0)
		retu -1
	elsei char =~# "\<CR>"
		retu s:getregs()
	en
	retu s:regisfilter(char)
endf

fu! s:regisfilter(reg)
	retu substitute(getreg(a:reg), "[\t\n]", ' ', 'g')
endf
" Misc {{{2
fu! s:modevar()
	let s:matchtype = s:mtype()
	let s:ispath = s:ispathitem()
	if !s:ispath | let s:byfname = 0 | en
	let s:mfunc = s:mfunc()
	let s:nolim = s:getextvar('nolim')
	let s:dosort = s:getextvar('sort')
	let s:spi = !s:itemtype || s:getextvar('specinput') > 0
endf

fu! s:nosort()
	retu s:matcher != {} || s:nolim == 1 || ( s:itemtype == 2 && s:mrudef )
		\ || ( s:itemtype =~ '\v^(1|2)$' && s:prompt == ['', '', ''] ) || !s:dosort
endf

fu! s:narrowable()
	retu exists('s:act_add') && exists('s:matched') && s:matched != []
		\ && exists('s:mdata') && s:mdata[:2] == [s:dyncwd, s:itemtype, s:regexp]
		\ && s:matcher == {}
endf

fu! s:migemo(str)
	let str = a:str
	let dict = s:glbpath(&rtp, printf("dict/%s/migemo-dict", &enc), 1)
	if !len(dict)
		let dict = s:glbpath(&rtp, "dict/migemo-dict", 1)
	en
	if len(dict)
		let [tokens, str, cmd] = [split(str, '\s'), '', 'cmigemo -v -w %s -d %s']
		for token in tokens
			let rtn = system(printf(cmd, shellescape(token), shellescape(dict)))
			let str .= !v:shell_error && strlen(rtn) > 0 ? '.*'.rtn : token
		endfo
	en
	retu str
endf

fu! s:strwidth(str)
	retu exists('*strdisplaywidth') ? strdisplaywidth(a:str) : strlen(a:str)
endf

fu! ctrlp#j2l(nr)
	exe a:nr
	sil! norm! zvzz
endf

fu! s:maxf(len)
	retu s:maxfiles && a:len > s:maxfiles ? 1 : 0
endf

fu! s:regexfilter(str)
	let str = a:str
	for key in keys(s:fpats) | if str =~ key
		let str = substitute(str, s:fpats[key], '', 'g')
	en | endfo
	retu str
endf

fu! s:walker(m, p, d)
	retu a:d > 0 ? a:p < a:m ? a:p + a:d : 0 : a:p > 0 ? a:p + a:d : a:m
endf

fu! s:delent(rfunc)
	if a:rfunc == '' | retu | en
	let [s:force, tbrem] = [1, []]
	if exists('s:marked')
		let tbrem = values(s:marked)
		cal s:unmarksigns()
		unl s:marked
	en
	if tbrem == [] && ( has('dialog_gui') || has('dialog_con') ) &&
		\ confirm("Wipe all entries?", "&OK\n&Cancel") != 1
		unl s:force
		cal s:BuildPrompt(0)
		retu
	en
	let g:ctrlp_lines = call(a:rfunc, [tbrem])
	cal s:BuildPrompt(1)
	unl s:force
endf
" Entering & Exiting {{{2
fu! s:getenv()
	let [s:cwd, s:winres] = [getcwd(), [winrestcmd(), &lines, winnr('$')]]
	let [s:crfile, s:crfpath] = [expand('%:p', 1), expand('%:p:h', 1)]
	let [s:crword, s:crline] = [expand('<cword>', 1), getline('.')]
	let [s:winh, s:crcursor] = [min([s:mxheight, &lines]), getpos('.')]
	let [s:crbufnr, s:crvisual] = [bufnr('%'), s:lastvisual()]
	let s:wpmode = exists('b:ctrlp_working_path_mode')
		\ ? b:ctrlp_working_path_mode : s:pathmode
	let [s:mrbs, s:crgfile] = [ctrlp#mrufiles#bufs(), expand('<cfile>', 1)]
endf

fu! s:lastvisual()
	let cview = winsaveview()
	let [ovreg, ovtype] = [getreg('v'), getregtype('v')]
	let [oureg, outype] = [getreg('"'), getregtype('"')]
	sil! norm! gv"vy
	let selected = s:regisfilter('v')
	cal setreg('v', ovreg, ovtype)
	cal setreg('"', oureg, outype)
	cal winrestview(cview)
	retu selected
endf

fu! s:log(m)
	if exists('g:ctrlp_log') && g:ctrlp_log | if a:m
		let cadir = ctrlp#utils#cachedir()
		sil! exe 'redi! >' cadir.s:lash(cadir).'ctrlp.log'
	el
		sil! redi END
	en | en
endf

fu! s:buffunc(e)
	if a:e && has_key(s:buffunc, 'enter')
		cal call(s:buffunc['enter'], [])
	elsei !a:e && has_key(s:buffunc, 'exit')
		cal call(s:buffunc['exit'], [])
	en
endf

fu! s:openfile(cmd, fid, tail, ...)
	let cmd = a:cmd =~ '^[eb]$' && &modified ? 'hid '.a:cmd : a:cmd
	let cmd = cmd =~ '^tab' ? ctrlp#tabcount().cmd : cmd
	let j2l = a:0 && a:1 ? a:2 : 0
	exe cmd.( a:0 && a:1 ? '' : a:tail ) ctrlp#fnesc(a:fid)
	if j2l
		exe j2l
	en
	if !empty(a:tail) || j2l
		sil! norm! zvzz
	en
	if cmd != 'bad'
		cal ctrlp#setlcdir()
	en
endf

fu! ctrlp#tabcount()
	if exists('s:tabct')
		let tabct = s:tabct
		let s:tabct += 1
	elsei !type(s:tabpage)
		let tabct = s:tabpage
	elsei type(s:tabpage) == 1
		let tabpos =
			\ s:tabpage =~ 'c' ? tabpagenr() :
			\ s:tabpage =~ 'f' ? 1 :
			\ s:tabpage =~ 'l' ? tabpagenr('$') :
			\ tabpagenr()
		let tabct =
			\ s:tabpage =~ 'a' ? tabpos :
			\ s:tabpage =~ 'b' ? tabpos - 1 :
			\ tabpos
	en
	retu tabct < 0 ? 0 : tabct
endf

fu! s:settype(type)
	retu a:type < 0 ? exists('s:itemtype') ? s:itemtype : 0 : a:type
endf
" Matching {{{2
fu! s:matchfname(item, pat)
	let parts = split(a:item, '[\/]\ze[^\/]\+$')
	let mfn = match(parts[-1], a:pat[0])
	retu len(a:pat) == 1 ? mfn : len(a:pat) == 2 ?
		\ ( mfn >= 0 && ( len(parts) == 2 ? match(parts[0], a:pat[1]) : -1 ) >= 0
		\ ? 0 : -1 ) : -1
	en
endf

fu! s:matchtabs(item, pat)
	retu match(split(a:item, '\t\+')[0], a:pat)
endf

fu! s:matchtabe(item, pat)
	retu match(split(a:item, '\t\+[^\t]\+$')[0], a:pat)
endf

fu! s:buildpat(lst)
	let pat = a:lst[0]
	for item in range(1, len(a:lst) - 1)
		let pat .= '[^'.a:lst[item - 1].']\{-}'.a:lst[item]
	endfo
	retu pat
endf

fu! s:mfunc()
	let mfunc = 'match'
	if s:byfname && s:ispath
		let mfunc = 's:matchfname'
	elsei s:itemtype > 2
		let matchtypes = { 'tabs': 's:matchtabs', 'tabe': 's:matchtabe' }
		if has_key(matchtypes, s:matchtype)
			let mfunc = matchtypes[s:matchtype]
		en
	en
	retu mfunc
endf

fu! s:mmode()
	let matchmodes = {
		\ 'match': 'full-line',
		\ 's:matchfname': 'filename-only',
		\ 's:matchtabs': 'first-non-tab',
		\ 's:matchtabe': 'until-last-tab',
		\ }
	retu matchmodes[s:mfunc]
endf
" Cache {{{2
fu! s:writecache(cache_file)
	if ( g:ctrlp_newcache || !filereadable(a:cache_file) ) && !s:nocache()
		cal ctrlp#utils#writecache(g:ctrlp_allfiles)
		let g:ctrlp_newcache = 0
	en
endf

fu! s:nocache()
	retu !s:caching || ( s:caching > 1 && get(s:ficounts, s:dyncwd) < s:caching )
endf

fu! s:insertcache(str)
	let [data, g:ctrlp_newcache, str] = [g:ctrlp_allfiles, 1, a:str]
	if strlen(str) <= strlen(data[0])
		let pos = 0
	elsei strlen(str) >= strlen(data[-1])
		let pos = len(data) - 1
	el
		let pos = 0
		for each in data
			if strlen(each) > strlen(str) | brea | en
			let pos += 1
		endfo
	en
	cal insert(data, str, pos)
	cal s:writecache(ctrlp#utils#cachefile())
endf
" Extensions {{{2
fu! s:mtype()
	retu s:itemtype > 2 ? s:getextvar('type') : 'path'
endf

fu! s:execextvar(key)
	if !empty(g:ctrlp_ext_vars)
		cal map(filter(copy(g:ctrlp_ext_vars),
			\ 'has_key(v:val, a:key)'), 'eval(v:val[a:key])')
	en
endf

fu! s:getextvar(key)
	if s:itemtype > 2
		let vars = g:ctrlp_ext_vars[s:itemtype - 3]
		retu has_key(vars, a:key) ? vars[a:key] : -1
	en
	retu -1
endf

fu! ctrlp#exit()
	cal s:PrtExit()
endf

fu! ctrlp#prtclear()
	cal s:PrtClear()
endf

fu! ctrlp#switchtype(id)
	cal s:ToggleType(a:id - s:itemtype)
endf

fu! ctrlp#nosy()
	retu !( has('syntax') && exists('g:syntax_on') )
endf

fu! ctrlp#hicheck(grp, defgrp)
	if !hlexists(a:grp)
		exe 'hi link' a:grp a:defgrp
	en
endf

fu! ctrlp#call(func, ...)
	cal call(a:func, a:000)
endf
"}}}1
" * Initialization {{{1
fu! ctrlp#setlines(...)
	if a:0 | let s:itemtype = a:1 | en
	cal s:modevar()
	let types = ['ctrlp#files()', 'ctrlp#buffers()', 'ctrlp#mrufiles#list()']
	if !empty(g:ctrlp_ext_vars)
		cal map(copy(g:ctrlp_ext_vars), 'add(types, v:val["init"])')
	en
	let g:ctrlp_lines = eval(types[s:itemtype])
endf

fu! ctrlp#init(type, ...)
	if exists('s:init') || s:iscmdwin() | retu | en
	let [s:matches, s:init] = [1, 1]
	cal ctrlp#reset()
	noa cal s:Open()
	cal s:SetWD(a:0 ? a:1 : {})
	cal s:MapKeys()
	cal ctrlp#syntax()
	cal ctrlp#setlines(s:settype(a:type))
	cal s:SetDefTxt()
	cal s:BuildPrompt(1)
endf
" - Autocmds {{{1
if has('autocmd')
	aug CtrlPAug
		au!
		au BufEnter ControlP cal s:checkbuf()
		au BufLeave ControlP noa cal s:Close()
		au VimLeavePre * cal s:leavepre()
	aug END
en

fu! s:autocmds()
	if !has('autocmd') | retu | en
	if exists('#CtrlPLazy')
		au! CtrlPLazy
	en
	if s:lazy
		aug CtrlPLazy
			au!
			au CursorHold ControlP cal s:ForceUpdate()
		aug END
	en
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
