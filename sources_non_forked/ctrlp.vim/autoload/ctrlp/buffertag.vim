" =============================================================================
" File:          autoload/ctrlp/buffertag.vim
" Description:   Buffer Tag extension
" Maintainer:    Kien Nguyen <github.com/kien>
" Credits:       Much of the code was taken from tagbar.vim by Jan Larres, plus
"                a few lines from taglist.vim by Yegappan Lakshmanan and from
"                buffertag.vim by Takeshi Nishida.
" =============================================================================

" Init {{{1
if exists('g:loaded_ctrlp_buftag') && g:loaded_ctrlp_buftag
	fini
en
let g:loaded_ctrlp_buftag = 1

cal add(g:ctrlp_ext_vars, {
	\ 'init': 'ctrlp#buffertag#init(s:crfile)',
	\ 'accept': 'ctrlp#buffertag#accept',
	\ 'lname': 'buffer tags',
	\ 'sname': 'bft',
	\ 'exit': 'ctrlp#buffertag#exit()',
	\ 'type': 'tabs',
	\ 'opts': 'ctrlp#buffertag#opts()',
	\ })

let s:id = g:ctrlp_builtins + len(g:ctrlp_ext_vars)

let [s:pref, s:opts] = ['g:ctrlp_buftag_', {
	\ 'systemenc': ['s:enc', &enc],
	\ 'ctags_bin': ['s:bin', ''],
	\ 'types': ['s:usr_types', {}],
	\ }]

let s:bins = [
	\ 'ctags-exuberant',
	\ 'exuberant-ctags',
	\ 'exctags',
	\ '/usr/local/bin/ctags',
	\ '/opt/local/bin/ctags',
	\ 'ctags',
	\ 'ctags.exe',
	\ 'tags',
	\ ]

let s:types = {
	\ 'ant'    : '%sant%sant%spt',
	\ 'asm'    : '%sasm%sasm%sdlmt',
	\ 'aspperl': '%sasp%sasp%sfsv',
	\ 'aspvbs' : '%sasp%sasp%sfsv',
	\ 'awk'    : '%sawk%sawk%sf',
	\ 'beta'   : '%sbeta%sbeta%sfsv',
	\ 'c'      : '%sc%sc%sdgsutvf',
	\ 'cpp'    : '%sc++%sc++%snvdtcgsuf',
	\ 'cs'     : '%sc#%sc#%sdtncEgsipm',
	\ 'cobol'  : '%scobol%scobol%sdfgpPs',
	\ 'delphi' : '%spascal%spascal%sfp',
	\ 'dosbatch': '%sdosbatch%sdosbatch%slv',
	\ 'eiffel' : '%seiffel%seiffel%scf',
	\ 'erlang' : '%serlang%serlang%sdrmf',
	\ 'expect' : '%stcl%stcl%scfp',
	\ 'fortran': '%sfortran%sfortran%spbceiklmntvfs',
	\ 'go'     : '%sgo%sgo%sfctv',
	\ 'html'   : '%shtml%shtml%saf',
	\ 'java'   : '%sjava%sjava%spcifm',
	\ 'javascript': '%sjavascript%sjavascript%sf',
	\ 'lisp'   : '%slisp%slisp%sf',
	\ 'lua'    : '%slua%slua%sf',
	\ 'make'   : '%smake%smake%sm',
	\ 'matlab' : '%smatlab%smatlab%sf',
	\ 'ocaml'  : '%socaml%socaml%scmMvtfCre',
	\ 'pascal' : '%spascal%spascal%sfp',
	\ 'perl'   : '%sperl%sperl%sclps',
	\ 'php'    : '%sphp%sphp%scdvf',
	\ 'python' : '%spython%spython%scmf',
	\ 'rexx'   : '%srexx%srexx%ss',
	\ 'ruby'   : '%sruby%sruby%scfFm',
	\ 'rust'   : '%srust%srust%sfTgsmctid',
	\ 'scheme' : '%sscheme%sscheme%ssf',
	\ 'sh'     : '%ssh%ssh%sf',
	\ 'csh'    : '%ssh%ssh%sf',
	\ 'zsh'    : '%ssh%ssh%sf',
	\ 'scala'  : '%sscala%sscala%sctTmlp',
	\ 'slang'  : '%sslang%sslang%snf',
	\ 'sml'    : '%ssml%ssml%secsrtvf',
	\ 'sql'    : '%ssql%ssql%scFPrstTvfp',
	\ 'tex'    : '%stex%stex%sipcsubPGl',
	\ 'tcl'    : '%stcl%stcl%scfmp',
	\ 'vera'   : '%svera%svera%scdefgmpPtTvx',
	\ 'verilog': '%sverilog%sverilog%smcPertwpvf',
	\ 'vhdl'   : '%svhdl%svhdl%sPctTrefp',
	\ 'vim'    : '%svim%svim%savf',
	\ 'yacc'   : '%syacc%syacc%sl',
	\ }

cal map(s:types, 'printf(v:val, "--language-force=", " --", "-types=")')

if executable('jsctags')
	cal extend(s:types, { 'javascript': { 'args': '-f -', 'bin': 'jsctags' } })
en

fu! ctrlp#buffertag#opts()
	for [ke, va] in items(s:opts)
		let {va[0]} = exists(s:pref.ke) ? {s:pref.ke} : va[1]
	endfo
	" Ctags bin
	if empty(s:bin)
		for bin in s:bins | if executable(bin)
			let s:bin = bin
			brea
		en | endfo
	el
		let s:bin = expand(s:bin, 1)
	en
	" Types
	cal extend(s:types, s:usr_types)
endf
" Utilities {{{1
fu! s:validfile(fname, ftype)
	if ( !empty(a:fname) || !empty(a:ftype) ) && filereadable(a:fname)
		\ && index(keys(s:types), a:ftype) >= 0 | retu 1 | en
	retu 0
endf

fu! s:exectags(cmd)
	if exists('+ssl')
		let [ssl, &ssl] = [&ssl, 0]
	en
	if &sh =~ 'cmd\.exe'
		let [sxq, &sxq, shcf, &shcf] = [&sxq, '"', &shcf, '/s /c']
	en
	let output = system(a:cmd)
	if &sh =~ 'cmd\.exe'
		let [&sxq, &shcf] = [sxq, shcf]
	en
	if exists('+ssl')
		let &ssl = ssl
	en
	retu output
endf

fu! s:exectagsonfile(fname, ftype)
	let [ags, ft] = ['-f - --sort=no --excmd=pattern --fields=nKs --extra= --file-scope=yes ', a:ftype]
	if type(s:types[ft]) == 1
		let ags .= s:types[ft]
		let bin = s:bin
	elsei type(s:types[ft]) == 4
		let ags = s:types[ft]['args']
		let bin = expand(s:types[ft]['bin'], 1)
	en
	if empty(bin) | retu '' | en
	let cmd = s:esctagscmd(bin, ags, a:fname)
	if empty(cmd) | retu '' | en
	let output = s:exectags(cmd)
	if v:shell_error || output =~ 'Warning: cannot open' | retu '' | en
	retu output
endf

fu! s:esctagscmd(bin, args, ...)
	if exists('+ssl')
		let [ssl, &ssl] = [&ssl, 0]
	en
	let fname = a:0 ? shellescape(a:1) : ''
	if  (has('win32') || has('win64'))
		let cmd = a:bin.' '.a:args.' '.fname
	else
		let cmd = shellescape(a:bin).' '.a:args.' '.fname
	endif
	if &sh =~ 'cmd\.exe'
		let cmd = substitute(cmd, '[&()@^<>|]', '^\0', 'g')
	en
	if exists('+ssl')
		let &ssl = ssl
	en
	if has('iconv')
		let last = s:enc != &enc ? s:enc : !empty( $LANG ) ? $LANG : &enc
		let cmd = iconv(cmd, &enc, last)
	en
	retu cmd
endf

fu! s:process(fname, ftype)
	if !s:validfile(a:fname, a:ftype) | retu [] | endif
	let ftime = getftime(a:fname)
	if has_key(g:ctrlp_buftags, a:fname)
		\ && g:ctrlp_buftags[a:fname]['time'] >= ftime
		let lines = g:ctrlp_buftags[a:fname]['lines']
	el
		let data = s:exectagsonfile(a:fname, a:ftype)
		let [raw, lines] = [split(data, '\n\+'), []]
		for line in raw
			if line !~# '^!_TAG_' && len(split(line, ';"')) == 2
				let parsed_line = s:parseline(line)
				if parsed_line != ''
					cal add(lines, parsed_line)
				en
			en
		endfo
		let cache = { a:fname : { 'time': ftime, 'lines': lines } }
		cal extend(g:ctrlp_buftags, cache)
	en
	retu lines
endf

fu! s:parseline(line)
	let vals = matchlist(a:line,
		\ '\v^([^\t]+)\t(.+)\t[?/]\^?(.{-1,})\$?[?/]\;\"\t(.+)\tline(no)?\:(\d+)')
	if vals == [] | retu '' | en
	let [bufnr, bufname] = [bufnr('^'.vals[2].'$'), fnamemodify(vals[2], ':p:t')]
	retu vals[1].'	'.vals[4].'|'.bufnr.':'.bufname.'|'.vals[6].'| '.vals[3]
endf

fu! s:syntax()
	if !ctrlp#nosy()
		cal ctrlp#hicheck('CtrlPTagKind', 'Title')
		cal ctrlp#hicheck('CtrlPBufName', 'Directory')
		cal ctrlp#hicheck('CtrlPTabExtra', 'Comment')
		sy match CtrlPTagKind '\zs[^\t|]\+\ze|\d\+:[^|]\+|\d\+|'
		sy match CtrlPBufName '|\d\+:\zs[^|]\+\ze|\d\+|'
		sy match CtrlPTabExtra '\zs\t.*\ze$' contains=CtrlPBufName,CtrlPTagKind
	en
endf

fu! s:chknearby(pat)
	if match(getline('.'), a:pat) < 0
		let [int, forw, maxl] = [1, 1, line('$')]
		wh !search(a:pat, 'W'.( forw ? '' : 'b' ))
			if !forw
				if int > maxl | brea | en
				let int += int
			en
			let forw = !forw
		endw
	en
endf
" Public {{{1
fu! ctrlp#buffertag#init(fname)
	let bufs = exists('s:btmode') && s:btmode
		\ ? filter(ctrlp#buffers(), 'filereadable(v:val)')
		\ : [exists('s:bufname') ? s:bufname : a:fname]
	let lines = []
	for each in bufs
		let bname = fnamemodify(each, ':p')
		let tftype = get(split(getbufvar('^'.bname.'$', '&ft'), '\.'), 0, '')
		cal extend(lines, s:process(bname, tftype))
	endfo
	cal s:syntax()
	retu lines
endf

fu! ctrlp#buffertag#accept(mode, str)
	let vals = matchlist(a:str,
		\ '\v^[^\t]+\t+[^\t|]+\|(\d+)\:[^\t|]+\|(\d+)\|\s(.+)$')
	let bufnr = str2nr(get(vals, 1))
	if bufnr
		cal ctrlp#acceptfile(a:mode, bufnr)
		exe 'norm!' str2nr(get(vals, 2, line('.'))).'G'
		cal s:chknearby('\V\C'.get(vals, 3, ''))
		sil! norm! zvzz
	en
endf

fu! ctrlp#buffertag#cmd(mode, ...)
	let s:btmode = a:mode
	if a:0 && !empty(a:1)
		let s:btmode = 0
		let bname = a:1 =~# '^%$\|^#\d*$' ? expand(a:1) : a:1
		let s:bufname = fnamemodify(bname, ':p')
	en
	retu s:id
endf

fu! ctrlp#buffertag#exit()
	unl! s:btmode s:bufname
endf
"}}}

" vim:fen:fdm=marker:fmr={{{,}}}:fdl=0:fdc=1:ts=2:sw=2:sts=2
