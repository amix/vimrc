let s:go_decls_var = {
			\  'init':   'ctrlp#decls#init()',
			\  'exit':   'ctrlp#decls#exit()',
			\  'enter':  'ctrlp#decls#enter()',
			\  'accept': 'ctrlp#decls#accept',
			\  'lname':  'declarations',
			\  'sname':  'decls',
			\  'type':   'tabs',
			\}

if exists('g:ctrlp_ext_vars') && !empty(g:ctrlp_ext_vars)
	let g:ctrlp_ext_vars = add(g:ctrlp_ext_vars, s:go_decls_var)
else
	let g:ctrlp_ext_vars = [s:go_decls_var]
endif

function! ctrlp#decls#init()
	cal s:enable_syntax()
	return s:decls
endfunction

function! ctrlp#decls#exit()
	unlet! s:decls s:current_dir s:target
endfunction

" The action to perform on the selected string
" Arguments:
"  a:mode   the mode that has been chosen by pressing <cr> <c-v> <c-t> or <c-x>
"           the values are 'e', 'v', 't' and 'h', respectively
"  a:str    the selected string
function! ctrlp#decls#accept(mode, str)
	let cd = exists('*haslocaldir') && haslocaldir() ? 'lcd ' : 'cd '
	let dir = getcwd()
	try
		" we jump to the file directory so we can get the fullpath via fnamemodify
		" below
		execute cd . s:current_dir

		let vals = matchlist(a:str, '|\(.\{-}\):\(\d\+\):\(\d\+\)\s*\(.*\)|')

		" i.e: main.go
		let filename =  vals[1] 
		let line =  vals[2]
		let col =  vals[3]

		" i.e: /Users/fatih/vim-go/main.go
		let filepath =  fnamemodify(filename, ":p")

		" acceptile is a very versatile method,
		call ctrlp#acceptfile(a:mode, filepath)
		call cursor(line, col)
		silent! norm! zvzz
	finally
		"jump back to old dir
		execute cd . fnameescape(dir)
	endtry
endfunction

function! ctrlp#decls#enter()
	let s:current_dir = fnameescape(expand('%:p:h'))
	let s:decls = []

	let bin_path = go#path#CheckBinPath('motion')
	if empty(bin_path)
		return
	endif
	let command = printf("%s -format vim -mode decls", bin_path)
	let command .= " -include ".  get(g:, "go_decls_includes", "func,type")

	call go#cmd#autowrite()

	if s:mode == 0
		" current file mode
		let fname = expand("%:p")
		if exists('s:target')
			let fname = s:target
		endif

		let command .= printf(" -file %s", fname)
	else
		" all functions mode
		let dir = expand("%:p:h")
		if exists('s:target')
			let dir = s:target
		endif

		let command .= printf(" -dir %s", dir)
	endif

	let out = system(command)
	if v:shell_error != 0
		call go#util#EchoError(out)
		return
	endif

	if exists("l:tmpname")
		call delete(l:tmpname)
	endif

	let result = eval(out)
	if type(result) != 4 || !has_key(result, 'decls')
		return
	endif

	let decls = result.decls

	" find the maximum function name
	let max_len = 0
	for decl in decls
		if len(decl.ident)> max_len
			let max_len = len(decl.ident)
		endif
	endfor

	for decl in decls
		" paddings
		let space = " "
		for i in range(max_len - len(decl.ident))
			let space .= " "
		endfor

		call add(s:decls, printf("%s\t%s |%s:%s:%s|\t%s", 
					\ decl.ident . space,
					\ decl.keyword,
					\ fnamemodify(decl.filename, ":t"),
					\ decl.line,
					\ decl.col,
					\ decl.full,
					\))
	endfor
endfunc

function! s:enable_syntax()
	if !(has('syntax') && exists('g:syntax_on'))
		return
	endif

	syntax match CtrlPIdent      '\zs\h\+\ze\s' 
	syntax match CtrlPKeyword		 '\zs[^\t|]\+\ze|[^|]\+:\d\+:\d\+|'
	syntax match CtrlPFilename   '|\zs[^|]\+:\d\+:\d\+\ze|'
	syntax match CtrlPSignature  '\zs\t.*\ze$' contains=CtrlPKeyWord,CtrlPFilename

	highlight link  CtrlPIdent      Function
	highlight link  CtrlPKeyword   Keyword
	highlight link  CtrlPFilename  SpecialComment
	highlight link  CtrlPSignature Comment
endfunction

let s:id = g:ctrlp_builtins + len(g:ctrlp_ext_vars)

function! ctrlp#decls#cmd(mode, ...)
	let s:mode = a:mode
	if a:0 && !empty(a:1)
		let s:target = a:1
	endif
	return s:id
endfunction

