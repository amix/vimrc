if !exists("g:go_godef_bin")
	let g:go_godef_bin = "godef"
endif

if go#vimproc#has_vimproc()
	let s:vim_system = get(g:, 'gocomplete#system_function', 'vimproc#system2')
else
	let s:vim_system = get(g:, 'gocomplete#system_function', 'system')
endif

fu! s:system(str, ...)
	return call(s:vim_system, [a:str] + a:000)
endf

" modified and improved version of vim-godef
function! go#def#Jump(...)
	if !len(a:000)
		let arg = "-o=" . go#util#OffsetCursor()
	else
		let arg = a:1
	endif

	let bin_path = go#path#CheckBinPath(g:go_godef_bin)
	if empty(bin_path)
		return
	endif

	let old_gopath = $GOPATH
	let $GOPATH = go#path#Detect()

	let fname = fnamemodify(expand("%"), ':p:gs?\\?/?')
	let command = bin_path . " -f=" . shellescape(fname) . " -i " . shellescape(arg)

	" get output of godef
	let out = s:system(command, join(getbufline(bufnr('%'), 1, '$'), go#util#LineEnding()))

	" jump to it
	call s:godefJump(out, "")
	let $GOPATH = old_gopath
endfunction


function! go#def#JumpMode(mode)
	let arg = "-o=" . go#util#OffsetCursor()

	let bin_path = go#path#CheckBinPath(g:go_godef_bin)
	if empty(bin_path)
		return
	endif

	let old_gopath = $GOPATH
	let $GOPATH = go#path#Detect()

	let fname = fnamemodify(expand("%"), ':p:gs?\\?/?')
	let command = bin_path . " -f=" . shellescape(fname) . " -i " . shellescape(arg)

	" get output of godef
	let out = s:system(command, join(getbufline(bufnr('%'), 1, '$'), go#util#LineEnding()))

	call s:godefJump(out, a:mode)
	let $GOPATH = old_gopath
endfunction


function! s:getOffset()
	return "-o=" . go#util#OffsetCursor()
endfunction


function! s:godefJump(out, mode)
	let old_errorformat = &errorformat
	let &errorformat = "%f:%l:%c"

	if a:out =~ 'godef: '
		let out = substitute(a:out, go#util#LineEnding() . '$', '', '')
		echom out
	else
		let parts = split(a:out, ':')
		" parts[0] contains filename
		let fileName = parts[0]

		" put the error format into location list so we can jump automatically to
		" it
		lgetexpr a:out

		" needed for restoring back user setting this is because there are two
		" modes of switchbuf which we need based on the split mode
		let old_switchbuf = &switchbuf

		if a:mode == "tab"
			let &switchbuf = "usetab"

			if bufloaded(fileName) == 0
				tab split
			endif
		else
			if a:mode  == "split"
				split
			elseif a:mode == "vsplit"
				vsplit
			endif
		endif

		" jump to file now
		sil ll 1
		normal! zz

		let &switchbuf = old_switchbuf
	end
	let &errorformat = old_errorformat
endfunction
