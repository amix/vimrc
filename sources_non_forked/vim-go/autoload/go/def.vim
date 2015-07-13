if !exists("g:go_godef_bin")
	let g:go_godef_bin = "godef"
endif


" modified and improved version of vim-godef
function! go#def#Jump(...)
	if !len(a:000)
		" gives us the offset of the word, basicall the position of the word under
		" he cursor
		let arg = s:getOffset()
	else
		let arg = a:1
	endif

	let bin_path = go#path#CheckBinPath(g:go_godef_bin)
	if empty(bin_path)
		return
	endif

	let old_gopath = $GOPATH
	let $GOPATH = go#path#Detect()

	let command = bin_path . " -f=" . shellescape(expand("%:p")) . " -i " . shellescape(arg)

	" get output of godef
	let out=system(command, join(getbufline(bufnr('%'), 1, '$'), go#util#LineEnding()))

	" jump to it
	call s:godefJump(out, "")
	let $GOPATH = old_gopath
endfunction


function! go#def#JumpMode(mode)
	let arg = s:getOffset()

	let bin_path = go#path#CheckBinPath(g:go_godef_bin)
	if empty(bin_path)
		return
	endif

	let old_gopath = $GOPATH
	let $GOPATH = go#path#Detect()

	let command = bin_path . " -f=" . shellescape(expand("%:p")) . " -i " . shellescape(arg)

	" get output of godef
	let out=system(command, join(getbufline(bufnr('%'), 1, '$'), go#util#LineEnding()))

	call s:godefJump(out, a:mode)
	let $GOPATH = old_gopath
endfunction


function! s:getOffset()
	let pos = getpos(".")[1:2]
	if &encoding == 'utf-8'
		let offs = line2byte(pos[0]) + pos[1] - 2
	else
		let c = pos[1]
		let buf = line('.') == 1 ? "" : (join(getline(1, pos[0] - 1), go#util#LineEnding()) . go#util#LineEnding())
		let buf .= c == 1 ? "" : getline(pos[0])[:c-2]
		let offs = len(iconv(buf, &encoding, "utf-8"))
	endif

	let argOff = "-o=" . offs
	return argOff
endfunction


function! s:godefJump(out, mode)
	let old_errorformat = &errorformat
	let &errorformat = "%f:%l:%c"

	if a:out =~ 'godef: '
		let out=substitute(a:out, go#util#LineEnding() . '$', '', '')
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
		normal zz

		let &switchbuf = old_switchbuf
	end
	let &errorformat = old_errorformat
endfunction
