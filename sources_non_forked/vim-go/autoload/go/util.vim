" PathSep returns the appropriate OS specific path separator.
function! go#util#PathSep()
	if go#util#IsWin()
		return '\'
	endif
	return '/'
endfunction

" PathListSep returns the appropriate OS specific path list separator.
function! go#util#PathListSep()
	if go#util#IsWin()
		return ";"
	endif
	return ":"
endfunction

" LineEnding returns the correct line ending, based on the current fileformat
function! go#util#LineEnding()
	if &fileformat == 'dos'
		return "\r\n"
	elseif &fileformat == 'mac'
		return "\r"
	endif

	return "\n"
endfunction

" IsWin returns 1 if current OS is Windows or 0 otherwise
function! go#util#IsWin()
	let win = ['win16', 'win32', 'win32unix', 'win64', 'win95']
	for w in win
		if (has(w))
			return 1
		endif
	endfor

	return 0
endfunction

" StripPath strips the path's last character if it's a path separator.
" example: '/foo/bar/'  -> '/foo/bar'
function! go#util#StripPathSep(path)
	let last_char = strlen(a:path) - 1
	if a:path[last_char] == go#util#PathSep()
		return strpart(a:path, 0, last_char)
	endif

	return a:path
endfunction

" vim:ts=4:sw=4:et
