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
	let win = ['win16', 'win32', 'win64', 'win95']
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

" Shelljoin returns a shell-safe string representation of arglist. The
" {special} argument of shellescape() may optionally be passed.
function! go#util#Shelljoin(arglist, ...)
	if a:0
		return join(map(copy(a:arglist), 'shellescape(v:val, ' . a:1 . ')'), ' ')
	endif

	return join(map(copy(a:arglist), 'shellescape(v:val)'), ' ')
endfunction

" Shelljoin returns a shell-safe representation of the items in the given
" arglist. The {special} argument of shellescape() may optionally be passed.
function! go#util#Shelllist(arglist, ...)
	if a:0
		return map(copy(a:arglist), 'shellescape(v:val, ' . a:1 . ')')
    endif
	return map(copy(a:arglist), 'shellescape(v:val)')
endfunction

" TODO(arslan): I couldn't parameterize the highlight types. Check if we can
" simplify the following functions

function! go#util#EchoSuccess(msg)
    redraws! | echon "vim-go: " | echohl Function | echon a:msg | echohl None
endfunction

function! go#util#EchoError(msg)
    redraws! | echon "vim-go: " | echohl ErrorMsg | echon a:msg | echohl None
endfunction

function! go#util#EchoWarning(msg)
    redraws! | echon "vim-go: " | echohl WarningMsg | echon a:msg | echohl None
endfunction

function! go#util#EchoProgress(msg)
    redraws! | echon "vim-go: " | echohl Identifier | echon a:msg | echohl None
endfunction

" vim:ts=4:sw=4:et
