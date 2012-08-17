function! Powerline#Functions#ft_man#GetName() " {{{
	let matches = matchlist(getline(1), '\v^([a-zA-Z_\.\-]+)\((\d+)\)')

	if ! len(matches)
		return 'n/a'
	endif

	let file = tolower(matches[1])
	let num = matches[2]

	return file
endfunction " }}}
