function! Pl#Match#Add(pat, expr) " {{{
	return [a:pat, a:expr]
endfunction " }}}
function! Pl#Match#Any(...) " {{{
	let matches = []

	for match_name in a:000
		if empty(match_name)
			" Skip empty match parameters
			continue
		endif

		if has_key(g:Powerline#Matches#matches, match_name)
			call add(matches, g:Powerline#Matches#matches[match_name])
		endif

		unlet! match_name
	endfor

	return ['match', 'any', matches]
endfunction " }}}
function! Pl#Match#Validate(theme) " {{{
	let match = a:theme.matches[1]

	if match == 'none'
		return 0
	elseif match == 'any'
		let matches = a:theme.matches[2]

		if ! len(matches)
			" Empty match array matches everything
			return 1
		endif

		for [eval, re] in matches
			if match(eval(eval), '\v'. re) != -1
				return 1
			endif
		endfor

		return 0
	endif
endfunction " }}}
