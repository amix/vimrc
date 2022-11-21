let s:sigil = nr2char(31)
let snipmate#legacy#sigil = s:sigil

" Prepare snippet to be processed by s:BuildTabStops
function! snipmate#legacy#process_snippet(snip) abort
	let snippet = a:snip
	let esc_bslash = '\%(\\\@<!\%(\\\\\)*\)\@<='

	if exists('b:snipmate_visual')
		let visual = substitute(b:snipmate_visual, "\n$", '', '')
		unlet b:snipmate_visual
	else
		let visual = ''
	endif
        let snippet = s:substitute_visual(snippet, visual)

	" Evaluate eval (`...`) expressions.
	" Backquotes prefixed with a backslash "\" are ignored.
	" And backslash can be escaped by doubling it.
	" Using a loop here instead of a regex fixes a bug with nested "\=".
	if stridx(snippet, '`') != -1
		let new = []
		let snip = split(snippet, esc_bslash . '`', 1)
		let isexp = 0
		for i in snip
			if isexp
				call add(new, substitute(snipmate#util#eval(i),
                                            \ "\n\\%$", '', ''))
			else
				call add(new, i)
			endif
			let isexp = !isexp
		endfor
		let snippet = join(new, '')
		let snippet = substitute(snippet, "\r", "\n", 'g')
		let snippet = substitute(snippet, '\\`', "`", 'g')
	endif

	" Place all text after a colon in a tab stop after the tab stop
	" (e.g. "${#:foo}" becomes "${:foo}foo").
	" This helps tell the position of the tab stops later.
	let snippet = substitute(snippet, esc_bslash . '\$\({\d\+:\(.\{-}\)}\|{\d\+}\)', s:sigil . '\1\2', 'g')
	let snippet = substitute(snippet, esc_bslash . '\$\(\d\+\)', s:sigil . '\1', 'g')
	let snippet = substitute(snippet, esc_bslash . '\\\$', '$', 'g')
	let snippet = substitute(snippet, '\\\\', "\\", 'g')

	" Update the a:snip so that all the $# become the text after
	" the colon in their associated ${#}.
	" (e.g. "${1:foo}" turns all "$1"'s into "foo")
	let i = 0
	if snippet !~ s:sigil . '{0'
		let snippet .= s:sigil . '{0}'
	endif
	while snippet =~ s:sigil.'{'.i
		let s = matchstr(snippet, s:sigil . '{' . i . ':\zs.\{-}\ze}')
		if s != ''
			let snippet = substitute(snippet, s:sigil . i, s.'&', 'g')
		endif
		let i += 1
	endw

	if &et " Expand tabs to spaces if 'expandtab' is set.
		return substitute(snippet, '\t', repeat(' ', snipmate#util#tabwidth()), 'g')
	endif
	return snippet
endfunction

" Builds a list of a list of each tab stop in the snippet containing:
" 1.) The tab stop's line number.
" 2.) The tab stop's column number
"     (by getting the length of the string between the last "\n" and the
"     tab stop).
" 3.) The length of the text after the colon for the current tab stop
"     (e.g. "${1:foo}" would return 3).
" 4.) If the "${#:}" construct is given, another list containing all
"     the matches of "$#", to be replaced with the placeholder. This list is
"     composed the same way as the parent; the first item is the line number,
"     and the second is the column.
function! snipmate#legacy#build_stops(snip, lnum, col, indent) abort
	let stops = {}
	let i = 0
	let withoutVars = substitute(a:snip, s:sigil . '\d\+', '', 'g')
	while a:snip =~ s:sigil . '{' . i
		let beforeTabStop = matchstr(withoutVars, '^.*\ze'.s:sigil .'{'.i.'\D')
		let withoutOthers = substitute(withoutVars, ''.s:sigil .'{\('.i.'\D\)\@!\d\+.\{-}}', '', 'g')

		let stops[i] = {}
		let stops[i].line = a:lnum + s:count(beforeTabStop, "\n")
		let stops[i].col = a:indent + len(matchstr(withoutOthers, '[^\n]\{-}\ze'.s:sigil .'{'.i.'\D'))
		let stops[i].placeholder = 0
		let stops[i].mirrors = []
		if stops[i].line == a:lnum
			let stops[i].col += a:col
		endif

		" Get all $# matches in another list, if ${#:name} is given
		if withoutVars =~ printf('%s{%d:', s:sigil, i)
			let stops[i].placeholder = len(matchstr(withoutVars, ''.s:sigil .'{'.i.':\zs.\{-}\ze}'))
			let withoutOthers = substitute(a:snip, ''.s:sigil .'{\d\+.\{-}}\|'.s:sigil .''.i.'\@!\d\+', '', 'g')

			while match(withoutOthers, ''.s:sigil .''.i.'\(\D\|$\)') != -1
				let stops[i].mirrors = get(stops[i], 'mirrors', [])
				let beforeMark = matchstr(withoutOthers,
							\ printf('^.\{-}\ze%s%s%d\(\D\|$\)',
							\ repeat('.', stops[i].placeholder), s:sigil, i))
				let line = a:lnum + s:count(beforeMark, "\n")
				let col = a:indent + (line > a:lnum
				                           \ ? len(matchstr(beforeMark, '.*\n\zs.*'))
				                           \ : a:col + len(beforeMark))
				call add(stops[i].mirrors, { 'line' : line, 'col' : col })
				let withoutOthers = substitute(withoutOthers, ''.s:sigil .''.i.'\ze\(\D\|$\)', '', '')
			endw
		endif
		let i += 1
	endw
	let stops[i] = stops[0]
	return [stops, i + 1]
endfunction

function! s:substitute_visual(snippet, visual) abort
    let lines = []
    for line in split(a:snippet, "\n")
        let indent = matchstr(line, '^\t\+')
        call add(lines, substitute(line, '{VISUAL}',
                    \ substitute(escape(a:visual, '%\'), "\n", "\n" . indent, 'g'), 'g'))
    endfor
    return join(lines, "\n")
endfunction

" Counts occurences of haystack in needle
function! s:count(haystack, needle) abort
	let counter = 0
	let index = stridx(a:haystack, a:needle)
	while index != -1
		let index = stridx(a:haystack, a:needle, index+1)
		let counter += 1
	endw
	return counter
endfunction
