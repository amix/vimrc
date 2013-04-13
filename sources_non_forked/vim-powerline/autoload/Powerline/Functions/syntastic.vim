function! Powerline#Functions#syntastic#GetErrors(line_symbol) " {{{
	if ! exists('g:syntastic_stl_format')
		" Syntastic hasn't been loaded yet
		return ''
	endif

	" Temporarily change syntastic output format
	let old_stl_format = g:syntastic_stl_format
	let g:syntastic_stl_format = '%E{ ERRORS (%e) '. a:line_symbol .' %fe }%W{ WARNINGS (%w) '. a:line_symbol .' %fw }'

	let ret = SyntasticStatuslineFlag()

	let g:syntastic_stl_format = old_stl_format

	return ret
endfunction " }}}
