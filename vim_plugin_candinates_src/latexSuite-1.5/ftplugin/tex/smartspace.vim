"=============================================================================
" 	     File: smartspace.vim
"      Author: Carl Muller
"     Created: Fri Dec 06 12:00 AM 2002 PST
" 
" Description: 
"     Maps the <space> key in insert mode so that mathematical formulaes are
"     always kept on the same line. i.e, $$'s dont get broken across multiple
"     lines.
"=============================================================================

" Avoid reinclusion or if the user doesn't want us.
if exists('b:done_smartspace') 
	\ || (exists('g:Tex_SmartKeySpace') && !g:Tex_SmartKeySpace)
	finish
endif
let b:done_smartspace = 1

" Smart space relies on taking over vim's insertion of carriage returns in
" order to keep $$'s on the same line. The only way to get vim not to break
" lines is to set tw=0. 
"
" NOTE: setting tw != 0 will break smartspace
"       the user's 'tw' setting is still respected in the insert mode.
"       However, normal mode actions which rely on 'tw' such as gqap will be
"       broken because of the faulty 'tw' setting.
let b:tw = &l:tw
setlocal tw=0

inoremap <buffer> <silent> <Space> <Space><Esc>:call <SID>TexFill(b:tw)<CR>a

" Do not redefine the function.
if exists('*s:TexFill')
	finish
endif

" TexFormatLine: format line retaining $$'s on the same line. {{{
function! s:TexFill(width) 
	if a:width != 0 && col(".") > a:width
		" For future use, record the current line and the number of the current column
		let current_line = getline(".")
		let current_column = col(".")
		exe "normal! a##\<Esc>"
		call <SID>TexFormatLine(a:width,current_line,current_column)
		exe "normal! ?##\<CR>2s\<Esc>"
		" Remove ## from the search history.
		call histdel("/", -1)|let @/=histget("/", -1)
	endif
endfunction

" }}}
function! s:TexFormatLine(width, current_line, current_column)    " {{{
	" get the first non-blank character.
	let first = matchstr(getline('.'), '\S')
	normal! $
	let length = col('.')
	let go = 1
	while length > a:width+2 && go
		let between = 0
		let string = strpart(getline('.'), 0, a:width)
		" Count the dollar signs
		let number_of_dollars = 0
		let evendollars = 1
		let counter = 0
		while counter <= a:width-1
			" Pay attention to '$$'.
			if string[counter] == '$' && string[counter-1] != '$'
				let evendollars = 1 - evendollars
				let number_of_dollars = number_of_dollars + 1
			endif
			let counter = counter + 1
		endwhile
		" Get ready to split the line.
		exe 'normal! ' . (a:width + 1) . '|'
		if evendollars
			" Then you are not between dollars.
			exe "normal! ?\\$\\+\\| \<CR>W"
		else
			" Then you are between dollars.
			normal! F$
			if col(".") == 1 || getline('.')[col(".")-1] != "$"
				let go = 0
			endif
		endif
		if first == '$' && number_of_dollars == 1
			let go = 0
		else
			exe "normal! i\<CR>\<Esc>$"
			" get the first non-blank character.
			let first = matchstr(getline('.'), '\S')
		endif
		let length = col(".")
	endwhile
	if go == 0 && strpart(a:current_line, 0, a:current_column) =~ '.*\$.*\$.*'
		exe "normal! ^f$a\<CR>\<Esc>"
		call <SID>TexFormatLine(a:width, a:current_line, a:current_column)
	endif
endfunction

" }}}

" vim:fdm=marker:ts=4:sw=4:noet
