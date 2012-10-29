" ==============================================================================
" Author: Carl Mueller
" 		  (incorporated into latex-suite by Srinath Avadhanula)
" Last Change: Tue Dec 31 11:00 AM 2002 PST
" Description:
" 	This ftplugin provides the following maps:
" . <M-b> encloses the previous character in \mathbf{}
" . <M-c> is polymorphic as follows:
"     Insert mode:
"     1. If the previous character is a letter or number, then capitalize it and
"        enclose it in \mathcal{}
"     2. otherwise insert \cite{}
"     Visual Mode:
"     1. Enclose selection in \mathcal{}
" . <M-l> is also polymorphic as follows:
"     If the character before typing <M-l> is one of '([{|<q', then do the
"     following:
"       1. (<M-l>       \left(\right
"               similarly for [, |
"          {<M-l>       \left\{\right\}
"       2. <<M-l>       \langle\rangle
"       3. q<M-l>       \lefteqn{}
"     otherwise insert  \label{}
" 
" These functions make it extremeley easy to do all the \left \right stuff in
" latex.
"
" NOTE: The insert mode maps are created only if maps are no maps already to
" the relevant functions Tex_MathBF, Tex_MathCal and Tex_LeftRight. This is to
" enable people who might need the alt keys for typing to use some other
" keypress to trigger the same behavior. In order to use some other key, (say
" <C-c>) to use Tex_MathCal(), do the following
"
" 	inoremap <buffer> <silent> <C-c> <C-r>=Tex_MathCal()<CR>
"
" ============================================================================== 
" Avoid reinclusion.
if exists('b:did_brackets')
	finish
endif
let b:did_brackets = 1

" ==============================================================================
" Insert mode mappings
" All the insert mode mappings check to see if the function they are creating
" the map for already exists in the rhs of some map.
" ============================================================================== 
" {{{

" Provide <plug>'d mapping for easy user customization.
"
inoremap <silent> <Plug>Tex_MathBF      <C-r>=Tex_MathBF()<CR>
inoremap <silent> <Plug>Tex_MathCal     <C-r>=Tex_MathCal()<CR>
inoremap <silent> <Plug>Tex_LeftRight   <C-r>=Tex_LeftRight()<CR>

" Provide mappings only if the user hasn't provided a map already or if the
" target lhs doesn't have a mapping.
" TODO: These will be removed in future revisions. Alt mappings are a headache
"       for European users...
if !hasmapto('<Plug>Tex_MathBF', 'i') && mapcheck('<M-b>', 'i') == ''
	imap <buffer> <silent> <M-b>        <Plug>Tex_MathBF
endif
if !hasmapto('<Plug>Tex_MathCal', 'i') && mapcheck('<M-c>', 'i') == ''
	imap <buffer> <silent> <M-c>        <Plug>Tex_MathCal
endif
if !hasmapto('<Plug>Tex_LeftRight', 'i') && mapcheck('<M-l>', 'i') == ''
	imap <buffer> <silent> <M-l>        <Plug>Tex_LeftRight
endif

" }}}

" ==============================================================================
" Visual/Normal Mode mappings.
" ==============================================================================
" {{{

vnoremap <buffer> <silent> <M-b> <C-C>`>a}<Esc>`<i\mathbf{<Esc>
vnoremap <buffer> <silent> <M-c> <C-C>`>a}<Esc>`<i\mathcal{<Esc>
nnoremap <buffer> <silent> <M-l> :call <SID>PutLeftRight()<CR>

" }}}

" ==============================================================================
" Function definitions
" ============================================================================== 
" define the funtions only once.
if exists('*Tex_MathBF')
	finish
endif
" Tex_MathBF: encloses te previous letter/number in \mathbf{} {{{
" Description: 
function! Tex_MathBF()
	return "\<Left>\\mathbf{\<Right>}\<Esc>hvUla"
endfunction " }}}
" Tex_MathCal: enclose the previous letter/number in \mathcal {{{
" Description:
" 	if the last character is not a letter/number, then insert \cite{}
function! Tex_MathCal()
	let line = getline(line("."))
	let char = line[col(".")-2]

	if char =~ '[a-zA-Z0-9]'
		return "\<BS>".'\mathcal{'.toupper(char).'}'
	else
		return IMAP_PutTextWithMovement('\cite{<++>}<++>')
	endif
endfunction
" }}}
" Tex_LeftRight: maps <M-l> in insert mode. {{{
" Description:
" This is a polymorphic function, which maps the behaviour of <M-l> in the
" following way:
" If the character before typing <M-l> is one of '([{|<q', then do the
" following:
" 	1. (<M-l>		\left(<++>\right<++>
" 	    	similarly for [, |
" 	   {<M-l>		\left\{<++>\right\}<++>
" 	2. <<M-l>		\langle<++>\rangle<++>
" 	3. q<M-l>		\lefteqn{<++>}<++>
" otherwise insert  \label{<++>}<++>
function! Tex_LeftRight()
	let line = getline(line("."))
	let char = line[col(".")-2]
	let previous = line[col(".")-3]

	let matchedbrackets = '()[]{}||'
	if char =~ '(\|\[\|{\||'
		let add = ''
		if char =~ '{'
			let add = "\\"
		endif
		let rhs = matchstr(matchedbrackets, char.'\zs.\ze')
		return "\<BS>".IMAP_PutTextWithMovement('\left'.add.char.'<++>\right'.add.rhs.'<++>')
	elseif char == '<'
		return "\<BS>".IMAP_PutTextWithMovement('langle<++>\rangle<++>')
	elseif char == 'q'
		return "\<BS>".IMAP_PutTextWithMovement('\lefteqn{<++>}<++>')
	else
		return IMAP_PutTextWithMovement('\label{<++>}<++>')
	endif
endfunction " }}}
" Tex_PutLeftRight: maps <M-l> in normal mode {{{
" Description:
" Put \left...\right in front of the matched brackets.
function! Tex_PutLeftRight()
	let previous = getline(line("."))[col(".") - 2]
	let char = getline(line("."))[col(".") - 1]
	if previous == '\'
		if char == '{'
			exe "normal ileft\\\<Esc>l%iright\\\<Esc>l%"
		elseif char == '}'
			exe "normal iright\\\<Esc>l%ileft\\\<Esc>l%"
		endif
	elseif char =~ '\[\|('
		exe "normal i\\left\<Esc>l%i\\right\<Esc>l%"
	elseif char =~ '\]\|)'
		exe "normal i\\right\<Esc>l%i\\left\<Esc>l%"
	endif
endfunction " }}}

" vim:fdm=marker
