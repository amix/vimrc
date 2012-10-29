"            File: tex.vim
"            Type: compiler plugin for LaTeX
" Original Author: Artem Chuprina <ran@ran.pp.ru>
"   Customization: Srinath Avadhanula <srinath@fastmail.fm>
"             CVS: $Id: tex.vim,v 1.6 2003/07/24 02:52:35 srinathava Exp $
" Description:  {{{
"   This file sets the 'makeprg' and 'errorformat' options for the LaTeX
"   compiler. It is customizable to optionally ignore certain warnings and
"   provides the ability to set a dynamic 'ignore-warning' level.
"
"   By default it is set up in a 'non-verbose', 'ignore-common-warnings' mode,
"   which means that irrelevant lines from the compilers output will be
"   ignored and also some very common warnings are ignored.
"   
"   Depending on the 'ignore-level', the following kinds of messages are
"   ignored. An ignore level of 3 for instance means that messages 1-3 will be
"   ignored. By default, the ignore level is set to 4. 
"
"   1. LaTeX Warning: Specifier 'h' changed to 't'. 
"      This errors occurs when TeX is not able to correctly place a floating
"      object at a specified location, because of which it defaulted to the
"      top of the page.
"   2. LaTeX Warning: Underfull box ...
"   3. LaTeX Warning: Overfull box ...
"      both these warnings (very common) are due to \hbox settings not being
"      satisfied nicely.
"   4. LaTeX Warning: You have requested ..., 
"      This warning occurs in slitex when using the xypic package.
"   5. Missing number error:
"      Usually, when the name of an included eps file is spelled incorrectly,
"      then the \bb-error message is accompanied by a bunch of "missing
"      number, treated as zero" error messages. This level ignores these
"      warnings.
"      NOTE: number 5 is actually a latex error, not a warning!
"
"   Use 
"       TCLevel <level>
"   where level is a number to set the ignore level dynamically.
"
"   When TCLevel is called with the unquoted string strict
"      TClevel strict
"    then the 'efm' switches to a 'verbose', 'no-lines-ignored' mode which is
"    useful when you want to make final checks of your document and want to be
"    careful not to let things slip by.
" 
" TIP: MikTeX has a bug where it sometimes erroneously splits a line number
"      into multiple lines. i.e, if the warning is on line 1234. the compiler
"      output is:
"      LaTeX Warning: ... on input line 123
"      4.
"      In this case, vim will wrongly interpret the line-number as 123 instead
"      of 1234. If you have cygwin, a simple remedy around this is to first
"      copy the file vimlatex (provided) into your $PATH, make sure its
"      executable and then set the variable g:tex_flavor to vimlatex in your
"      ~/.vimrc (i.e putting let "g:tex_flavor = 'vimlatex'" in your .vimrc).
"      This problem occurs rarely enough that its not a botheration for most
"      people.
"
" TODO:
"   1. menu items for dynamically selecting a ignore warning level.
" }}}

" avoid reinclusion for the same buffer. keep it buffer local so it can be
" externally reset in case of emergency re-sourcing.
if exists('b:doneTexCompiler') && !exists('b:forceRedoTexCompiler')
	finish
endif
let b:doneTexCompiler = 1

" ==============================================================================
" Customization of 'efm':  {{{
" This section contains the customization variables which the user can set.
" g:Tex_IgnoredWarnings: This variable contains a ¡ seperated list of
" patterns which will be ignored in the TeX compiler's output. Use this
" carefully, otherwise you might end up losing valuable information.
if !exists('g:Tex_IgnoredWarnings')
	let g:Tex_IgnoredWarnings =
		\'Underfull'."\n".
		\'Overfull'."\n".
		\'specifier changed to'."\n".
		\'You have requested'."\n".
		\'Missing number, treated as zero.'."\n".
		\'There were undefined references'."\n".
		\'Citation %.%# undefined'
endif
" This is the number of warnings in the g:Tex_IgnoredWarnings string which
" will be ignored.
if !exists('g:Tex_IgnoreLevel')
	let g:Tex_IgnoreLevel = 7
endif
" There will be lots of stuff in a typical compiler output which will
" completely fall through the 'efm' parsing. This options sets whether or not
" you will be shown those lines.
if !exists('g:Tex_IgnoreUnmatched')
	let g:Tex_IgnoreUnmatched = 1
endif
" With all this customization, there is a slight risk that you might be
" ignoring valid warnings or errors. Therefore before getting the final copy
" of your work, you might want to reset the 'efm' with this variable set to 1.
" With that value, all the lines from the compiler are shown irrespective of
" whether they match the error or warning patterns.
" NOTE: An easier way of resetting the 'efm' to show everything is to do
"       TCLevel strict
if !exists('g:Tex_ShowallLines')
	let g:Tex_ShowallLines = 0
endif

" }}}
" ==============================================================================
" Customization of 'makeprg': {{{

" There are several alternate ways in which 'makeprg' is set up. 
"
" Case 1
" ------
" The first is when this file is a part of latex-suite. In this case, a
" variable called g:Tex_DefaultTargetFormat exists, which gives the default
" format .tex files should be compiled into. In this case, we use the TTarget
" command provided by latex-suite.
"
" Case 2
" ------
" The user is using this file without latex-suite AND he wants to directly
" specify the complete 'makeprg'. Then he should set the g:Tex_CompileRule_dvi
" variable. This is a string which should be directly be able to be cast into
" &makeprg. An example of one such string is:
"
" 	g:Tex_CompileRule_dvi = 'pdflatex \\nonstopmode \\input\{$*\}'
"
" NOTE: You will need to escape back-slashes, {'s etc yourself if you are
"       using this file independently of latex-suite.
" TODO: Should we also have a check for backslash escaping here based on
"       platform?
"
" Case 3
" ------
" The use is using this file without latex-suite and he doesnt want any
" customization. In this case, this file makes some intelligent guesses based
" on the platform. If he doesn't want to specify the complete 'makeprg' but
" only the name of the compiler program (for example 'pdflatex' or 'latex'),
" then he sets b:tex_flavor or g:tex_flavor. 

if exists('g:Tex_DefaultTargetFormat')
	exec 'TTarget '.g:Tex_DefaultTargetFormat
elseif exists('g:Tex_CompileRule_dvi')
	let &l:makeprg = g:Tex_CompileRule_dvi
else
	" If buffer-local variable 'tex_flavor' exists, it defines TeX flavor,
	" otherwize the same for global variable with same name, else it will be LaTeX
	if exists("b:tex_flavor")
		let current_compiler = b:tex_flavor
	elseif exists("g:tex_flavor")
		let current_compiler = g:tex_flavor
	else
		let current_compiler = "latex"
	end
	if has('win32')
		let escChars = ''
	else
		let escChars = '{}\'
	endif
	" Furthermore, if 'win32' is detected, then we want to set the arguments up so
	" that miktex can handle it.
	if has('win32')
		let options = '--src-specials'
	else
		let options = ''
	endif
	let &l:makeprg = current_compiler . ' ' . options .
				\ escape(' \nonstopmode \input{$*}', escChars)
endif

" }}}
" ==============================================================================
" Functions for setting up a customized 'efm' {{{

" IgnoreWarnings: parses g:Tex_IgnoredWarnings for message customization {{{
" Description: 
function! <SID>IgnoreWarnings()
	let i = 1
	while s:Strntok(g:Tex_IgnoredWarnings, "\n", i) != '' &&
				\ i <= g:Tex_IgnoreLevel
		let warningPat = s:Strntok(g:Tex_IgnoredWarnings, "\n", i)
		let warningPat = escape(substitute(warningPat, '[\,]', '%\\\\&', 'g'), ' ')
		exe 'setlocal efm+=%-G%.%#'.warningPat.'%.%#'
		let i = i + 1
	endwhile
endfunction 

" }}}
" SetLatexEfm: sets the 'efm' for the latex compiler {{{
" Description: 
function! <SID>SetLatexEfm()

	let pm = ( g:Tex_ShowallLines == 1 ? '+' : '-' )

	set efm=

	if !g:Tex_ShowallLines
		call s:IgnoreWarnings()
	endif

	setlocal efm+=%E!\ LaTeX\ %trror:\ %m
	setlocal efm+=%E!\ %m

	setlocal efm+=%+WLaTeX\ %.%#Warning:\ %.%#line\ %l%.%#
	setlocal efm+=%+W%.%#\ at\ lines\ %l--%*\\d
	setlocal efm+=%+WLaTeX\ %.%#Warning:\ %m

	exec 'setlocal efm+=%'.pm.'Cl.%l\ %m'
	exec 'setlocal efm+=%'.pm.'C\ \ %m'
	exec 'setlocal efm+=%'.pm.'C%.%#-%.%#'
	exec 'setlocal efm+=%'.pm.'C%.%#[]%.%#'
	exec 'setlocal efm+=%'.pm.'C[]%.%#'
	exec 'setlocal efm+=%'.pm.'C%.%#%[{}\\]%.%#'
	exec 'setlocal efm+=%'.pm.'C<%.%#>%m'
	exec 'setlocal efm+=%'.pm.'C\ \ %m'
	exec 'setlocal efm+=%'.pm.'GSee\ the\ LaTeX%m'
	exec 'setlocal efm+=%'.pm.'GType\ \ H\ <return>%m'
	exec 'setlocal efm+=%'.pm.'G\ ...%.%#'
	exec 'setlocal efm+=%'.pm.'G%.%#\ (C)\ %.%#'
	exec 'setlocal efm+=%'.pm.'G(see\ the\ transcript%.%#)'
	exec 'setlocal efm+=%'.pm.'G\\s%#'
	exec 'setlocal efm+=%'.pm.'O(%*[^()])%r'
	exec 'setlocal efm+=%'.pm.'P(%f%r'
	exec 'setlocal efm+=%'.pm.'P\ %\\=(%f%r'
	exec 'setlocal efm+=%'.pm.'P%*[^()](%f%r'
	exec 'setlocal efm+=%'.pm.'P(%f%*[^()]'
	exec 'setlocal efm+=%'.pm.'P[%\\d%[^()]%#(%f%r'
	if g:Tex_IgnoreUnmatched && !g:Tex_ShowallLines
		setlocal efm+=%-P%*[^()]
	endif
	exec 'setlocal efm+=%'.pm.'Q)%r'
	exec 'setlocal efm+=%'.pm.'Q%*[^()])%r'
	exec 'setlocal efm+=%'.pm.'Q[%\\d%*[^()])%r'
	if g:Tex_IgnoreUnmatched && !g:Tex_ShowallLines
		setlocal efm+=%-Q%*[^()]
	endif
	if g:Tex_IgnoreUnmatched && !g:Tex_ShowallLines
		setlocal efm+=%-G%.%#
	endif

endfunction 

" }}}
" Strntok: extract the n^th token from a list {{{
" example: Strntok('1,23,3', ',', 2) = 23
fun! <SID>Strntok(s, tok, n)
	return matchstr( a:s.a:tok[0], '\v(\zs([^'.a:tok.']*)\ze['.a:tok.']){'.a:n.'}')
endfun

" }}}
" SetTexCompilerLevel: sets the "level" for the latex compiler {{{
function! <SID>SetTexCompilerLevel(...)
	if a:0 > 0
		let level = a:1
	else
		call Tex_ResetIncrementNumber(0)
		echo substitute(g:Tex_IgnoredWarnings, 
			\ '^\|\n\zs\S', '\=Tex_IncrementNumber(1)." ".submatch(0)', 'g')
		let level = input("\nChoose an ignore level: ")
		if level == ''
			return
		endif
	endif
	if level == 'strict'
		let g:Tex_ShowallLines = 1
	elseif level =~ '^\d\+$'
		let g:Tex_ShowallLines = 0
		let g:Tex_IgnoreLevel = level
	else
		echoerr "SetTexCompilerLevel: Unkwown option [".level."]"
	end
	call s:SetLatexEfm()
endfunction 

com! -nargs=? TCLevel :call <SID>SetTexCompilerLevel(<f-args>)
" }}}

" }}}
" ==============================================================================

call s:SetLatexEfm()

" vim: fdm=marker:commentstring=\ \"\ %s
