"=============================================================================
" 	     File: folding.vim
"      Author: Srinath Avadhanula
" 	  Version: $Id: folding.vim,v 1.12.2.1 2003/11/25 20:34:54 srinathava Exp $
"     Created: Tue Apr 23 05:00 PM 2002 PST
" 
"  Description: functions to interact with Syntaxfolds.vim
"=============================================================================

nnoremap <unique> <Plug>Tex_RefreshFolds :call MakeTexFolds(1)<cr>

augroup LatexSuite
	au LatexSuite User LatexSuiteFileType 
		\ call Tex_Debug('folding.vim: catching LatexSuiteFileType') | 
		\ call s:SetFoldOptions()
augroup END

" SetFoldOptions: sets maps for every buffer {{{
" Description: 
function! <SID>SetFoldOptions()
	if exists('b:doneSetFoldOptions')
		return
	endif
	let b:doneSetFoldOptions = 1

	setlocal foldtext=TexFoldTextFunction()

	if g:Tex_Folding && g:Tex_AutoFolding
		call MakeTexFolds(0)
	endif

	if g:Tex_Folding && !hasmapto('<Plug>Tex_RefreshFolds')
		nmap <silent> <buffer> <Leader>rf  <Plug>Tex_RefreshFolds
	endif

endfunction " }}}
" MakeTexFolds: function to create fold items for latex. {{{
"
" used in conjunction with MakeSyntaxFolds().
" see ../plugin/syntaxFolds.vim for documentation
"
function! MakeTexFolds(force)
	if exists('g:Tex_Folding') && !g:Tex_Folding
		return
	endif
	if &ft != 'tex'
		return
	end

	" the order in which these calls are made decides the nestedness. in
	" latex, a table environment will always be embedded in either an item or
	" a section etc. not the other way around. so we first fold up all the
	" tables. and then proceed with the other regions.

	let b:numFoldItems = 0

	" ========================================================================
	" How to add new folding items {{{
	" ========================================================================
	"
	" Each of the following function calls defines a syntax fold region. Each
	" definition consists of a call to the AddSyntaxFoldItem() function.
	" 
	" The order in which the folds are defined is important. Juggling the
	" order of the function calls will create havoc with folding. The
	" "deepest" folding item needs to be called first. For example, if
	" the \begin{table} environment is a subset (or lies within) the \section
	" environment, then add the definition for the \table first.
	"
	" The AddSyntaxFoldItem() function takes either 4 or 6 arguments. When it
	" is called with 4 arguments, it is equivalent to calling it with 6
	" arguments with the last two left blank (i.e as empty strings)
	"
	" The explanation for each argument is as follows:
	"    startpat: a line matching this pattern defines the beginning of a fold.
	"    endpat  : a line matching this pattern defines the end of a fold.
	"    startoff: this is the offset from the starting line at which folding will
	"              actually start
	"    endoff  : like startoff, but gives the offset of the actual fold end from
	"              the line satisfying endpat.
	"              startoff and endoff are necessary when the folding region does
	"              not have a specific end pattern corresponding to a start
	"              pattern. for example in latex,
	"              \begin{section}
	"              defines the beginning of a section, but its not necessary to
	"              have a corresponding
	"              \end{section}
	"              the section is assumed to end 1 line _before_ another section
	"              starts.
	"    startskip: a pattern which defines the beginning of a "skipped" region.
	"
	"               For example, suppose we define a \itemize fold as follows:
	"               startpat =  '^\s*\\item',
	"               endpat = '^\s*\\item\|^\s*\\end{\(enumerate\|itemize\|description\)}',
	"               startoff = 0,
	"               endoff = -1
	"
	"               This defines a fold which starts with a line beginning with an
	"               \item and ending one line before a line beginning with an
	"               \item or \end{enumerate} etc.
	"
	"               Then, as long as \item's are not nested things are fine.
	"               However, once items begin to nest, the fold started by one
	"               \item can end because of an \item in an \itemize
	"               environment within this \item. i.e, the following can happen:
	"
	"               \begin{itemize}
	"               \item Some text <------- fold will start here
	"                     This item will contain a nested item
	"                     \begin{itemize} <----- fold will end here because next line contains \item...
	"                     \item Hello
	"                     \end{itemize} <----- ... instead of here.
	"               \item Next item of the parent itemize
	"               \end{itemize}
	"
	"               Therefore, in order to completely define a folding item which
	"               allows nesting, we need to also define a "skip" pattern.
	"               startskip and end skip do that.
	"               Leave '' when there is no nesting.
	"    endskip: the pattern which defines the end of the "skip" pattern for
	"             nested folds.
	"
	"    Example: 
	"    1. A syntax fold region for a latex section is
	"           startpat = "\\section{"
	"           endpat   = "\\section{"
	"           startoff = 0
	"           endoff   = -1
	"           startskip = ''
	"           endskip = ''
	"    Note that the start and end patterns are thus the same and endoff has a
	"    negative value to capture the effect of a section ending one line before
	"    the next starts.
	"    2. A syntax fold region for the \itemize environment is:
	"           startpat = '^\s*\\item',
	"           endpat = '^\s*\\item\|^\s*\\end{\(enumerate\|itemize\|description\)}',
	"           startoff = 0,
	"           endoff = -1,
	"           startskip = '^\s*\\begin{\(enumerate\|itemize\|description\)}',
	"           endskip = '^\s*\\end{\(enumerate\|itemize\|description\)}'
	"     Note the use of startskip and endskip to allow nesting.
	"
	"
	" }}}
	" ========================================================================
	" {{{ footnote
	call AddSyntaxFoldItem (
		\ '^\s*\\footnote{',
		\ '^\s*}',
		\ 0,
		\ 0
		\ )
	" }}}
	" {{{ intertext
	call AddSyntaxFoldItem (
		\ '^\s*\\intertext{',
		\ '^\s*}',
		\ 0,
		\ 0
		\ )
	" }}}
	" {{{ abstract
	call AddSyntaxFoldItem (
		\ '^\s*\\begin{abstract}',
		\ '^\s*\\end{abstract}',
		\ 0,
		\ 0
		\ )
	" }}}
	" {{{ keywords
	call AddSyntaxFoldItem (
		\ '^\s*\\begin{keywords}',
		\ '^\s*\\end{keywords}',
		\ 0,
		\ 0
		\ )
	" }}}
	" {{{ thebibliography
	call AddSyntaxFoldItem (
		\ '^\s*\\begin{thebibliography}',
		\ '^\s*\\end{thebibliography}',
		\ 0,
		\ 0
		\ )
	" }}}
	" {{{ table
	call AddSyntaxFoldItem (
		\ '^\s*\\begin{table}',
		\ '^\s*\\end{table}',
		\ 0,
		\ 0
		\ )
	" }}}
	" {{{ figure
	call AddSyntaxFoldItem (
		\ '^\s*\\begin{figure',
		\ '^\s*\\end{figure}',
		\ 0,
		\ 0
		\ )
	" }}}
	" {{{ align/alignat
	call AddSyntaxFoldItem (
		\ '^\s*\\begin{align',
		\ '^\s*\\end{align',
		\ 0,
		\ 0
		\ )
	" }}}
	" {{{ gather
	call AddSyntaxFoldItem (
		\ '^\s*\\begin{gather',
		\ '^\s*\\end{gather',
		\ 0,
		\ 0
		\ )
	" }}}
	" {{{ equation/eqnarray
	call AddSyntaxFoldItem (
		\ '^\s*\\begin{eq',
		\ '^\s*\\end{eq',
		\ 0,
		\ 0
		\ )
	" }}}
	" {{{ items
	call AddSyntaxFoldItem (
		\ '^\s*\\item',
		\ '^\s*\\item\|^\s*\\end{\(enumerate\|itemize\|description\)}',
		\ 0,
		\ -1,
		\ '^\s*\\begin{\(enumerate\|itemize\|description\)}',
		\ '^\s*\\end{\(enumerate\|itemize\|description\)}'
		\ )
	" }}}
	" {{{ subsubsection
	call AddSyntaxFoldItem (
		\ '^\s*\\subsubsection\W',
		\ '^\s*\\appendix\W\|^\s*\\subsubsection\W\|^\s*\\subsection\W\|^\s*\\section\W\|^\s*%%fakesection\|^\s*\\chapter\W\|^\s*\\begin{slide\|^\s*\\end{document',
		\ 0,
		\ -1,
		\ )
	" }}}
	" {{{ subsection
	call AddSyntaxFoldItem (
		\ '^\s*\\subsection\W',
		\ '^\s*\\appendix\W\|^\s*\\subsection\W\|^\s*\\section\W\|^\s*%%fakesection\|^\s*\\bibliography\|^\s*\\chapter\W\|^\s*\\begin{slide\|^\s*\\begin{thebibliography\|^\s*\\end{document',
		\ 0,
		\ -1,
		\ )
	" }}}
	" {{{ section
	call AddSyntaxFoldItem (
		\ '^\s*\\section\W',
		\ '^\s*\\appendix\W\|^\s*\\section\W\|^\s*\\bibliography\|^\s*%%fakesection\|^\s*\\chapter\W\|^\s*\\begin{slide\|^\s*\\begin{thebibliography\|^\s*\\end{document',
		\ 0,
		\ -1,
		\ )
	" }}}
	" {{{ fakesection (for forcing a fold item manually)
	call AddSyntaxFoldItem (
		\ '^\s*%%fakesection',
		\ '^\s*\\appendix\W\|^\s*\\section\W\|^\s*%%fakesection\|^\s*\\bibliography\|^\s*\\chapter\W\|^\s*\\begin{slide\|^\s*\\begin{thebibliography\|^\s*\\end{document',
		\ 0,
		\ -1,
		\ )
	" }}}
	" {{{ chapter
	call AddSyntaxFoldItem(
		\ '^\s*\\chapter\W',
		\ '^\s*\\appendix\W\|^\s*\\chapter\W\|^\s*\\bibliography\|^\s*\\begin{slide\|^\s*\\begin{thebibliography\|^\s*\\end{document',
		\ 0,
		\ -1
		\ )
	" }}}
	" {{{ slide
	call AddSyntaxFoldItem (
		\ '^\s*\\begin{slide',
		\ '^\s*\\appendix\W\|^\s*\\chapter\W\|^\s*\\end{slide\|^\s*\\end{document',
		\ 0,
		\ 0
		\ )
	" }}}

	call MakeSyntaxFolds(a:force)
	normal! zv
endfunction

" }}}
" TexFoldTextFunction: create fold text for folds {{{
function! TexFoldTextFunction()
	if getline(v:foldstart) =~ '^\s*\\begin{'
		let header = matchstr(getline(v:foldstart), '^\s*\\begin{\zs\(figure\|sidewaysfigure\|table\|equation\|eqnarray\|gather\|align\|abstract\|keywords\|thebibliography\)[^}]*\ze}')

		let caption = ''
		let label = ''
		let i = v:foldstart
		while i <= v:foldend
			if getline(i) =~ '\\caption'
				let caption = matchstr(getline(i), '\\caption{\zs.*')
				let caption = substitute(caption, '\zs}[^}]*$', '', '')
			elseif getline(i) =~ '\\label'
				let label = matchstr(getline(i), '\\label{\zs.*')
				let label = substitute(label, '\zs}[^}]*$', '', '')
			end

			let i = i + 1
		endwhile

		let ftxto = foldtext()
		" if no caption found, then use the second line.
		if caption == ''
			let caption = getline(v:foldstart + 1)
		end

		let retText = matchstr(ftxto, '^[^:]*').': '.header.' ('.label.') : '.caption
		return retText
	else
		return foldtext()
	end
endfunction
" }}}

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
