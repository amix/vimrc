" ==============================================================================
"        File: syntaxFolds.vim
"      Author: Srinath Avadhanula
"              ( srinath@fastmail.fm )
" Last Change: Sun Oct 27 01:00 AM 2002 PST
" Description: Emulation of the syntax folding capability of vim using manual
"              folding
"
" This script provides an emulation of the syntax folding of vim using manual
" folding. Just as in syntax folding, the folds are defined by regions. Each
" region is specified by a call to FoldRegions() which accepts 4 parameters:
"
"    call FoldRegions(startpat, endpat, startoff, endoff)
"
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
" Each time a call is made to FoldRegions(), all the regions (which might be
" disjoint, but not nested) are folded up.
" Nested folds can be created by successive calls to FoldRegions(). The first
" call defines the region which is deepest in the folding. See MakeTexFolds()
" for an idea of how this works for latex files.

" Function: AddSyntaxFoldItem (start, end, startoff, endoff [, skipStart, skipEnd]) {{{
function! AddSyntaxFoldItem(start, end, startoff, endoff, ...)
	if a:0 > 0
		let skipStart = a:1
		let skipEnd = a:2
	else
		let skipStart = ''
		let skipEnd = ''
	end
	if !exists('b:numFoldItems')
		let b:numFoldItems = 0
	end
	let b:numFoldItems = b:numFoldItems + 1

	exe 'let b:startPat_'.b:numFoldItems.' = a:start'
	exe 'let b:endPat_'.b:numFoldItems.' = a:end'
	exe 'let b:startOff_'.b:numFoldItems.' = a:startoff'
	exe 'let b:endOff_'.b:numFoldItems.' = a:endoff'
	exe 'let b:skipStartPat_'.b:numFoldItems.' = skipStart'
	exe 'let b:skipEndPat_'.b:numFoldItems.' = skipEnd'
endfunction 


" }}}
" Function: MakeSyntaxFolds (force) {{{
" Description: This function calls FoldRegions() several times with the
"     parameters specifying various regions resulting in a nested fold
"     structure for the file.
function! MakeSyntaxFolds(force, ...)
	if exists('b:doneFolding') && a:force == 0
		return
	end

	let skipEndPattern = ''
	if a:0 > 0
		let line1 = a:1
		let skipEndPattern = '\|'.a:2
	else
		let line1 = 1
		let r = line('.')
		let c = virtcol('.')
		
		setlocal fdm=manual
		normal! zE
	end
	if !exists('b:numFoldItems')
		b:numFoldItems = 1000000
	end
	
	let i = 1

	let maxline = line('.')

	while exists('b:startPat_'.i) && i <= b:numFoldItems
		exe 'let startPat = b:startPat_'.i
		exe 'let endPat = b:endPat_'.i
		exe 'let startOff = b:startOff_'.i
		exe 'let endOff = b:endOff_'.i
		
		let skipStart = ''
		let skipEnd = ''
		if exists('b:skipStartPat_'.i)
			exe 'let skipStart = b:skipStartPat_'.i
			exe 'let skipEnd = b:skipEndPat_'.i
		end
		exe line1
		let lastLoc = line1

		if skipStart != ''
			call InitStack('BeginSkipArray')
			call FoldRegionsWithSkip(startPat, endPat, startOff, endOff, skipStart, skipEnd, 1, line('$'))
			" call PrintError('done folding ['.startPat.']')
		else
			call FoldRegionsWithNoSkip(startPat, endPat, startOff, endOff, 1, line('$'), '')
		end

		let i = i + 1
	endwhile

	exe maxline
	
	if a:0 == 0
		exe r
		exe "normal! ".c."|"
		if foldlevel(r) > 1
			exe "normal! ".(foldlevel(r) - 1)."zo"
		end
		let b:doneFolding = 0
	end
endfunction


" }}}
" FoldRegionsWithSkip: folding things such as \item's which can be nested. {{{
function! FoldRegionsWithSkip(startpat, endpat, startoff, endoff, startskip, endskip, line1, line2)
	exe a:line1
	" count the regions which have been skipped as we go along. do not want to
	" create a fold which with a beginning or end line in one of the skipped
	" regions.
	let skippedRegions = ''

	" start searching for either the starting pattern or the end pattern.
	while search(a:startskip.'\|'.a:endskip, 'W')
	
		if getline('.') =~ a:endskip

			let lastBegin = Pop('BeginSkipArray')
			" call PrintError('popping '.lastBegin.' from stack and folding till '.line('.'))
			call FoldRegionsWithNoSkip(a:startpat, a:endpat, a:startoff, a:endoff, lastBegin, line('.'), skippedRegions)
			let skippedRegions = skippedRegions.lastBegin.','.line('.').'|'


		" if this is the beginning of a skip region, then, push this line as
		" the beginning of a skipped region.
		elseif getline('.') =~ a:startskip

			" call PrintError('pushing '.line('.').' ['.getline('.').'] into stack')
			call Push('BeginSkipArray', line('.'))

		end
	endwhile

	" call PrintError('with skip starting at '.a:line1.' returning at line# '.line('.'))
endfunction

" }}}
" FoldRegionsWithNoSkip: folding things such as \sections which do not nest. {{{
function! FoldRegionsWithNoSkip(startpat, endpat, startoff, endoff, line1, line2, skippedRegions)
	exe a:line1

	" call PrintError('line1 = '.a:line1.', searching from '.line('.').'... for ['.a:startpat.'')
	let lineBegin = s:MySearch(a:startpat, 'in')
	" call PrintError('... and finding it at '.lineBegin)

	while lineBegin <= a:line2
		if IsInSkippedRegion(lineBegin, a:skippedRegions)
			let lineBegin = s:MySearch(a:startpat, 'out')
			" call PrintError(lineBegin.' is being skipped')
			continue
		end
		let lineEnd = s:MySearch(a:endpat, 'out')
		while IsInSkippedRegion(lineEnd, a:skippedRegions) && lineEnd <= a:line2
			let lineEnd = s:MySearch(a:endpat, 'out')
		endwhile
		if lineEnd > a:line2
			exe (lineBegin + a:startoff).','.a:line2.' fold'
			break
		else
			" call PrintError ('for ['.a:startpat.'] '.(lineBegin + a:startoff).','.(lineEnd + a:endoff).' fold')
			exe (lineBegin + a:startoff).','.(lineEnd + a:endoff).' fold'
		end

		" call PrintError('line1 = '.a:line1.', searching from '.line('.').'... for ['.a:startpat.'')
		let lineBegin = s:MySearch(a:startpat, 'in')
		" call PrintError('... and finding it at '.lineBegin)
	endwhile

	exe a:line2
	return
endfunction

" }}}
" InitStack: initialize a stack {{{
function! InitStack(name)
	exe 'let s:'.a:name.'_numElems = 0'
endfunction
" }}}
" Push: push element into stack {{{
function! Push(name, elem)
	exe 'let numElems = s:'.a:name.'_numElems'
	let numElems = numElems + 1
	exe 'let s:'.a:name.'_Element_'.numElems.' = a:elem'
	exe 'let s:'.a:name.'_numElems = numElems'
endfunction
" }}}
" Pop: pops element off stack {{{
function! Pop(name)
	exe 'let numElems = s:'.a:name.'_numElems'
	if numElems == 0
		return ''
	else
		exe 'let ret = s:'.a:name.'_Element_'.numElems
		let numElems = numElems - 1
		exe 'let s:'.a:name.'_numElems = numElems'
		return ret
	end
endfunction
" }}}
" MySearch: just like search(), but returns large number on failure {{{
function! <SID>MySearch(pat, opt)
	if a:opt == 'in'
		if getline('.') =~ a:pat
			let ret = line('.')
		else
			let ret = search(a:pat, 'W')
		end
	else
		normal! $
		let ret = search(a:pat, 'W')
	end

	if ret == 0
		let ret = line('$') + 1
	end
	return ret
endfunction
" }}}
" Function: IsInSkippedRegion (lnum, regions) {{{
" Description: finds whether a given line number is within one of the regions
"              skipped.
function! IsInSkippedRegion(lnum, regions)
	let i = 1
	let subset = s:Strntok(a:regions, '|', i)
	while subset != ''
		let n1 = s:Strntok(subset, ',', 1)
		let n2 = s:Strntok(subset, ',', 2)
		if a:lnum >= n1 && a:lnum <= n2
			return 1
		end

		let subset = s:Strntok(a:regions, '|', i)
		let i = i + 1
	endwhile

	return 0
endfunction " }}}
" Function: Strntok (string, tok, n) {{{
" extract the n^th token from s seperated by tok.
" example: Strntok('1,23,3', ',', 2) = 23
fun! <SID>Strntok(s, tok, n)
	return matchstr( a:s.a:tok[0], '\v(\zs([^'.a:tok.']*)\ze['.a:tok.']){'.a:n.'}')
endfun " }}}

" vim600:fdm=marker
