" LaTeX filetype
"	  Language: LaTeX (ft=tex)
"	Maintainer: Srinath Avadhanula
"		 Email: srinath@fastmail.fm
"		   CVS: $Id: main.vim,v 1.47 2003/09/13 07:03:13 srinathava Exp $
"		   URL: 

" line continuation used here.
let s:save_cpo = &cpo
set cpo&vim

" avoiding re-inclusion {{{
" the avoiding re-inclusion statement is not provided here because the files
" which call this file should in the normal course of events handle the
" re-inclusion stuff.

" we definitely dont want to run through the entire file each and every time.
" only once to define the functions. for successive latex files, just set up
" the folding and mappings and quit.
if exists('s:doneFunctionDefinitions') && !exists('b:forceRedoLocalTex')
	call s:SetTeXOptions()
	finish
endif

let s:doneFunctionDefinitions = 1

" get the place where this plugin resides for setting cpt and dict options.
" these lines need to be outside the function.
let s:path = expand('<sfile>:p:h')
" set up personal defaults.
runtime ftplugin/tex/texrc
" set up global defaults.
exe "so ".s:path.'/texrc'

" }}}

nmap <silent> <script> <plug> i
imap <silent> <script> <C-o><plug> <Nop>

" ==============================================================================
" mappings
" ==============================================================================
" {{{
" calculate the mapleader character.
let s:ml = exists('g:mapleader') ? g:mapleader : '\'

if !exists('s:doneMappings')
	let s:doneMappings = 1
	" short forms for latex formatting and math elements. {{{
	" taken from auctex.vim or miktexmacros.vim
	call IMAP ('__', '_{<++>}<++>', "tex")
	call IMAP ('()', '(<++>)<++>', "tex")
	call IMAP ('[]', '[<++>]<++>', "tex")
	call IMAP ('{}', '{<++>}<++>', "tex")
	call IMAP ('^^', '^{<++>}<++>', "tex")
	call IMAP ('$$', '$<++>$<++>', "tex")
	call IMAP ('==', '&=& ', "tex")
	call IMAP ('~~', '&\approx& ', "tex")
	call IMAP ('=~', '\approx', "tex")
	call IMAP ('::', '\dots', "tex")
	call IMAP ('((', '\left( <++> \right)<++>', "tex")
	call IMAP ('[[', '\left[ <++> \right]<++>', "tex")
	call IMAP ('{{', '\left\{ <++> \right\}<++>', "tex")
	call IMAP (g:Tex_Leader.'^', '\hat{<++>}<++>', "tex")
	call IMAP (g:Tex_Leader.'_', '\bar{<++>}<++>', "tex")
	call IMAP (g:Tex_Leader.'6', '\partial', "tex")
	call IMAP (g:Tex_Leader.'8', '\infty', "tex")
	call IMAP (g:Tex_Leader.'/', '\frac{<++>}{<++>}<++>', "tex")
	call IMAP (g:Tex_Leader.'%', '\frac{<++>}{<++>}<++>', "tex")
	call IMAP (g:Tex_Leader.'@', '\circ', "tex")
	call IMAP (g:Tex_Leader.'0', '^\circ', "tex")
	call IMAP (g:Tex_Leader.'=', '\equiv', "tex")
	call IMAP (g:Tex_Leader."\\",'\setminus', "tex")
	call IMAP (g:Tex_Leader.'.', '\cdot', "tex")
	call IMAP (g:Tex_Leader.'*', '\times', "tex")
	call IMAP (g:Tex_Leader.'&', '\wedge', "tex")
	call IMAP (g:Tex_Leader.'-', '\bigcap', "tex")
	call IMAP (g:Tex_Leader.'+', '\bigcup', "tex")
	call IMAP (g:Tex_Leader.'M', '\sum_{<++>}^{<++>}<++>', 'tex')
	call IMAP (g:Tex_Leader.'S', '\sum_{<++>}^{<++>}<++>', 'tex')
	call IMAP (g:Tex_Leader.'(', '\subset', "tex")
	call IMAP (g:Tex_Leader.')', '\supset', "tex")
	call IMAP (g:Tex_Leader.'<', '\le', "tex")
	call IMAP (g:Tex_Leader.'>', '\ge', "tex")
	call IMAP (g:Tex_Leader.',', '\nonumber', "tex")
	call IMAP (g:Tex_Leader.'~', '\tilde{<++>}<++>', "tex")
	call IMAP (g:Tex_Leader.';', '\dot{<++>}<++>', "tex")
	call IMAP (g:Tex_Leader.':', '\ddot{<++>}<++>', "tex")
	call IMAP (g:Tex_Leader.'2', '\sqrt{<++>}<++>', "tex")
	call IMAP (g:Tex_Leader.'|', '\Big|', "tex")
	call IMAP (g:Tex_Leader.'I', "\\int_{<++>}^{<++>}<++>", 'tex')
	" }}}
	" Greek Letters {{{
	call IMAP(g:Tex_Leader.'a', '\alpha', 'tex')
	call IMAP(g:Tex_Leader.'b', '\beta', 'tex')
	call IMAP(g:Tex_Leader.'c', '\chi', 'tex')
	call IMAP(g:Tex_Leader.'d', '\delta', 'tex')
	call IMAP(g:Tex_Leader.'e', '\varepsilon', 'tex')
	call IMAP(g:Tex_Leader.'f', '\varphi', 'tex')
	call IMAP(g:Tex_Leader.'g', '\gamma', 'tex')
	call IMAP(g:Tex_Leader.'h', '\eta', 'tex')
	call IMAP(g:Tex_Leader.'k', '\kappa', 'tex')
	call IMAP(g:Tex_Leader.'l', '\lambda', 'tex')
	call IMAP(g:Tex_Leader.'m', '\mu', 'tex')
	call IMAP(g:Tex_Leader.'n', '\nu', 'tex')
	call IMAP(g:Tex_Leader.'p', '\pi', 'tex')
	call IMAP(g:Tex_Leader.'q', '\theta', 'tex')
	call IMAP(g:Tex_Leader.'r', '\rho', 'tex')
	call IMAP(g:Tex_Leader.'s', '\sigma', 'tex')
	call IMAP(g:Tex_Leader.'t', '\tau', 'tex')
	call IMAP(g:Tex_Leader.'u', '\upsilon', 'tex')
	call IMAP(g:Tex_Leader.'v', '\varsigma', 'tex')
	call IMAP(g:Tex_Leader.'w', '\omega', 'tex')
	call IMAP(g:Tex_Leader.'w', '\wedge', 'tex')  " AUCTEX style
	call IMAP(g:Tex_Leader.'x', '\xi', 'tex')
	call IMAP(g:Tex_Leader.'y', '\psi', 'tex')
	call IMAP(g:Tex_Leader.'z', '\zeta', 'tex')
	" not all capital greek letters exist in LaTeX!
	" reference: http://www.giss.nasa.gov/latex/ltx-405.html
	call IMAP(g:Tex_Leader.'D', '\Delta', 'tex')
	call IMAP(g:Tex_Leader.'F', '\Phi', 'tex')
	call IMAP(g:Tex_Leader.'G', '\Gamma', 'tex')
	call IMAP(g:Tex_Leader.'Q', '\Theta', 'tex')
	call IMAP(g:Tex_Leader.'L', '\Lambda', 'tex')
	call IMAP(g:Tex_Leader.'X', '\Xi', 'tex')
	call IMAP(g:Tex_Leader.'Y', '\Psi', 'tex')
	call IMAP(g:Tex_Leader.'S', '\Sigma', 'tex')
	call IMAP(g:Tex_Leader.'U', '\Upsilon', 'tex')
	call IMAP(g:Tex_Leader.'W', '\Omega', 'tex')
	" }}}
	" ProtectLetters: sets up indentity maps for things like ``a {{{
	" " Description: If we simply do
	" 		call IMAP('`a', '\alpha', 'tex')
	" then we will never be able to type 'a' after a tex-quotation. Since
	" IMAP() always uses the longest map ending in the letter, this problem
	" can be avoided by creating a fake map for ``a -> ``a.
	" This function sets up fake maps of the following forms:
	" 	``[aA]  -> ``[aA]    (for writing in quotations)
	" 	\`[aA]  -> \`[aA]    (for writing diacritics)
	" 	"`[aA]  -> "`[aA]    (for writing german quotations)
	" It does this for all printable lower ascii characters just to make sure
	" we dont let anything slip by.
	function! s:ProtectLetters(first, last)
		let i = a:first
		while i <= a:last
			if nr2char(i) =~ '[[:print:]]'
				call IMAP('``'.nr2char(i), '``'.nr2char(i), 'tex')
				call IMAP('\`'.nr2char(i), '\`'.nr2char(i), 'tex')
				call IMAP('"`'.nr2char(i), '"`'.nr2char(i), 'tex')
			endif
			let i = i + 1
		endwhile
	endfunction 
	call s:ProtectLetters(32, 127)
	" }}}
	" vmaps: enclose selected region in brackets, environments {{{ 
	" The action changes depending on whether the selection is character-wise
	" or line wise. for example, selecting linewise and pressing \v will
	" result in the region being enclosed in \begin{verbatim}, \end{verbatim},
	" whereas in characterise visual mode, the thingie is enclosed in \verb|
	" and |.
	exec 'vnoremap <silent> '.g:Tex_Leader."( \<C-\\>\<C-N>:call VEnclose('\\left( ', ' \\right)', '\\left(', '\\right)')\<CR>"
	exec 'vnoremap <silent> '.g:Tex_Leader."[ \<C-\\>\<C-N>:call VEnclose('\\left[ ', ' \\right]', '\\left[', '\\right]')\<CR>"
	exec 'vnoremap <silent> '.g:Tex_Leader."{ \<C-\\>\<C-N>:call VEnclose('\\left\\{ ', ' \\right\\}', '\\left\\{', '\\right\\}')\<CR>"
	exec 'vnoremap <silent> '.g:Tex_Leader."$ \<C-\\>\<C-N>:call VEnclose('$', '$', '\\[', '\\]')\<CR>"
	" }}}
end

" }}}

" ==============================================================================
" Smart key-mappings
" ============================================================================== 
" TexQuotes: inserts `` or '' instead of " {{{
if g:Tex_SmartKeyQuote

	" TexQuotes: inserts `` or '' instead of "
	" Taken from texmacro.vim by Benji Fisher <benji@e-math.AMS.org>
	" TODO:  Deal with nested quotes.
	" The :imap that calls this function should insert a ", move the cursor to
	" the left of that character, then call this with <C-R>= .
	function! s:TexQuotes()
		let l = line(".")
		let c = col(".")
		let restore_cursor = l . "G" . virtcol(".") . "|"
		normal! H
		let restore_cursor = "normal!" . line(".") . "Gzt" . restore_cursor
		execute restore_cursor
		" In math mode, or when preceded by a \, just move the cursor past the
		" already-inserted " character.
		if synIDattr(synID(l, c, 1), "name") =~ "^texMath"
			\ || (c > 1 && getline(l)[c-2] == '\')
			return "\<Right>"
		endif
		" Find the appropriate open-quote and close-quote strings.
		if exists("b:Tex_SmartQuoteOpen")
			let open = b:Tex_SmartQuoteOpen
		elseif exists("g:Tex_SmartQuoteOpen")
			let open = g:Tex_SmartQuoteOpen
		else
			let open = "``"
		endif
		if exists("b:Tex_SmartQuoteClose")
			let close = b:Tex_SmartQuoteClose
		elseif exists("g:Tex_SmartQuoteClose")
			let close = g:Tex_SmartQuoteClose
		else
			let close = "''"
		endif
		let boundary = '\|'
		" This code seems to be obsolete, since this script variable is never
		" set.  The idea is that some languages use ",," as an open- or
		" close-quote string, and we want to avoid confusing ordinary ","
		" with a quote boundary.
		if exists("s:TeX_strictquote")
			if( s:TeX_strictquote == "open" || s:TeX_strictquote == "both" )
				let boundary = '\<' . boundary
			endif
			if( s:TeX_strictquote == "close" || s:TeX_strictquote == "both" )
				let boundary = boundary . '\>'
			endif
		endif
		" Eventually return q; set it to the default value now.
		let q = open
		while 1	" Look for preceding quote (open or close), ignoring
			" math mode and '\"' .
			call search(escape(open . boundary . close . '\|^$\|"', "~"), "bw")
			if synIDattr(synID(line("."), col("."), 1), "name") !~ "^texMath"
				\ && (col(".") == 1 || getline(".")[col(".")-2] != '\')
				break
			endif
		endwhile
		" Now, test whether we actually found a _preceding_ quote; if so, is it
		" an open quote?
		if ( line(".") < l || line(".") == l && col(".") < c )
			if strpart(getline("."), col(".")-1) =~
						\ '\V\^' . escape(open, '\')
				if line(".") == l && col(".") + strlen(open) == c
					" Insert "<++>''<++>" instead of just "''".
					let q = IMAP_PutTextWithMovement("<++>".close."<++>")
				else
					let q = close
				endif
			endif
		endif
		" Return to line l, column c:
		execute restore_cursor
		" Start with <Del> to remove the " put in by the :imap .
		return "\<Del>" . q
	endfunction

endif
" }}}
" SmartBS: smart backspacing {{{
if g:Tex_SmartKeyBS 

	" SmartBS: smart backspacing
	" SmartBS lets you treat diacritic characters (those \'{a} thingies) as a
	" single character. This is useful for example in the following situation:
	"
	" \v{s}\v{t}astn\'{y}    ('happy' in Slovak language :-) )
	" If you will delete this normally (without using smartBS() function), you
	" must press <BS> about 19x. With function smartBS() you must press <BS> only
	" 7x. Strings like "\v{s}", "\'{y}" are considered like one character and are
	" deleted with one <BS>.
	"
	let s:smartBS_pat = '\(' .
		\ "\\\\[\"^'=v]{\\S}"      . '\|' .
		\ "\\\\[\"^'=]\\S"         . '\|' .
		\ '\\v \S'                 . '\|' .
		\ "\\\\[\"^'=v]{\\\\[iI]}" . '\|' .
		\ '\\v \\[iI]'             . '\|' .
		\ '\\q \S'                 . '\|' .
		\ '\\-'                    .
		\ '\)' . "$"
	fun! s:SmartBS_pat()
		return s:smartBS_pat
	endfun

	" This function comes from Benji Fisher <benji@e-math.AMS.org>
	" http://vim.sourceforge.net/scripts/download.php?src_id=409 
	" (modified/patched by Lubomir Host 'rajo' <host8 AT keplerDOTfmphDOTuniba.sk>)
	function! s:SmartBS(pat)
		let init = strpart(getline("."), 0, col(".")-1)
		let matchtxt = matchstr(init, a:pat)
		if matchtxt != ''
			let bstxt = substitute(matchtxt, '.', "\<bs>", 'g')
			return bstxt
		else
			return "\<bs>"
		endif
	endfun
	
endif " }}}
" SmartDots: inserts \cdots instead of ... in math mode otherwise \ldots {{{
" if amsmath package is detected then just use \dots and let amsmath take care
" of it.
if g:Tex_SmartKeyDot

	function! <SID>SmartDots()
		if strpart(getline('.'), col('.')-3, 2) == '..' && 
			\ g:Tex_package_detected =~ '\<amsmath\>'
			return "\<bs>\<bs>\\dots"
		elseif synIDattr(synID(line('.'),col('.')-1,0),"name") =~ '^texMath'
			\&& strpart(getline('.'), col('.')-3, 2) == '..' 
			return "\<bs>\<bs>\\cdots"
		elseif strpart(getline('.'), col('.')-3, 2) == '..' 
			return "\<bs>\<bs>\\ldots"
		else
			return '.'
		endif
	endfunction 

endif
" }}}

" ==============================================================================
" Helper Functions
" ============================================================================== 
" Tex_ShowVariableValue: debugging help {{{
" provides a way to examine script local variables from outside the script.
" very handy for debugging.
function! Tex_ShowVariableValue(...)
	let i = 1
	while i <= a:0
		exe 'let arg = a:'.i
		if exists('s:'.arg) ||
		\  exists('*s:'.arg)
			exe 'let val = s:'.arg
			echomsg 's:'.arg.' = '.val
		end
		let i = i + 1
	endwhile
endfunction

" }}}
" Tex_Strntok: extract the n^th token from a list {{{
" example: Strntok('1,23,3', ',', 2) = 23
fun! Tex_Strntok(s, tok, n)
	return matchstr( a:s.a:tok[0], '\v(\zs([^'.a:tok.']*)\ze['.a:tok.']){'.a:n.'}')
endfun

" }}}
" Tex_CreatePrompt: creates a prompt string {{{
" Description: 
" Arguments:
"     promptList: This is a string of the form:
"         'item1,item2,item3,item4'
"     cols: the number of columns in the resultant prompt
"     sep: the list seperator token
"
" Example:
" Tex_CreatePrompt('item1,item2,item3,item4', 2, ',')
" returns
" "(1) item1\t(2)item2\n(3)item3\t(4)item4"
"
" This string can be used in the input() function.
function! Tex_CreatePrompt(promptList, cols, sep)

	let g:listSep = a:sep
	let num_common = GetListCount(a:promptList)

	let i = 1
	let promptStr = ""

	while i <= num_common

		let j = 0
		while j < a:cols && i + j <= num_common
			let com = Tex_Strntok(a:promptList, a:sep, i+j)
			let promptStr = promptStr.'('.(i+j).') '. 
						\ com."\t".( strlen(com) < 4 ? "\t" : '' )

			let j = j + 1
		endwhile

		let promptStr = promptStr."\n"

		let i = i + a:cols
	endwhile
	return promptStr
endfunction 

" }}}
" Tex_CleanSearchHistory: removes last search item from search history {{{
" Description: This function needs to be globally visible because its
"              called from outside the script during expansion.
function! Tex_CleanSearchHistory()
  call histdel("/", -1)
  let @/ = histget("/", -1)
endfunction
nmap <silent> <script> <plug>cleanHistory :call Tex_CleanSearchHistory()<CR>

" }}}
" Tex_GetVarValue: gets the value of the variable {{{
" Description: 
" 	See if a window-local, buffer-local or global variable with the given name
" 	exists and if so, returns the corresponding value. Otherwise return the
" 	provided default value.
function! Tex_GetVarValue(varname, default)
	if exists('w:'.a:varname)
		return w:{a:varname}
	elseif exists('b:'.a:varname)
		return b:{a:varname}
	elseif exists('g:'.a:varname)
		return g:{a:varname}
	else
		return a:default
	endif
endfunction " }}}
" Tex_GetMainFileName: gets the name (without extension) of the main file being compiled. {{{
" Description:  returns '' if .latexmain doesnt exist.
"               i.e if main.tex.latexmain exists, then returns:
"                   d:/path/to/main
"               if a:1 is supplied, then it is used to modify the *.latexmain
"               file instead of using ':p:r:r'.
function! Tex_GetMainFileName(...)
	if a:0 > 0
		let modifier = a:1
	else
		let modifier = ':p:r:r'
	endif

	" If the user wants to use his own way to specify the main file name, then
	" use it straight away.
	if Tex_GetVarValue('Tex_MainFileExpression', '') != ''
		exec 'let retval = '.Tex_GetVarValue('Tex_MainFileExpression', '')
		return retval
	endif

	let curd = getcwd()

	let dirmodifier = '%:p:h'
	let dirLast = expand(dirmodifier)
	" escape spaces whenever we use cd (diego Caraffini)
	exe 'cd '.escape(dirLast, ' ')

	" move up the directory tree until we find a .latexmain file.
	" TODO: Should we be doing this recursion by default, or should there be a
	"       setting?
	while glob('*.latexmain') == ''
		let dirmodifier = dirmodifier.':h'
		" break from the loop if we cannot go up any further.
		if expand(dirmodifier) == dirLast
			break
		endif
		let dirLast = expand(dirmodifier)
		exec 'cd '.escape(dirLast, ' ')
	endwhile

	let lheadfile = glob('*.latexmain')
	if lheadfile != ''
		let lheadfile = fnamemodify(lheadfile, modifier)
	endif

	exe 'cd '.escape(curd, ' ')

	return escape(lheadfile, ' ')
endfunction 

" }}}
" Tex_ChooseFromPrompt: process a user input to a prompt string {{{
" " Description: 
function! Tex_ChooseFromPrompt(dialog, list, sep)
	let inp = input(a:dialog)
	if inp =~ '\d\+'
		return Tex_Strntok(a:list, a:sep, inp)
	else
		return inp
	endif
endfunction " }}}
" Tex_ChooseFile: produces a file list and prompts for choice {{{
" Description: 
function! Tex_ChooseFile(dialog)
	let files = glob('*')
	if files == ''
		return ''
	endif
	let s:incnum = 0
	echo a:dialog
	let filenames = substitute(files, "\\v(^|\n)", "\\=submatch(0).Tex_IncrementNumber(1).' : '", 'g')
	echo filenames
	let choice = input('Enter Choice : ')
	let g:choice = choice
	if choice == ''
		return ''
	endif
	if choice =~ '^\s*\d\+\s*$'
		let retval = Tex_Strntok(files, "\n", choice)
	else
		let filescomma = substitute(files, "\n", ",", "g")
		let retval = GetListMatchItem(filescomma, choice)
	endif
	if retval == ''
		return ''
	endif
	return retval
endfunction 

" }}}
" Tex_IncrementNumber: returns an incremented number each time {{{
" Description: 
let s:incnum = 0
function! Tex_IncrementNumber(increm)
	let s:incnum = s:incnum + a:increm
	return s:incnum
endfunction 

" }}}
" Tex_ResetIncrementNumber: increments s:incnum to zero {{{
" Description: 
function! Tex_ResetIncrementNumber(val)
	let s:incnum = a:val
endfunction " }}}
" Tex_EscapeForGrep: escapes \ and " the correct number of times {{{
" Description: This command escapes the backslash and double quotes in a
" 	search pattern the correct number of times so it can be used in the :grep
" 	command. This command is meant to be used as:
" 	exec "silent! grep '".Tex_EscapeForGrep(pattern)."' file"
" 	NOTE: The pattern in the grep command should _always_ be enclosed in
" 	      single quotes (not double quotes) for robust performance.
function! Tex_EscapeForGrep(string)
	" This first escaping is so that grep gets a string like '\\bibitem' when
	" we want to search for a string like '\bibitem'.
	let retVal = escape(a:string, "\\")
	" The next escape is because when the shellxquote is ", then the grep
	" commad is usually called as bash -c "grep pattern filename" which means
	" that we need to escape backslashes (because they get halved) and also
	" double quotes.
	if &shellxquote == '"'
		let retVal = escape(retVal, "\"\\")
	endif
	return retVal
endfunction " }}}
" Functions for debugging {{{
" Tex_Debug: appends the argument into s:debugString {{{
" Description: 
" 
" Do not want a memory leak! Set this to zero so that latex-suite always
" starts out in a non-debugging mode.
if !exists('g:Tex_Debug')
	let g:Tex_Debug = 0
endif
function! Tex_Debug(str, ...)
	if !g:Tex_Debug
		return
	endif
	if a:0 > 0
		let pattern = a:1
	else
		let pattern = ''
	endif
	if !exists('s:debugString_'.pattern)
		let s:debugString_{pattern} = ''
	endif
	let s:debugString_{pattern} = s:debugString_{pattern}.a:str."\n"
	let s:debugString_ = s:debugString_.pattern.' : '.a:str."\n"
endfunction " }}}
" Tex_PrintDebug: prings s:debugString {{{
" Description: 
" 
function! Tex_PrintDebug(...)
	if a:0 > 0
		let pattern = a:1
	else
		let pattern = ''
	endif
	if exists('s:debugString_'.pattern)
		echo s:debugString_{pattern}
	endif
endfunction " }}}
" Tex_ClearDebug: clears the s:debugString string {{{
" Description: 
" 
function! Tex_ClearDebug(...)
	if a:0 > 0
		let pattern = a:1
	else
		let pattern = ''
	endif
	if exists('s:debugString_'.pattern)
		let s:debugString_{pattern} = ''
	endif
endfunction " }}}
" }}}

" source texproject.vim before other files
exe 'source '.s:path.'/texproject.vim'

" source all the relevant files.
exe 'source '.s:path.'/texmenuconf.vim'
exe 'source '.s:path.'/envmacros.vim'
exe 'source '.s:path.'/elementmacros.vim'

" source utf-8 or plain math menus
if exists("g:Tex_UseUtfMenus") && g:Tex_UseUtfMenus != 0 && has("gui_running")
	exe 'source '.s:path.'/mathmacros-utf.vim'
else
	exe 'source '.s:path.'/mathmacros.vim'
endif

exe 'source '.s:path.'/multicompile.vim'
exe 'source '.s:path.'/compiler.vim'
exe 'source '.s:path.'/folding.vim'
exe 'source '.s:path.'/templates.vim'
exe 'source '.s:path.'/custommacros.vim'
exe 'source '.s:path.'/bibtex.vim'

if g:Tex_Diacritics != 0
	exe 'source '.s:path.'/diacritics.vim'
endif

" ==============================================================================
" Finally set up the folding, options, mappings and quit.
" ============================================================================== 
" SetTeXOptions: sets options/mappings for this file. {{{
function! <SID>SetTeXOptions()
	" Avoid reinclusion.
	if exists('b:doneSetTeXOptions')
		return
	endif
	let b:doneSetTeXOptions = 1

	exe 'setlocal dict+='.s:path.'/dictionaries/dictionary'

	call Tex_Debug('SetTeXOptions: sourcing maps')
	" smart functions
	if g:Tex_SmartKeyQuote 
		inoremap <buffer> <silent> " "<Left><C-R>=<SID>TexQuotes()<CR>
	endif
	if g:Tex_SmartKeyBS
		inoremap <buffer> <silent> <BS> <C-R>=<SID>SmartBS(<SID>SmartBS_pat())<CR>
	endif
	if g:Tex_SmartKeyDot
		inoremap <buffer> <silent> . <C-R>=<SID>SmartDots()<CR>
	endif

	" This line seems to be necessary to source our compiler/tex.vim file.
	" The docs are unclear why this needs to be done even though this file is
	" the first compiler plugin in 'runtimepath'.
	runtime compiler/tex.vim

endfunction

augroup LatexSuite
	au LatexSuite User LatexSuiteFileType 
		\ call Tex_Debug('main.vim: Catching LatexSuiteFileType event') | 
		\ call <SID>SetTeXOptions()
augroup END

" }}}

" This variable has to be set before sourcing package files to add names of
" commands to completion
let g:Tex_completion_explorer = ',' 
 
" Mappings defined in package files will overwrite all other

exe 'source '.s:path.'/packages.vim'

let &cpo = s:save_cpo

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4:nowrap
