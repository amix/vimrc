"=============================================================================
" 	     File: elementmacros.vim
"      Author: Mikolaj Machowski
"     Created: Tue Apr 23 06:00 PM 2002 PST
" 
"  Description: macros for dimensions/fonts/counters.
"               and various common commands such ref/label/footnote.
"=============================================================================

nmap <silent> <script> <plug> i
imap <silent> <script> <C-o><plug> <Nop>

if exists('s:lastElementsLocation') && g:Tex_ElementsMenuLocation == s:lastElementsLocation
	finish
endif

if exists('s:lastElementsLocation')
	exe 'aunmenu '.s:lastElementsLocation.'Font.'
	exe 'aunmenu '.s:lastElementsLocation.'Dimension.'
	exe 'aunmenu '.s:lastElementsLocation.'Counters.'
	exe 'aunmenu '.s:lastElementsLocation.'Various.'
endif

let s:lastElementsLocation = g:Tex_ElementsMenuLocation

let s:fontMenuLoc       = g:Tex_ElementsMenuLocation.'Font.'
let s:dimensionMenuLoc  = g:Tex_ElementsMenuLocation.'Dimension.'
let s:counterMenuLoc    = g:Tex_ElementsMenuLocation.'Counters.'
let s:variousMenuLoc    = g:Tex_ElementsMenuLocation.'Various.'

" ==============================================================================
" Set up the functions the first time.
" ============================================================================== 
if !exists('s:definedFuncs') " {{{
	let s:definedFuncs = 1

	" Tex_RemoveElementMenus: remove the elements menu {{{
	"
	function! Tex_RemoveElementMenus()
		exe 'silent! aunmenu '.s:lastElementsLocation.'Font.'
		exe 'silent! aunmenu '.s:lastElementsLocation.'Dimension.'
		exe 'silent! aunmenu '.s:lastElementsLocation.'Counters.'
		exe 'silent! aunmenu '.s:lastElementsLocation.'Various.'
	endfunction

	" }}}
	" Tex_FontFamily: sets up font menus {{{
	"
	function! <SID>Tex_FontFamily(font,fam)
		let vislhs = matchstr(tolower(a:font), '^.\zs.*')

		" avoid redoing imaps and vmaps for every reconfiguration of menus.
		if !exists('s:doneOnce') && g:Tex_FontMaps
			exe "vnoremap <silent> ".g:Tex_Leader.vislhs.
				\" \<C-\\>\<C-N>:call VEnclose('\\text".vislhs."{', '}', '{\\".vislhs.a:fam." ', '}')<CR>"
			exe 'call IMAP ("'.a:font.'", "\\text'.vislhs.'{<++>}<++>", "tex")'
		endif

		" menu entry.
		if g:Tex_Menus && g:Tex_FontMenus
			let location = s:fontMenuLoc.substitute(a:fam, '^.', '\u&', '').'.'.vislhs.a:fam.'<tab>'.a:font.'\ ('.g:Tex_Leader.vislhs.')'
			exe "amenu ".location.
				\" <plug><C-r>=IMAP_PutTextWithMovement('\\text".vislhs."{<++>}<++>')<CR>"
			exe "vmenu ".location.
				\" \<C-\\>\<C-N>:call VEnclose('\\text".vislhs."{', '}', '{\\".vislhs.a:fam." ', '}')<CR>"
		endif

	endfunction

	" }}}
	" Tex_FontDiacritics: sets up menus for diacritics. {{{
	"
	function! <SID>Tex_FontDiacritics(name, rhs)
		let location = s:fontMenuLoc.'&Diacritics.'.a:name.'<tab>'
		exe 'amenu '.location.
			\" <plug><C-r>=IMAP_PutTextWithMovement('\\".a:rhs."{<++>}<++>')<CR>"
		exe 'vmenu '.location.
			\" \<C-\\>\<C-n>:call VEnclose('\\".a:rhs."{', '}', '', '')<CR>" 
	endfunction " }}}
	" Tex_FontSize: sets up size fonts {{{
	"
	function! <SID>Tex_FontSize(name)
		let location = s:fontMenuLoc.'&Size.'.a:name.'<tab>'
		exe 'amenu '.location." <plug>\\".a:name
		exe 'vunmenu '.location
	endfunction " }}}
	" Tex_Fontfont: sets up the 'font' part of font menus {{{
	" 
	function! <SID>Tex_Fontfont(desc, lhs)
		let location = s:fontMenuLoc.'&font.'.a:desc.'<tab>'
		exe "amenu ".location."  <plug><C-r>=IMAP_PutTextWithMovement('".a:lhs."')<CR>"
		exe "vunmenu ".location
	endfunction " }}}
	" Tex_DimMenus: set up dimension menus {{{
	function! <SID>Tex_DimMenus(submenu, rhs)
		let location = s:dimensionMenuLoc.a:submenu.'.'.a:rhs.'<tab>'
		exe "amenu ".location." <plug>\\".a:rhs
		exe "vunmenu ".location
	endfunction " }}}
	" Tex_CounterMenus: set up counters menus {{{
	function! <SID>Tex_CounterMenus(submenu, rhs)
		let location = s:counterMenuLoc.a:submenu.'.'.a:rhs.'<tab>'
		exe "amenu ".location." <plug>\\".a:rhs
		exe "vunmenu ".location
	endfunction " }}}
	" Tex_VariousMenus: set up various menus {{{
	function! <SID>Tex_VariousMenus(desc, lhs)
		let location = s:variousMenuLoc.a:desc.'<tab>'
		exe "amenu ".location." <plug><C-r>=IMAP_PutTextWithMovement('".a:lhs."')<CR>"
		exe "vunmenu ".location
	endfunction " }}}
endif
" }}}

" ==============================================================================
" Fonts
" ============================================================================== 
" series/family/shape {{{
call <SID>Tex_FontFamily("FBF","series")
call <SID>Tex_FontFamily("FMD","series")

call <SID>Tex_FontFamily("FTT","family")
call <SID>Tex_FontFamily("FSF","family")
call <SID>Tex_FontFamily("FRM","family")

call <SID>Tex_FontFamily("FUP","shape")
call <SID>Tex_FontFamily("FSL","shape")
call <SID>Tex_FontFamily("FSC","shape")
call <SID>Tex_FontFamily("FIT","shape")

" the \emph is special.
if g:Tex_FontMaps | exe "vnoremap <silent> ".g:Tex_Leader."em \<C-\\>\<C-N>:call VEnclose('\\emph{', '}', '{\\em', '\\/}')<CR>" | endif
if g:Tex_FontMaps | exe 'call IMAP ("FEM", "\\emph{<++>}<++>", "tex")' | endif

" }}}
if g:Tex_Menus && g:Tex_FontMenus
	" {{{ diacritics
	call <SID>Tex_FontDiacritics('Acute',        '"')
	call <SID>Tex_FontDiacritics('Breve',        'u')
	call <SID>Tex_FontDiacritics('Circle',       'r')
	call <SID>Tex_FontDiacritics('Circumflex',   '^')
	call <SID>Tex_FontDiacritics('Umlaut',       '"')
	call <SID>Tex_FontDiacritics('HUmlaut',      'H')
	call <SID>Tex_FontDiacritics('Dot\ over',    '.')
	call <SID>Tex_FontDiacritics('Grave',        '`')
	call <SID>Tex_FontDiacritics('Hacek',        'v')
	call <SID>Tex_FontDiacritics('Makron',       '=')
	call <SID>Tex_FontDiacritics('Tilde',        '~')
	call <SID>Tex_FontDiacritics('Underline',    'b')
	call <SID>Tex_FontDiacritics('Cedille',      'c')
	call <SID>Tex_FontDiacritics('Dot\ under',   ' ')
	call <SID>Tex_FontDiacritics('Ligature',     't')
	" }}}
	" {{{ Si&ze.
	call <SID>Tex_FontSize('tiny')
	call <SID>Tex_FontSize('scriptsize')
	call <SID>Tex_FontSize('footnotesize')
	call <SID>Tex_FontSize('small')
	call <SID>Tex_FontSize('normalsize')
	call <SID>Tex_FontSize('large')
	call <SID>Tex_FontSize('Large')
	call <SID>Tex_FontSize('LARGE')
	call <SID>Tex_FontSize('huge')
	call <SID>Tex_FontSize('Huge')
	" }}}
	" {{{ &font.
	call s:Tex_Fontfont('fontencoding{}',               '\fontencoding{<++>}<++>')
	call s:Tex_Fontfont('fontfamily{qtm}',              '\fontfamily{<++>}<++>')
	call s:Tex_Fontfont('fontseries{m\ b\ bx\ sb\ c}',  '\fontseries{<++>}<++>')
	call s:Tex_Fontfont('fontshape{n\ it\ sl\ sc\ ui}', '\fontshape{<++>}<++>')
	call s:Tex_Fontfont('fontsize{}{}',                 '\fontsize{<++>}{<++>}<++>')
	call s:Tex_Fontfont('selectfont',                   '\selectfont ')
	" }}}
endif

" ==============================================================================
" Dimensions
" ============================================================================== 
if g:Tex_Menus
	" {{{ Static1
	call <SID>Tex_DimMenus('Static1', 'arraycolsep')
	call <SID>Tex_DimMenus('Static1', 'arrayrulewidth')
	call <SID>Tex_DimMenus('Static1', 'bibindent')
	call <SID>Tex_DimMenus('Static1', 'columnsep')
	call <SID>Tex_DimMenus('Static1', 'columnseprule')
	call <SID>Tex_DimMenus('Static1', 'columnwidth')
	call <SID>Tex_DimMenus('Static1', 'doublerulesep')
	call <SID>Tex_DimMenus('Static1', 'evensidemargin')
	call <SID>Tex_DimMenus('Static1', 'fboxrule')
	call <SID>Tex_DimMenus('Static1', 'fboxsep')
	call <SID>Tex_DimMenus('Static1', 'footheight')
	call <SID>Tex_DimMenus('Static1', 'footnotesep')
	call <SID>Tex_DimMenus('Static1', 'footskip')
	call <SID>Tex_DimMenus('Static1', 'headheight')
	call <SID>Tex_DimMenus('Static1', 'headsep')
	call <SID>Tex_DimMenus('Static1', 'itemindent')
	call <SID>Tex_DimMenus('Static1', 'labelsep')
	call <SID>Tex_DimMenus('Static1', 'labelwidth')
	call <SID>Tex_DimMenus('Static1', 'leftmargin')
	call <SID>Tex_DimMenus('Static1', 'leftmargini')
	call <SID>Tex_DimMenus('Static1', 'leftmarginii')
	call <SID>Tex_DimMenus('Static1', 'leftmarginiii')
	call <SID>Tex_DimMenus('Static1', 'leftmarginiv')
	call <SID>Tex_DimMenus('Static1', 'leftmarginv')
	call <SID>Tex_DimMenus('Static1', 'leftmarginvi')
	call <SID>Tex_DimMenus('Static1', 'linewidth')
	call <SID>Tex_DimMenus('Static1', 'listparindent')
	call <SID>Tex_DimMenus('Static1', 'marginparpush')
	call <SID>Tex_DimMenus('Static1', 'marginparsep')
	call <SID>Tex_DimMenus('Static1', 'marginparwidth')
	call <SID>Tex_DimMenus('Static1', 'mathindent')
	call <SID>Tex_DimMenus('Static1', 'oddsidemargin')
	" }}}
	" {{{ Static2
	call <SID>Tex_DimMenus('Static2', 'paperheight')
	call <SID>Tex_DimMenus('Static2', 'paperwidth')
	call <SID>Tex_DimMenus('Static2', 'parindent')
	call <SID>Tex_DimMenus('Static2', 'rightmargin')
	call <SID>Tex_DimMenus('Static2', 'tabbingsep')
	call <SID>Tex_DimMenus('Static2', 'tabcolsep')
	call <SID>Tex_DimMenus('Static2', 'textheight')
	call <SID>Tex_DimMenus('Static2', 'textwidth')
	call <SID>Tex_DimMenus('Static2', 'topmargin')
	call <SID>Tex_DimMenus('Static2', 'unitlength')
	" }}}
	" {{{ Dynamic
	call <SID>Tex_DimMenus('Dynamic', 'abovedisplayshortskip')
	call <SID>Tex_DimMenus('Dynamic', 'abovedisplayskip')
	call <SID>Tex_DimMenus('Dynamic', 'baselineskip')
	call <SID>Tex_DimMenus('Dynamic', 'belowdisplayshortskip')
	call <SID>Tex_DimMenus('Dynamic', 'belowdisplayskip')
	call <SID>Tex_DimMenus('Dynamic', 'dblfloatsep')
	call <SID>Tex_DimMenus('Dynamic', 'dbltextfloatsep')
	call <SID>Tex_DimMenus('Dynamic', 'floatsep')
	call <SID>Tex_DimMenus('Dynamic', 'intextsep')
	call <SID>Tex_DimMenus('Dynamic', 'itemsep')
	call <SID>Tex_DimMenus('Dynamic', 'parsep')
	call <SID>Tex_DimMenus('Dynamic', 'parskip')
	call <SID>Tex_DimMenus('Dynamic', 'partopsep')
	call <SID>Tex_DimMenus('Dynamic', 'textfloatsep')
	call <SID>Tex_DimMenus('Dynamic', 'topsep')
	call <SID>Tex_DimMenus('Dynamic', 'topskip')
	" }}}
	" {{{ Change
	call <SID>Tex_DimMenus('Change', 'setlength')
	call <SID>Tex_DimMenus('Change', 'addtolength')
	call <SID>Tex_DimMenus('Change', 'settoheight')
	call <SID>Tex_DimMenus('Change', 'settowidth')
	call <SID>Tex_DimMenus('Change', 'settolength')
	" }}}
endif

" ==============================================================================
" Counters
" ============================================================================== 
if g:Tex_Menus
	" Counters {{{
	call <SID>Tex_CounterMenus('Counters', 'bottomnumber')
	call <SID>Tex_CounterMenus('Counters', 'chapter')
	call <SID>Tex_CounterMenus('Counters', 'dbltopnumber')
	call <SID>Tex_CounterMenus('Counters', 'enumi')
	call <SID>Tex_CounterMenus('Counters', 'enumii')
	call <SID>Tex_CounterMenus('Counters', 'enumiii')
	call <SID>Tex_CounterMenus('Counters', 'enumiv')
	call <SID>Tex_CounterMenus('Counters', 'equation')
	call <SID>Tex_CounterMenus('Counters', 'figure')
	call <SID>Tex_CounterMenus('Counters', 'footnote')
	call <SID>Tex_CounterMenus('Counters', 'mpfootnote')
	call <SID>Tex_CounterMenus('Counters', 'page')
	call <SID>Tex_CounterMenus('Counters', 'paragraph')
	call <SID>Tex_CounterMenus('Counters', 'part')
	call <SID>Tex_CounterMenus('Counters', 'secnumdepth')
	call <SID>Tex_CounterMenus('Counters', 'section')
	call <SID>Tex_CounterMenus('Counters', 'subparagraph')
	call <SID>Tex_CounterMenus('Counters', 'subsection')
	call <SID>Tex_CounterMenus('Counters', 'subsubsection')
	call <SID>Tex_CounterMenus('Counters', 'table')
	call <SID>Tex_CounterMenus('Counters', 'tocdepth')
	call <SID>Tex_CounterMenus('Counters', 'topnumber')
	call <SID>Tex_CounterMenus('Counters', 'totalnumber')
	" }}}
	" theCounters {{{
	call <SID>Tex_CounterMenus('theCounters', 'thebottomnumber')
	call <SID>Tex_CounterMenus('theCounters', 'thechapter')
	call <SID>Tex_CounterMenus('theCounters', 'thedbltopnumber')
	call <SID>Tex_CounterMenus('theCounters', 'theenumi')
	call <SID>Tex_CounterMenus('theCounters', 'theenumii')
	call <SID>Tex_CounterMenus('theCounters', 'theenumiii')
	call <SID>Tex_CounterMenus('theCounters', 'theenumiv')
	call <SID>Tex_CounterMenus('theCounters', 'theequation')
	call <SID>Tex_CounterMenus('theCounters', 'thefigure')
	call <SID>Tex_CounterMenus('theCounters', 'thefootnote')
	call <SID>Tex_CounterMenus('theCounters', 'thempfootnote')
	call <SID>Tex_CounterMenus('theCounters', 'thepage')
	call <SID>Tex_CounterMenus('theCounters', 'theparagraph')
	call <SID>Tex_CounterMenus('theCounters', 'thepart')
	call <SID>Tex_CounterMenus('theCounters', 'thesecnumdepth')
	call <SID>Tex_CounterMenus('theCounters', 'thesection')
	call <SID>Tex_CounterMenus('theCounters', 'thesubparagraph')
	call <SID>Tex_CounterMenus('theCounters', 'thesubsection')
	call <SID>Tex_CounterMenus('theCounters', 'thesubsubsection')
	call <SID>Tex_CounterMenus('theCounters', 'thetable')
	call <SID>Tex_CounterMenus('theCounters', 'thetocdepth')
	call <SID>Tex_CounterMenus('theCounters', 'thetopnumber')
	call <SID>Tex_CounterMenus('theCounters', 'thetotalnumber')
	" }}}
	" Type {{{
	call <SID>Tex_CounterMenus('Type', 'alph')
	call <SID>Tex_CounterMenus('Type', 'Alph')
	call <SID>Tex_CounterMenus('Type', 'arabic')
	call <SID>Tex_CounterMenus('Type', 'roman')
	call <SID>Tex_CounterMenus('Type', 'Roman')
	" }}}
endif

" ==============================================================================
" Various
" ============================================================================== 
if g:Tex_Menus
	" Various {{{
	call <SID>Tex_VariousMenus('ref{}'         , '\ref{<++>}<++>')
	call <SID>Tex_VariousMenus('pageref{}'     , '\pageref{<++>}<++>')
	call <SID>Tex_VariousMenus('label{}'       , '\label{<++>}<++>')
	call <SID>Tex_VariousMenus('footnote{}'    , '\footnote{<++>}<++>')
	call <SID>Tex_VariousMenus('footnotemark{}', '\footnotemark{<++>}<++>')
	call <SID>Tex_VariousMenus('footnotemark{}', '\footnotetext{<++>}<++>')
	call <SID>Tex_VariousMenus('cite{}'        , '\cite{<++>}<++>')
	call <SID>Tex_VariousMenus('nocite{}'      , '\nocite{<++>}<++>')
	" }}}
endif

if g:Tex_CatchVisMapErrors
	exe "vnoremap ".g:Tex_Leader."   :\<C-u>call ExecMap('".g:Tex_Leader."', 'v')\<CR>"
endif
" this is for avoiding reinclusion of imaps from next time on.
let s:doneOnce = 1

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
