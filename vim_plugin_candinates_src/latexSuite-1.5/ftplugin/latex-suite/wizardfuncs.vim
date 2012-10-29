"        File: wizardfuncs.vim
"      Author: Mikolaj Machowski <mikmach@wp.pl>
" Description: 
" 
" Installation:
"      History: pluginized by Srinath Avadhanula
"               ( srinath@fastmail.fm)
"=============================================================================

if exists('s:doneOnce')
	finish
endif
let s:doneOnce = 1

let s:mapleader = exists('mapleader') ? mapleader : "\\"
" ==============================================================================
" Specialized functions for handling sections from command line
" ============================================================================== 

com! -nargs=? TSection call Tex_section(<f-args>)
com! -nargs=? TSectionAdvanced call Tex_section_adv(<f-args>)

" Tex_VisSecAdv: handles visual selection for sections {{{
function! Tex_VisSecAdv(section)
	let shorttitle =  input("Short title? ")
	let toc = input("Include in table of contents [y]/n ? ")
	let sstructure = "\\".a:section
	if ( toc == "" || toc == "y" )
		let toc = ""
	else
		let toc = "*"
	endif
	if shorttitle != ""
		let shorttitle = '['.shorttitle.']'
	endif
	exe "normal `>a}\<cr>\<esc>`<i".sstructure.toc.shorttitle."{"
endfunction 

" }}}
" Tex_InsSecAdv: section wizard in insert mode {{{
function! Tex_InsSecAdv(structure)
	let ttitle = input("Title? ")
	let shorttitle =  input("Short title? ")
	let toc = input("Include in table of contents [y]/n ? ")
	"Structure
	let sstructure = "\\".a:structure
	"TOC
	if ( toc == "" || toc == "y" )
		let toc = ""
	else
		let toc = "*"
	endif
	"Shorttitle
	if shorttitle != ""
		let shorttitle = '['.shorttitle.']'
	endif
	"Title
	let ttitle = '{'.ttitle.'}'
	"Happy end?
	return sstructure.toc.shorttitle.ttitle 
endfunction 


" }}}
function! Tex_section(...) "{{{
	silent let pos = line('.').' | normal! '.virtcol('.').'|'
	silent let last_section_value = s:Tex_section_detection()
	if a:0 == 0
		silent let last_section_name = s:Tex_section_name(last_section_value)
		silent call s:Tex_section_call(last_section_name)
	elseif a:1 =~ "[+=\-]"
		silent let sec_arg = a:1
		silent let curr_section_value = s:Tex_section_curr_rel_value(sec_arg, last_section_value)
		silent let curr_section_name = s:Tex_section_name(curr_section_value)
		silent call s:Tex_section_call(curr_section_name)
	elseif a:1 == "?"
		echo s:last_section_line
	else
		silent let curr_section_value = s:Tex_section_curr_value(a:1)
		silent let curr_section_name = s:Tex_section_name(curr_section_value)
		silent call s:Tex_section_call(curr_section_name)
	endif
	silent exe pos
endfunction "}}}
function! Tex_section_adv(...) "{{{
	let pos = line('.').' | normal! '.virtcol('.').'|'
	silent let last_section_value = s:Tex_section_detection()
	if a:0 == 0
		silent let last_section_name = s:Tex_section_name(last_section_value)
		let section = Tex_InsSecAdv(last_section_name)
	elseif a:1 =~ "[+=\-]"
		silent let sec_arg = a:1
		silent let curr_section_value = s:Tex_section_curr_rel_value(sec_arg, last_section_value)
		silent let curr_section_name = s:Tex_section_name(curr_section_value)
		let section = Tex_InsSecAdv(curr_section_name)
	else
		silent let curr_section_value = s:Tex_section_curr_value(a:1)
		silent let curr_section_name = s:Tex_section_name(curr_section_value)
		silent call s:Tex_section_call(curr_section_name)
		let section = Tex_InsSecAdv(curr_section_name)
	endif
	exe "normal i".section
	exe pos
endfunction "}}}
function! s:Tex_section_detection() "{{{
	let pos = line('.').' | normal! '.virtcol('.').'|'
	let last_section1 = search("\\\\\subparagraph\\|\\\\paragraph\\|\\\\subsubsection\\|\\\\subsection\\|\\\\section\\|\\\\chapter\\|\\\part\)", "b")
	exe pos
	let last_section2 = search("\\\\\part\\|\\\\chapter\\|\\\\section\\|\\\\subsection\\|\\\\subsubsection\\|\\\\paragraph\\|\\\subparagraph\)", "b")
	if last_section1 > last_section2
		let last_section = last_section1
	else
		let last_section = last_section2
	endif
	if last_section != 0
		exe last_section
		if getline(".") =~ "\\\\part"
			let last_section_value = 0
		elseif getline(".") =~ "\\\\chapter"
			let last_section_value = 1
		elseif getline(".") =~ "\\\\section"
			let last_section_value = 2
		elseif getline(".") =~ "\\\\subsection"
			let last_section_value = 3
		elseif getline(".") =~ "\\\\subsubsection"
			let last_section_value = 4
		elseif getline(".") =~ "\\\\paragraph"
			let last_section_value = 5
		elseif getline(".") =~ "\\\\subparagraph"
			let last_section_value = 6
		endif
		let s:last_section_line = getline(".")
	else
		let last_section_value = 0
	endif
	exe pos
	return last_section_value
endfunction "}}}
function! s:Tex_section_curr_value(sec_arg) "{{{
	if a:sec_arg == "pa" || a:sec_arg == "0" || a:sec_arg == "part"
		let curr_section_value = 0
	elseif a:sec_arg == "ch" || a:sec_arg == "1" || a:sec_arg == "chapter"
		let curr_section_value = 1
	elseif a:sec_arg == "se" || a:sec_arg == "2" || a:sec_arg == "section"
		let curr_section_value = 2
	elseif a:sec_arg == "ss" || a:sec_arg == "3" || a:sec_arg == "subsection"
		let curr_section_value = 3
	elseif a:sec_arg == "s2" || a:sec_arg == "4" || a:sec_arg == "subsubsection"
		let curr_section_value = 4
	elseif a:sec_arg == "pr" || a:sec_arg == "5" || a:sec_arg == "paragraph"
		let curr_section_value = 5
	elseif a:sec_arg == "sp" || a:sec_arg == "6" || a:sec_arg == "subparagraph"
		let curr_section_value = 6
	endif
	return curr_section_value
endfunction "}}}
function! s:Tex_section_curr_rel_value(sec_arg, last_section_value) "{{{
	let last_section_value = a:last_section_value
	if a:sec_arg == "+" || a:sec_arg == "+1"
		let curr_section_value = last_section_value + 1
	elseif a:sec_arg == "++" || a:sec_arg == "+2"
		let curr_section_value = last_section_value + 2
	elseif a:sec_arg == "-" || a:sec_arg == "-1"
		let curr_section_value = last_section_value - 1
	elseif a:sec_arg == "--" || a:sec_arg == "-2"
		let curr_section_value = last_section_value - 2
	elseif a:sec_arg == "="
		let curr_section_value = last_section_value
	else
		exe "let curr_section_value = last_section_value".a:sec_arg
	endif
	if curr_section_value < 0
		let curr_section_value = 0
	elseif curr_section_value > 6
		let curr_section_value = 6
	endif
	return curr_section_value
endfunction "}}}
function! s:Tex_section_name(section_value) "{{{
	if a:section_value == 0
		let section_name = "part"
	elseif a:section_value == 1
		let section_name = "chapter"
	elseif a:section_value == 2
		let section_name = "section"
	elseif a:section_value == 3
		let section_name = "subsection"
	elseif a:section_value == 4
		let section_name = "subsubsection"
	elseif a:section_value == 5
		let section_name = "paragraph"
	elseif a:section_value == 6
		let section_name = "subparagraph"
	endif
	return section_name
endfunction "}}}
function! s:Tex_section_call(section_name) "{{{
	exe "normal! i\\".a:section_name."{<++>}<++>\<Esc>0\<C-j>"
"	let ret_section = "\\".a:section_name."{<++>}<++>"
"	exe "normal! i\<C-r>=IMAP_PutTextWithMovement(ret_section)\<CR>"
"	normal f}i
endfunction "}}}

" ==============================================================================
" Add looking help into latexhelp.txt
" ============================================================================== 

inoremap <buffer> <silent> <F1> <C-O>:call <SID>TexHelp()<CR>
nnoremap <buffer> <silent> <F1> :call <SID>TexHelp()<CR>
command! -nargs=0 THelp call <SID>TexHelp()

" TexHelp: Cursor being on LaTeX item check if exists help tag about it " {{{
function! s:TexHelp()
	let syntax_item = synIDattr(synID(line('.'),col('.')-1,0),"name")
	if syntax_item =~ '^tex'
		setlocal isk+=\
		let curword = expand('<cword>')
		setlocal isk-=\
		let v:errmsg = ''
		if curword =~ "^\\" || syntax_item == 'texSectionName'
			exe 'silent! help '.curword
			if v:errmsg =~ '^E149:'
				echohl ErrorMsg
				exe "echomsg 'Sorry, no help for LaTeX: ".curword."'"
				echohl None
				let v:errmsg = ''
			endif
		else
			help
		endif
	else
		help
	endif
endfunction " }}}

" ==============================================================================
" Tables of shortcuts
" ============================================================================== 
"
command! -nargs=? Tshortcuts call Tex_shortcuts(<f-args>)<CR>

" Tex_shortcuts: Show shortcuts in terminal after : command {{{
function! Tex_shortcuts(...)
	if a:0 == 0
		let shorts = input(" Allowed arguments are:"
		\."\n g     General"
		\."\n e     Environments"
		\."\n f     Fonts"
		\."\n s     Sections"
		\."\n m     Math"
		\."\n a     All"
		\."\n Enter your choice (<Enter> quits) : ")
		call Tex_shortcuts(shorts)
	elseif a:1 == 'g'
		echo g:generalshortcuts
	elseif a:1 == 'e'
		echo g:environmentshortcuts
	elseif a:1 == 'f'
		echo g:fontshortcuts
	elseif a:1 == 's'
		echo g:sectionshortcuts
	elseif a:1 == 'm'
		echo g:mathshortcuts
	elseif a:1 == 'a'
		echo g:generalshortcuts
		echo g:environmentshortcuts
		echo g:fontshortcuts
		echo g:sectionshortcuts
		echo g:mathshortcuts
	endif

endfunction
" }}}

" General shortcuts {{{
let g:generalshortcuts = ''
\."\n General shortcuts"
\."\n <mapleader> is a value of <Leader>"
\."\n ".s:mapleader.'ll	compile whole document'
\."\n ".s:mapleader.'lv	view compiled document'
\."\n ".s:mapleader.'lp	view last compiled part of document'
\."\n ".s:mapleader.'ls	make forward searching if possible'
\."\n ".s:mapleader.'rf	refresh folds'
" }}}
" Environment shortcuts {{{
let g:environmentshortcuts = ''
\."\n Environment shortcuts"
\."\n <mapleader> is a value of g:Tex_Leader2"
\."\n I     v&V                       I     v&V"
\."\n ELI   ".g:Tex_Leader2."li   list                EQN   ".g:Tex_Leader2."qn   quotation"
\."\n EDE   ".g:Tex_Leader2."de   description         ESB   ".g:Tex_Leader2."sb   sloppybar"
\."\n EEN   ".g:Tex_Leader2."en   enumerate           ETI   ".g:Tex_Leader2."ti   theindex"
\."\n EIT   ".g:Tex_Leader2."it   itemize             ETP   ".g:Tex_Leader2."tp   titlepage"
\."\n ETI   ".g:Tex_Leader2."ti   theindex            EVM   ".g:Tex_Leader2."vm   verbatim"
\."\n ETL   ".g:Tex_Leader2."tl   trivlist            EVE   ".g:Tex_Leader2."ve   verse"
\."\n ETE   ".g:Tex_Leader2."te   table               ETB   ".g:Tex_Leader2."tb   thebibliography"
\."\n ETG   ".g:Tex_Leader2."tg   tabbing             ENO   ".g:Tex_Leader2."no   note"
\."\n ETR   ".g:Tex_Leader2."tr   tabular             EOV   ".g:Tex_Leader2."ov   overlay"
\."\n EAR   ".g:Tex_Leader2."ar   array               ESL   ".g:Tex_Leader2."sl   slide"
\."\n EDM   ".g:Tex_Leader2."dm   displaymath         EAB   ".g:Tex_Leader2."ab   abstract"
\."\n EEA   ".g:Tex_Leader2."ea   eqnarray            EAP   ".g:Tex_Leader2."ap   appendix"
\."\n EEQ   ".g:Tex_Leader2."eq   equation            ECE   ".g:Tex_Leader2."ce   center"
\."\n EDO   ".g:Tex_Leader2."do   document            EFI   ".g:Tex_Leader2."fi   figure"
\."\n EFC   ".g:Tex_Leader2."fc   filecontents        ELR   ".g:Tex_Leader2."lr   lrbox"
\."\n EFL   ".g:Tex_Leader2."fl   flushleft           EMP   ".g:Tex_Leader2."mp   minipage"
\."\n EFR   ".g:Tex_Leader2."fr   flushright          EPI   ".g:Tex_Leader2."pi   picture"
\."\n EMA   ".g:Tex_Leader2."ma   math                EQE   ".g:Tex_Leader2."qe   quote"
" }}}
" Font shortcuts {{{
let g:fontshortcuts = ''
\."\n Font shortcuts"
\."\n <mapleader> is a value of g:Tex_Leader"
\."\n Shortcuts         Effects"
\."\n I        v&V      I&v               V"
\."\n FBF      ".g:Tex_Leader."bf      \\textbf{}         {\\bfseries }"
\."\n FMD      ".g:Tex_Leader."md      \\textmd{}         {\\mdseries }"
\."\n"
\."\n FTT      ".g:Tex_Leader."tt      \\texttt{}         {\\ttfamily }"
\."\n FSF      ".g:Tex_Leader."sf      \\textsf{}         {\\sffamily }"
\."\n FRM      ".g:Tex_Leader."rm      \\textrm{}         {\\rmfamily }"
\."\n"
\."\n FUP      ".g:Tex_Leader."up      \\textup{}         {\\upshape }"
\."\n FSL      ".g:Tex_Leader."sl      \\textsl{}         {\\slshape }"
\."\n FSC      ".g:Tex_Leader."sc      \\textsc{}         {\\scshape }"
\."\n FIT      ".g:Tex_Leader."it      \\textit{}         {\\itshape }"
" }}}
" Section shortcuts {{{
let g:sectionshortcuts = ''
\."\n Section shortcuts"
\."\n <mapleader> is a value of g:Tex_Leader2"
\."\n I     v&V"
\."\n SPA   ".g:Tex_Leader2."pa   part"
\."\n SCH   ".g:Tex_Leader2."ch   chapter"
\."\n SSE   ".g:Tex_Leader2."se   section"
\."\n SSS   ".g:Tex_Leader2."ss   subsection"
\."\n SS2   ".g:Tex_Leader2."s2   subsubsection"
\."\n SPG   ".g:Tex_Leader2."pg   paragraph"
\."\n SSP   ".g:Tex_Leader2."sp   subparagraph"
" }}}
" Math shortcuts {{{
let g:mathshortcuts = ''
\."\n Math shortcuts - Insert mode"
\."\n `a     \\alpha            `b     \\beta"
\."\n `g     \\gamma            `d     \\delta"
\."\n `e     \\varepsilon       `z     \\zeta"
\."\n `h     \\eta              `q     \\theta"
\."\n `i     \\iota             `k     \\kappa"
\."\n `l     \\lambda           `m     \\mu"
\."\n `n     \\nu               `x     \\xi"
\."\n `p     \\pi               `r     \\rho"
\."\n `s     \\sigma            `v     \\varsigma"
\."\n `t     \\tau              `u     \\upsilon"
\."\n `f     \\varphi           `c     \\chi"
\."\n `y     \\psi              `w     \\omega"
\."\n `A     \\Alpha            `B     \\Beta"
\."\n `G     \\Gamma            `D     \\Delta"
\."\n `E     \\Epsilon          `Z     \\mathrm{Z}"
\."\n `H     \\Eta              `K     \\Kappa"
\."\n `L     \\Lambda           `M     \\Mu"
\."\n `N     \\Nu               `X     \\Xi"
\."\n `P     \\Pi               `R     \\Rho"
\."\n `S     \\Sigma            `T     \\Tau"
\."\n `U     \\Upsilon          `C     \\Chi"
\."\n `Y     \\Psi              `W     \\Omega"
\."\n `(     \\subset           `)     \\Subset"
\."\n `=     \\equiv            =~     \\approx"
\."\n `-     \\bigcap           `+     \\bigcup"
\."\n `.     \\cdot             `*     \\times"
\."\n `\\     \\setminus         `@     \\circ"
\."\n `&     \\wedge            `,     \\nonumber"
\."\n `8     \\infty            `_     \\bar{}"
\."\n `:     \\ddot{}           `;     \\dot{}"
\."\n `^     \\hat{}            `~     \\tilde{}"
\."\n `6     \\partial"
" }}}

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
