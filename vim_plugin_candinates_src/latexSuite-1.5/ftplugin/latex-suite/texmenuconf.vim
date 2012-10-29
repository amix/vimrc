"=============================================================================
"        File: texmenuconf.vim
"      Author: Srinath Avadhanula <srinath@fastmail.fm>
"   Copyright: Vim charityware license. :help license
" Description: 
"         CVS: $Id: texmenuconf.vim,v 1.20 2003/08/29 01:37:02 srinathava Exp $
" 
"=============================================================================

" Paths, crucial for functions
let s:path = expand("<sfile>:p:h")
let s:up_path = expand("<sfile>:p:h:h")
let s:mainmenuname = g:Tex_MenuPrefix.'S&uite.'
let s:mapleader = exists('mapleader') ? mapleader : "\\"

" This glboal variable is incremented each time a top-level latex-suite menu
" is created. We should always use this variable for setting the locations of
" newly created top-level menus.
let g:Tex_NextMenuLocation = g:Tex_MainMenuLocation

" The templates and macros menus are always nested within the main latex-suit
" menu.
let g:Tex_TemplatesMenuLocation = g:Tex_MainMenuLocation.'.20 '.s:mainmenuname.'&Templates.'
let g:Tex_MacrosMenuLocation = g:Tex_MainMenuLocation.'.20 '.s:mainmenuname.'&Macros.'

" The packages menu can either be a child of the main menu or be a top-level
" menu by itself.
if g:Tex_NestPackagesMenu
	let g:Tex_PackagesMenuLocation = (g:Tex_MainMenuLocation).'.10 '.s:mainmenuname.'&Packages.'
else
	let g:Tex_PackagesMenuLocation = (g:Tex_NextMenuLocation).'.10 '.g:Tex_MenuPrefix.'Packages.'
	let g:Tex_NextMenuLocation = g:Tex_NextMenuLocation + 1
endif

" Environments are always a top-level menu.
let g:Tex_EnvMenuLocation = (g:Tex_NextMenuLocation).'.20 '.g:Tex_MenuPrefix.'E&nvironments.'
let g:Tex_NextMenuLocation = g:Tex_NextMenuLocation + 1

" Elements are always a top-level menu. 
" If we choose to nest elements, then the top-level &TeX-Elements menu
" contains <Fonts / Counters / Dimensions>
" otherwise, the Fonts, Counters and Dimensions menus become top-level menus.
if g:Tex_NestElementMenus
	let g:Tex_ElementsMenuLocation = (g:Tex_NextMenuLocation).'.20 '.g:Tex_MenuPrefix.'E&lements.'
else
	let g:Tex_ElementsMenuLocation = (g:Tex_NextMenuLocation).'.20 '.g:Tex_MenuPrefix
endif
let g:Tex_NextMenuLocation = g:Tex_NextMenuLocation + 1


" Set up the compiler/viewer menus. {{{
"
if has('gui_running') && g:Tex_Menus
	exec 'anoremenu '.g:Tex_MainMenuLocation.'.25 '. s:mainmenuname.'-sepsuite0-  :'

	" menus for compiling / viewing etc.
	exec 'anoremenu '.g:Tex_MainMenuLocation.'.30 '.s:mainmenuname.'&Compile<tab>'.s:mapleader.'ll'.
		\'   :silent! call RunLaTeX()<CR>'
	exec 'anoremenu '.g:Tex_MainMenuLocation.'.40 '.s:mainmenuname.'&View<tab>'.s:mapleader.'lv'.
		\'   :silent! call ViewLaTeX()<CR>'
	exec 'anoremenu '.g:Tex_MainMenuLocation.'.50 '.s:mainmenuname.'&Search<tab>'.s:mapleader.'ls'.
		\'   :silent! call ForwardSearchLaTeX()<CR>'
	exec 'anoremenu '.g:Tex_MainMenuLocation.'.60 '.s:mainmenuname.'&Target\ Format<tab>:TTarget'.
		\'   :call SetTeXTarget()<CR>'
	exec 'anoremenu '.g:Tex_MainMenuLocation.'.70 '.s:mainmenuname.'&Compiler\ Target<tab>:TCTarget'.
		\'   :call SetTeXCompilerTarget("Compile", "")<CR>'
	exec 'anoremenu '.g:Tex_MainMenuLocation.'.80 '.s:mainmenuname.'&Viewer\ Target<tab>:TVTarget'.
		\'   :call SetTeXCompilerTarget("View", "")<CR>'
	exec 'anoremenu '.g:Tex_MainMenuLocation.'.90 '.s:mainmenuname.'Set\ &Ignore\ Level<tab>:TCLevel'.
		\'   :TCLevel<CR>'
	exec 'imenu '.g:Tex_MainMenuLocation.'.100 '.s:mainmenuname.'C&omplete\ Ref/Cite'.
		\'   <Plug>Tex_Completion'
	exec 'anoremenu '.g:Tex_MainMenuLocation.'.110 '.s:mainmenuname.'-sepsuite1- :'
	" refreshing folds
	if g:Tex_Folding
		exec 'anoremenu '.g:Tex_MainMenuLocation.'.120 '.s:mainmenuname.'&Refresh\ Folds<tab>'.s:mapleader.'rf'.
			\'   :call MakeTexFolds(1)<CR>'
		exec 'anoremenu '.g:Tex_MainMenuLocation.'.130 '.s:mainmenuname.'-sepsuite2- :'
	endif
endif

" }}}

" ==============================================================================
" MenuConf: configure the menus as compact/extended, with/without math
" ============================================================================== 
function! Tex_MenuConfigure(type, action) " {{{
	let menuloc = s:mainmenuname.'Configure\ Menu.'
	if a:type == 'math'
		if a:action == 1
			let g:Tex_MathMenus = 1
			exe 'so '.s:path.'/mathmacros.vim'
			exe 'amenu disable '.menuloc.'Add\ Math\ Menu'
			exe 'amenu enable '.menuloc.'Remove\ Math\ Menu'
		elseif a:action == 0
			call Tex_MathMenuRemove()
			exe 'amenu enable '.menuloc.'Add\ Math\ Menu'
			exe 'amenu disable '.menuloc.'Remove\ Math\ Menu'
		endif
	elseif a:type == 'elements'
		if a:action == 'expand'
			let g:Tex_ElementsMenuLocation = '80.20 '.g:Tex_MenuPrefix
			exe 'amenu disable '.menuloc.'Expand\ Elements'
			exe 'amenu enable '.menuloc.'Compress\ Elements'
		elseif a:action == 'nest'
			let g:Tex_ElementsMenuLocation = '80.20 '.g:Tex_MenuPrefix.'Elements.'
			exe 'amenu enable '.menuloc.'Expand\ Elements'
			exe 'amenu disable '.menuloc.'Compress\ Elements'
		endif
		exe 'source '.s:path.'/elementmacros.vim'
	elseif a:type == 'packages'
		if a:action == 1
			let g:Tex_PackagesMenu = 1
			exe 'so '.s:path.'/packages.vim'
			exe 'amenu disable '.menuloc.'Load\ Packages\ Menu'
		endif
	endif
endfunction

" }}}

" configuration menu.
if g:Tex_Menus
	exe 'amenu '.g:Tex_MainMenuLocation.'.900 '.s:mainmenuname.'Configure\ Menu.Add\ Math\ Menu         :call Tex_MenuConfigure("math", 1)<cr>'
	exe 'amenu '.g:Tex_MainMenuLocation.'.900 '.s:mainmenuname.'Configure\ Menu.Remove\ Math\ Menu      :call Tex_MenuConfigure("math", 0)<cr>'
	exe 'amenu '.g:Tex_MainMenuLocation.'.900 '.s:mainmenuname.'Configure\ Menu.Expand\ Elements        :call Tex_MenuConfigure("elements", "expand")<cr>'
	exe 'amenu '.g:Tex_MainMenuLocation.'.900 '.s:mainmenuname.'Configure\ Menu.Compress\ Elements      :call Tex_MenuConfigure("elements", "nest")<cr>'
	exe 'amenu '.g:Tex_MainMenuLocation.'.900 '.s:mainmenuname.'Configure\ Menu.Load\ Packages\ Menu    :call Tex_MenuConfigure("packages", 1)<cr>'
endif

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
