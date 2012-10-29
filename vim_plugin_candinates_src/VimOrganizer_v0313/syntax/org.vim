" org.vim - VimOrganizer plugin for Vim
" -------------------------------------------------------------
" Version: 0.30
" Maintainer: Herbert Sitz <hesitz@gmail.com>
" Last Change: 2011 Nov 02
"
" Script: http://www.vim.org/scripts/script.php?script_id=3342
" Github page: http://github.com/hsitz/VimOrganizer 
" Copyright: (c) 2010, 2011 by Herbert Sitz
" The VIM LICENSE applies to all files in the
" VimOrganizer plugin.  
" (See the Vim copyright except read "VimOrganizer"
" in places where that copyright refers to "Vim".)
" http://vimdoc.sourceforge.net/htmldoc/uganda.html#license
" No warranty, express or implied.
" *** *** Use At-Your-Own-Risk *** ***

" THIS FUNC is only for variable stars per level, currently
" not exposed to users
function! s:SynStars(perlevel)
	let b:levelstars = a:perlevel
	exe 'syntax match OL1 +^\(*\)\{1}\s.*+ contains=stars'
	exe 'syntax match OL2 +^\(*\)\{'.( 1 + 1*a:perlevel).'}\s.*+ contains=stars'
	exe 'syntax match OL3 +^\(*\)\{'.(1 + 2*a:perlevel).'}\s.*+ contains=stars'
	exe 'syntax match OL4 +^\(*\)\{'.(1 + 3*a:perlevel).'}\s.*+ contains=stars'
	exe 'syntax match OL5 +^\(*\)\{'.(1 + 4*a:perlevel).'}\s.*+ contains=stars'
	exe 'syntax match OL6 +^\(*\)\{'.(1 + 5*a:perlevel).'}\s.*+ contains=stars'
	exe 'syntax match OL7 +^\(*\)\{'.(1 + 6*a:perlevel).'}\s.*+ contains=stars'
	exe 'syntax match OL8 +^\(*\)\{'.(1 + 7*a:perlevel).'}\s.*+ contains=stars'
	exe 'syntax match OL9 +^\(*\)\{'.(1 + 8*a:perlevel).'}\s.*+ contains=stars'
endfunction
command! ChangeSyn  call <SID>SynStars(b:levelstars)


syntax match Org_Property_Value +^\s*:\S*:\ze.*+

syntax match Org_Tag +\s*:\S*:$+
syntax match Org_Drawer +^\s*:\(PROPERTIES\|LOGBOOK\|END\):\ze.*+
syntax match Org_Date +[<[]\d\d\d\d-\d\d-\d\d.\{-1,}[\]>]+
syntax match Org_Star +\*\+\*+me=e-1 contained
syntax match Org_Table +^\s*|.\+|\s*$+
"syntax match NEXT '\* \zsNEXT' containedin=OL1,OL2,OL3,OL4,OL5,OL6
"syntax match CANCELED '\* \zsCANCELED' containedin=OL1,OL2,OL3,OL4,OL5,OL6
"syntax match TODO '\* \zsTODO' containedin=OL1,OL2,OL3,OL4,OL5,OL6
"syntax match STARTED '\* \zsSTARTED' containedin=OL1,OL2,OL3,OL4,OL5,OL6
"syntax match DONE '\* \zsDONE' containedin=OL1,OL2,OL3,OL4,OL5,OL6

syntax match Org_Config_Line '^#+.*' containedin=Org_Block,Org_Src_Block
syntax region Org_Block start='\c^#+begin.*$' end='\c^#+end.*' keepend contains=Org_Config_Line
syntax region Org_Src_Block start='\c^#+begin_src.*$' end='\c^#+end.*' keepend contains=Org_Config_Line

syntax match OL1 +^\(*\)\{1}\s.*+ contains=stars
syntax match OL2 +^\(*\)\{2}\s.*+ contains=stars
syntax match OL3 +^\(*\)\{3}\s.*+ contains=stars
syntax match OL4 +^\(*\)\{4}\s.*+ contains=stars
syntax match OL5 +^\(*\)\{5}\s.*+ contains=stars
syntax match OL6 +^\(*\)\{6}\s.*+ contains=stars
syntax match OL7 +^\(*\)\{7}\s.*+ contains=stars
syntax match OL8 +^\(*\)\{8}\s.*+ contains=stars
syntax match OL9 +^\(*\)\{9}\s.*+ contains=stars

" character highlights
syn match Org_Code '=\S.\{-}\S='
syn match Org_Itals '\(\_^\|\W\)/\zs\S[^/]\{-}\S\ze/\_W'
syn match Org_Bold '\(\_^\|\W\)\*\zs\S[^*]\{-}\S\ze\*\_W'
syn match Org_Underline '\(\_^\|\W\)_\zs\S[^_]\{-}\S\ze_\_W'
syn match Org_Lnumber '^\t*\(\d\.\)*\s\s' contained

if has("conceal")
	syn region Org_Half_Link concealends matchgroup=linkends start='\[\[' end=']]' contains=FullLink
	syn region Org_Full_Link concealends matchgroup=linkends start='\[\[\(.\{-1,}\)]\[' end=']]'
endif	
"exec "syntax match DONETODO '" . b:v.todoDoneMatch . "' containedin=OL1,OL2,OL3,OL4,OL5,OL6" 
"exec "syntax match NOTDONETODO '" . b:v.todoNotDoneMatch . "' containedin=OL1,OL2,OL3,OL4,OL5,OL6" 

" ***********************************************
" section below is example for having subregions
" of code in an .org file that use syntax highlighting
" for the language in the code block itself 
" not regular Org syntax highlighting.
"  See Emacs' Org documentation for some details:
" http://orgmode.org/manual/Working-With-Source-Code.html#Working-With-Source-Code
" Notice that much of the functionality of 
" source code blocks is for when they are exported or 'tangled'.
" VimOrganizer calls out to an Emacs server for exports (and,
" -- soon to come -- for tangling) so the functionality described
" in the Emacs Org-mode docs already exists for VimOrganizer.
"
" The example below is for Lisp, other languages could be added
" using same priciple. In addition to using context-sensitive
" syntax highlighting for code blocks, VimOrganizer will
" eventually use context-sensitive language indent-rules. . . 
" ************************************************"
let b:current_syntax=''
unlet b:current_syntax
syntax include @Lispcode $VIMRUNTIME/syntax/lisp.vim
syntax region orgLisp start='^#+begin_src/semacs-lisp' end='^#+end_src$' contains=@Lispcode
let b:current_syntax = 'combined'
hi orgLisp gui=bold


" vim600: set foldmethod=marker foldlevel=0:
