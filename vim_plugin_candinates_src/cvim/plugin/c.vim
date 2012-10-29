"#################################################################################
"
"       Filename:  c.vim
"
"    Description:  C/C++-IDE. Write programs by inserting complete statements,
"                  comments, idioms, code snippets, templates and comments.
"                  Compile, link and run one-file-programs without a makefile.
"                  See also help file csupport.txt .
"
"   GVIM Version:  7.0+
"
"  Configuration:  There are some personal details which should be configured
"                   (see the files README.csupport and csupport.txt).
"
"         Author:  Dr.-Ing. Fritz Mehner, FH Südwestfalen, 58644 Iserlohn, Germany
"          Email:  mehner@fh-swf.de
"
"        Version:  see variable  g:C_Version  below
"        Created:  04.11.2000
"        License:  Copyright (c) 2000-2011, Fritz Mehner
"                  This program is free software; you can redistribute it and/or
"                  modify it under the terms of the GNU General Public License as
"                  published by the Free Software Foundation, version 2 of the
"                  License.
"                  This program is distributed in the hope that it will be
"                  useful, but WITHOUT ANY WARRANTY; without even the implied
"                  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
"                  PURPOSE.
"                  See the GNU General Public License version 2 for more details.
"       Revision:  $Id: c.vim,v 1.156 2011/12/27 21:05:07 mehner Exp $
"
"------------------------------------------------------------------------------
"
if v:version < 700
  echohl WarningMsg | echo 'The plugin c-support.vim needs Vim version >= 7 .'| echohl None
  finish
endif
"
" Prevent duplicate loading:
"
if exists("g:C_Version") || &cp
 finish
endif
let g:C_Version= "5.17"  							" version number of this script; do not change
"
"#################################################################################
"
"  Global variables (with default values) which can be overridden.
"
" Platform specific items:  {{{1
" - root directory
" - characters that must be escaped for filenames
"
let s:MSWIN = has("win16") || has("win32")   || has("win64")    || has("win95")
let s:UNIX	= has("unix")  || has("macunix") || has("win32unix")
"
let s:installation					= '*undefined*'
let s:C_GlobalTemplateFile	= ''
let s:C_GlobalTemplateDir		= ''
let s:C_LocalTemplateFile		= ''
let s:C_LocalTemplateDir		= ''
let s:C_FilenameEscChar 		= ''

if	s:MSWIN
  " ==========  MS Windows  ======================================================
	"
	" change '\' to '/' to avoid interpretation as escape character
	if match(	substitute( expand("<sfile>"), '\', '/', 'g' ), 
				\		substitute( expand("$HOME"),   '\', '/', 'g' ) ) == 0
		"
		" USER INSTALLATION ASSUMED
		let s:installation					= 'local'
		let s:plugin_dir  					= substitute( expand('<sfile>:p:h:h'), '\', '/', 'g' )
		let s:C_LocalTemplateFile		= s:plugin_dir.'/c-support/templates/Templates'
		let s:C_LocalTemplateDir		= fnamemodify( s:C_LocalTemplateFile, ":p:h" ).'/'
	else
		"
		" SYSTEM WIDE INSTALLATION
		let s:installation					= 'system'
		let s:plugin_dir						= $VIM.'/vimfiles'
		let s:C_GlobalTemplateDir		= s:plugin_dir.'/c-support/templates'
		let s:C_GlobalTemplateFile  = s:C_GlobalTemplateDir.'/Templates'
		let s:C_LocalTemplateFile		= $HOME.'/vimfiles/c-support/templates/Templates'
		let s:C_LocalTemplateDir		= fnamemodify( s:C_LocalTemplateFile, ":p:h" ).'/'
	endif
	"
  let s:C_FilenameEscChar 			= ''
	let s:C_Display    						= ''
	"
else
  " ==========  Linux/Unix  ======================================================
	"
	if match( expand("<sfile>"), expand("$HOME") ) == 0
		"
		" USER INSTALLATION ASSUMED
		let s:installation					= 'local'
		let s:plugin_dir 						= expand('<sfile>:p:h:h')
		let s:C_LocalTemplateFile		= s:plugin_dir.'/c-support/templates/Templates'
		let s:C_LocalTemplateDir		= fnamemodify( s:C_LocalTemplateFile, ":p:h" ).'/'
	else
		"
		" SYSTEM WIDE INSTALLATION
		let s:installation					= 'system'
		let s:plugin_dir						= $VIM.'/vimfiles'
		let s:C_GlobalTemplateDir		= s:plugin_dir.'/c-support/templates'
		let s:C_GlobalTemplateFile  = s:C_GlobalTemplateDir.'/Templates'
		let s:C_LocalTemplateFile		= $HOME.'/.vim/c-support/templates/Templates'
		let s:C_LocalTemplateDir		= fnamemodify( s:C_LocalTemplateFile, ":p:h" ).'/'
	endif
	"
  let s:C_FilenameEscChar 			= ' \%#[]'
	let s:C_Display								= $DISPLAY
	"
endif
"
let s:C_CodeSnippets  				= s:plugin_dir.'/c-support/codesnippets/'
let s:C_IndentErrorLog				= $HOME.'/.indent.errorlog'
"
"  Use of dictionaries  {{{1
"  Key word completion is enabled by the filetype plugin 'c.vim'
"  g:C_Dictionary_File  must be global
"
if !exists("g:C_Dictionary_File")
  let g:C_Dictionary_File = s:plugin_dir.'/c-support/wordlists/c-c++-keywords.list,'.
        \                   s:plugin_dir.'/c-support/wordlists/k+r.list,'.
        \                   s:plugin_dir.'/c-support/wordlists/stl_index.list'
endif
"
"  Modul global variables (with default values) which can be overridden. {{{1
"
if	s:MSWIN
	let s:C_CCompiler           = 'gcc.exe'  " the C   compiler
	let s:C_CplusCompiler       = 'g++.exe'  " the C++ compiler
	let s:C_ExeExtension        = '.exe'     " file extension for executables (leading point required)
	let s:C_ObjExtension        = '.obj'     " file extension for objects (leading point required)
	let s:C_Man                 = 'man.exe'  " the manual program
else
	let s:C_CCompiler           = 'gcc'      " the C   compiler
	let s:C_CplusCompiler       = 'g++'      " the C++ compiler
	let s:C_ExeExtension        = ''         " file extension for executables (leading point required)
	let s:C_ObjExtension        = '.o'       " file extension for objects (leading point required)
	let s:C_Man                 = 'man'      " the manual program
endif
let s:C_VimCompilerName				= 'gcc'      " the compiler name used by :compiler
"
let s:C_CExtension     				= 'c'                    " C file extension; everything else is C++
let s:C_CFlags         				= '-Wall -g -O0 -c'      " compiler flags: compile, don't optimize
let s:C_CodeCheckExeName      = 'check'
let s:C_CodeCheckOptions      = '-K13'
let s:C_LFlags         				= '-Wall -g -O0'         " compiler flags: link   , don't optimize
let s:C_Libs           				= '-lm'                  " libraries to use
let s:C_LineEndCommColDefault = 49
let s:C_LoadMenus      				= 'yes'
let s:C_CreateMenusDelayed     = 'no'
let s:C_MenuHeader     				= 'yes'
let s:C_OutputGvim            = 'vim'
let s:C_Printheader           = "%<%f%h%m%<  %=%{strftime('%x %X')}     Page %N"
let s:C_Root  	       				= '&C\/C\+\+.'           " the name of the root menu of this plugin
let s:C_TypeOfH               = 'cpp'
let s:C_Wrapper               = s:plugin_dir.'/c-support/scripts/wrapper.sh'
let s:C_XtermDefaults         = '-fa courier -fs 12 -geometry 80x24'
let s:C_GuiSnippetBrowser     = 'gui'										" gui / commandline
let s:C_GuiTemplateBrowser    = 'gui'										" gui / explorer / commandline
"
let s:C_TemplateOverriddenMsg = 'no'
let s:C_Ctrl_j								= 'on'
"
let s:C_FormatDate						= '%x'
let s:C_FormatTime						= '%X'
let s:C_FormatYear						= '%Y'
let s:C_SourceCodeExtensions  = 'c cc cp cxx cpp CPP c++ C i ii'
"
"------------------------------------------------------------------------------
"
"  Look for global variables (if any), to override the defaults.
"
function! C_CheckGlobal ( name )
  if exists('g:'.a:name)
    exe 'let s:'.a:name.'  = g:'.a:name
  endif
endfunction    " ----------  end of function C_CheckGlobal ----------
"
call C_CheckGlobal('C_CCompiler            ')
call C_CheckGlobal('C_CExtension           ')
call C_CheckGlobal('C_CFlags               ')
call C_CheckGlobal('C_CodeCheckExeName     ')
call C_CheckGlobal('C_CodeCheckOptions     ')
call C_CheckGlobal('C_CodeSnippets         ')
call C_CheckGlobal('C_CplusCompiler        ')
call C_CheckGlobal('C_Ctrl_j               ')
call C_CheckGlobal('C_ExeExtension         ')
call C_CheckGlobal('C_FormatDate           ')
call C_CheckGlobal('C_FormatTime           ')
call C_CheckGlobal('C_FormatYear           ')
call C_CheckGlobal('C_GlobalTemplateFile   ')
call C_CheckGlobal('C_GuiSnippetBrowser    ')
call C_CheckGlobal('C_GuiTemplateBrowser   ')
call C_CheckGlobal('C_IndentErrorLog       ')
call C_CheckGlobal('C_LFlags               ')
call C_CheckGlobal('C_Libs                 ')
call C_CheckGlobal('C_LineEndCommColDefault')
call C_CheckGlobal('C_LoadMenus            ')
call C_CheckGlobal('C_CreateMenusDelayed   ')
call C_CheckGlobal('C_LocalTemplateFile    ')
call C_CheckGlobal('C_Man                  ')
call C_CheckGlobal('C_MenuHeader           ')
call C_CheckGlobal('C_ObjExtension         ')
call C_CheckGlobal('C_OutputGvim           ')
call C_CheckGlobal('C_Printheader          ')
call C_CheckGlobal('C_Root                 ')
call C_CheckGlobal('C_SourceCodeExtensions ')
call C_CheckGlobal('C_TemplateOverriddenMsg')
call C_CheckGlobal('C_TypeOfH              ')
call C_CheckGlobal('C_VimCompilerName      ')
call C_CheckGlobal('C_XtermDefaults        ')

if exists('g:C_GlobalTemplateFile') && !empty(g:C_GlobalTemplateFile)
	let s:C_GlobalTemplateDir	= fnamemodify( s:C_GlobalTemplateFile, ":h" )
endif
"
"----- some variables for internal use only -----------------------------------
"
"
" set default geometry if not specified
"
if match( s:C_XtermDefaults, "-geometry\\s\\+\\d\\+x\\d\\+" ) < 0
	let s:C_XtermDefaults	= s:C_XtermDefaults." -geometry 80x24"
endif
"
" escape the printheader
"
let s:C_Printheader  = escape( s:C_Printheader, ' %' )
"
let s:C_HlMessage    = ""
"
" characters that must be escaped for filenames
"
let s:C_If0_Counter   = 0
let s:C_If0_Txt		 		= "If0Label_"
"
let s:C_SplintIsExecutable	= 0
if executable( "splint" )
	let s:C_SplintIsExecutable	= 1
endif
"
let s:C_CodeCheckIsExecutable	= 0
if executable( s:C_CodeCheckExeName )
	let s:C_CodeCheckIsExecutable	= 1
endif
"
"------------------------------------------------------------------------------
"  Control variables (not user configurable)
"------------------------------------------------------------------------------
let s:Attribute                = { 'below':'', 'above':'', 'start':'', 'append':'', 'insert':'' }
let s:C_Attribute              = {}
let s:C_ExpansionLimit         = 10
let s:C_FileVisited            = []
"
let s:C_MacroNameRegex         = '\([a-zA-Z][a-zA-Z0-9_]*\)'
let s:C_MacroLineRegex				 = '^\s*|'.s:C_MacroNameRegex.'|\s*=\s*\(.*\)'
let s:C_MacroCommentRegex			 = '^\$'
let s:C_ExpansionRegex				 = '|?'.s:C_MacroNameRegex.'\(:\a\)\?|'
let s:C_NonExpansionRegex			 = '|'.s:C_MacroNameRegex.'\(:\a\)\?|'
"
let s:C_TemplateNameDelimiter  = '-+_,\. '
let s:C_TemplateLineRegex			 = '^==\s*\([a-zA-Z][0-9a-zA-Z'.s:C_TemplateNameDelimiter
let s:C_TemplateLineRegex			.= ']\+\)\s*==\s*\([a-z]\+\s*==\)\?'
let s:C_TemplateIf						 = '^==\s*IF\s\+|STYLE|\s\+IS\s\+'.s:C_MacroNameRegex.'\s*=='
let s:C_TemplateEndif					 = '^==\s*ENDIF\s*=='
"
let s:C_Com1          				 = '/*'     " C-style : comment start
let s:C_Com2          				 = '*/'     " C-style : comment end
"
let s:C_ExpansionCounter       = {}
let s:C_TJT										 = '[ 0-9a-zA-Z_]*'
let s:C_TemplateJumpTarget1    = '<+'.s:C_TJT.'+>\|{+'.s:C_TJT.'+}'
let s:C_TemplateJumpTarget2    = '<-'.s:C_TJT.'->\|{-'.s:C_TJT.'-}'
let s:C_Macro                  = {'|AUTHOR|'         : 'first name surname',
											\						'|AUTHORREF|'      : '',
											\						'|COMPANY|'        : '',
											\						'|COPYRIGHTHOLDER|': '',
											\						'|EMAIL|'          : '',
											\						'|LICENSE|'        : 'GNU General Public License',
											\						'|ORGANIZATION|'   : '',
											\						'|PROJECT|'        : '',
											\						'|STYLE|'          : ''
											\						}
let	s:C_MacroFlag								= {	':l' : 'lowercase'			,
											\							':u' : 'uppercase'			,
											\							':c' : 'capitalize'		,
											\							':L' : 'legalize name'	,
											\						}
let s:C_ActualStyle					= 'default'
let s:C_ActualStyleLast			= s:C_ActualStyle
let s:C_Template            = { 'default' : {} }
let s:C_TemplatesLoaded			= 'no'

let s:C_ForTypes     = [
    \ 'char'                  ,
    \ 'int'                   ,
    \ 'long'                  ,
    \ 'long int'              ,
    \ 'long long'             ,
    \ 'long long int'         ,
    \ 'short'                 ,
    \ 'short int'             ,
    \ 'size_t'                ,
    \ 'unsigned'              , 
    \ 'unsigned char'         ,
    \ 'unsigned int'          ,
    \ 'unsigned long'         ,
    \ 'unsigned long int'     ,
    \ 'unsigned long long'    ,
    \ 'unsigned long long int',
    \ 'unsigned short'        ,
    \ 'unsigned short int'    ,
    \ ]

let s:C_ForTypes_Check_Order     = [
    \ 'char'                  ,
    \ 'int'                   ,
    \ 'long long int'         ,
    \ 'long long'             ,
    \ 'long int'              ,
    \ 'long'                  ,
    \ 'short int'             ,
    \ 'short'                 ,
    \ 'size_t'                ,
    \ 'unsigned short int'    ,
    \ 'unsigned short'        ,
    \ 'unsigned long long int',
    \ 'unsigned long long'    ,
    \ 'unsigned long int'     ,
    \ 'unsigned long'         ,
    \ 'unsigned int'          ,
    \ 'unsigned char'         ,
    \ 'unsigned'              ,
    \ ]

let s:MsgInsNotAvail	= "insertion not available for a fold" 
let s:MenuRun         = s:C_Root.'&Run'

"------------------------------------------------------------------------------

let s:C_SourceCodeExtensionsList	= split( s:C_SourceCodeExtensions, '\s\+' )

"------------------------------------------------------------------------------

"------------------------------------------------------------------------------
"  C : C_InitMenus                              {{{1
"  Initialization of C support menus
"------------------------------------------------------------------------------
"
function! C_InitMenus ()
"
" the menu names
"
	let MenuComments     = s:C_Root.'&Comments'
	let MenuStatements   = s:C_Root.'&Statements'
	let MenuIdioms       = s:C_Root.'&Idioms'
	let MenuPreprocessor = s:C_Root.'&Preprocessor'
	let MenuSnippets     = s:C_Root.'S&nippets'
	let MenuCpp          = s:C_Root.'C&++'
	"
	"===============================================================================================
	"----- Menu : C main menu entry -------------------------------------------   {{{2
	"===============================================================================================
	"
	if s:C_MenuHeader == 'yes'
		exe "amenu  ".s:C_Root.'C\/C\+\+                                                       :call C_MenuTitle()<CR>'
		exe "amenu  ".s:C_Root.'-Sep00-                                                        <Nop>'
		exe "amenu  ".MenuComments.'.&Comments<Tab>C\/C\+\+ 		          	                     :call C_MenuTitle()<CR>'
		exe "amenu  ".MenuComments.'.-Sep00-                       					                     <Nop>'
		exe "amenu  ".MenuStatements.'.&Statements<Tab>C\/C\+\+                                  :call C_MenuTitle()<CR>'
		exe "amenu  ".MenuStatements.'.-Sep00-                                                   <Nop>'
		exe "amenu  ".MenuIdioms.'.&Idioms<Tab>C\/C\+\+                                          :call C_MenuTitle()<CR>'
		exe "amenu  ".MenuIdioms.'.-Sep00-                                                       <Nop>'
		exe "amenu  ".MenuPreprocessor.'.&Preprocessor<Tab>C\/C\+\+                              :call C_MenuTitle()<CR>'
		exe "amenu  ".MenuPreprocessor.'.-Sep00-                                                 <Nop>'
		exe "amenu  ".MenuPreprocessor.'.#include\ &Std\.Lib\.<Tab>\\ps.Std\.Lib\.<Tab>C\/C\+\+  :call C_MenuTitle()<CR>'
		exe "amenu  ".MenuPreprocessor.'.#include\ &Std\.Lib\.<Tab>\\ps.-Sep0-                   <Nop>'
		exe "amenu  ".MenuPreprocessor.'.#include\ C&99<Tab>\\pc.C99<Tab>C\/C\+\+                :call C_MenuTitle()<CR>'
		exe "amenu  ".MenuPreprocessor.'.#include\ C&99<Tab>\\pc.-Sep0-                          <Nop>'
		exe "amenu  ".MenuSnippets.'.S&nippets<Tab>C\/C\+\+                                      :call C_MenuTitle()<CR>'
		exe "amenu  ".MenuSnippets.'.-Sep00-                                                     <Nop>'
		exe "amenu  ".MenuCpp.'.C&\+\+<Tab>C\/C\+\+                                              :call C_MenuTitle()<CR>'
		exe "amenu  ".MenuCpp.'.-Sep00-                                                          <Nop>'
		exe "amenu  ".s:MenuRun.'.&Run<Tab>C\/C\+\+                                                :call C_MenuTitle()<CR>'
		exe "amenu  ".s:MenuRun.'.-Sep00-                                                          <Nop>'
	endif
	"
	"===============================================================================================
	"----- Menu : C-Comments --------------------------------------------------   {{{2
	"===============================================================================================
	"
	exe "amenu <silent> ".MenuComments.'.end-of-&line\ comment<Tab>\\cl           :call C_EndOfLineComment( )<CR>'
	exe "vmenu <silent> ".MenuComments.'.end-of-&line\ comment<Tab>\\cl           :call C_EndOfLineComment( )<CR>'

	exe "amenu <silent> ".MenuComments.'.ad&just\ end-of-line\ com\.<Tab>\\cj     :call C_AdjustLineEndComm()<CR>'
	exe "vmenu <silent> ".MenuComments.'.ad&just\ end-of-line\ com\.<Tab>\\cj     :call C_AdjustLineEndComm()<CR>'

	exe "amenu <silent> ".MenuComments.'.&set\ end-of-line\ com\.\ col\.<Tab>\\cs :call C_GetLineEndCommCol()<CR>'

	exe "amenu  ".MenuComments.'.-SEP10-                              :'
	exe "amenu <silent> ".MenuComments.'.code\ ->\ comment\ \/&*\ *\/<Tab>\\c*    :call C_CodeToCommentC()<CR>:nohlsearch<CR>j'
	exe "vmenu <silent> ".MenuComments.'.code\ ->\ comment\ \/&*\ *\/<Tab>\\c*    :call C_CodeToCommentC()<CR>:nohlsearch<CR>j'
	exe "amenu <silent> ".MenuComments.'.code\ ->\ comment\ &\/\/<Tab>\\cc        :call C_CodeToCommentCpp()<CR>:nohlsearch<CR>j'
	exe "vmenu <silent> ".MenuComments.'.code\ ->\ comment\ &\/\/<Tab>\\cc        :call C_CodeToCommentCpp()<CR>:nohlsearch<CR>j'
	exe "amenu <silent> ".MenuComments.'.c&omment\ ->\ code<Tab>\\co              :call C_CommentToCode()<CR>:nohlsearch<CR>'
	exe "vmenu <silent> ".MenuComments.'.c&omment\ ->\ code<Tab>\\co              :call C_CommentToCode()<CR>:nohlsearch<CR>'

	exe "amenu          ".MenuComments.'.-SEP0-                        :'
	exe "amenu <silent> ".MenuComments.'.&frame\ comment<Tab>\\cfr                 :call C_InsertTemplate("comment.frame")<CR>'
	exe "amenu <silent> ".MenuComments.'.f&unction\ description<Tab>\\cfu          :call C_InsertTemplate("comment.function")<CR>'
	exe "amenu          ".MenuComments.'.-SEP1-                        :'
	exe "amenu <silent> ".MenuComments.'.&method\ description<Tab>\\cme            :call C_InsertTemplate("comment.method")<CR>'
	exe "amenu <silent> ".MenuComments.'.cl&ass\ description<Tab>\\ccl             :call C_InsertTemplate("comment.class")<CR>'
	exe "amenu          ".MenuComments.'.-SEP2-                        :'
	exe "amenu <silent> ".MenuComments.'.file\ description\ \(impl\.\)<Tab>\\cfdi  :call C_InsertTemplate("comment.file-description")<CR>'
	exe "amenu <silent> ".MenuComments.'.file\ description\ \(header\)<Tab>\\cfdh  :call C_InsertTemplate("comment.file-description-header")<CR>'
	exe "amenu          ".MenuComments.'.-SEP3-                        :'
	"
	"----- Submenu : C-Comments : file sections  -------------------------------------------------------------
	"
	if s:C_MenuHeader == 'yes'
		exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.file\ sections<Tab>C\/C\+\+   :call C_MenuTitle()<CR>'
		exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.-Sep0-                        <Nop>'
		exe "amenu  ".MenuComments.'.&H-file\ sections<Tab>\\chs.H-file\ sections<Tab>C\/C\+\+        :call C_MenuTitle()<CR>'
		exe "amenu  ".MenuComments.'.&H-file\ sections<Tab>\\chs.-Sep0-                               <Nop>'
		exe "amenu  ".MenuComments.'.-SEP4-                        :'
		exe "amenu  ".MenuComments.'.&keyword\ comm\.<Tab>\\ckc.keyw\.+comm\.<Tab>C\/C\+\+            :call C_MenuTitle()<CR>'
		exe "amenu  ".MenuComments.'.&keyword\ comm\.<Tab>\\ckc.-Sep0-            						        <Nop>'
		exe "amenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.special\ comm\.<Tab>C\/C\+\+          :call C_MenuTitle()<CR>'
		exe "amenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.-Sep0-                				        <Nop>'
		exe "amenu  ".MenuComments.'.ta&gs\ (plugin).tags\ (plugin)<Tab>C\/C\+\+                      :call C_MenuTitle()<CR>'
		exe "amenu  ".MenuComments.'.ta&gs\ (plugin).-Sep0-            							                  <Nop>'
	endif
	"
	exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.&Header\ File\ Includes  :call C_InsertTemplate("comment.file-section-cpp-header-includes")<CR>'
	exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.Local\ &Macros           :call C_InsertTemplate("comment.file-section-cpp-macros")<CR>'
	exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.Local\ &Type\ Def\.      :call C_InsertTemplate("comment.file-section-cpp-typedefs")<CR>'
	exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.Local\ &Data\ Types      :call C_InsertTemplate("comment.file-section-cpp-data-types")<CR>'
	exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.Local\ &Variables        :call C_InsertTemplate("comment.file-section-cpp-local-variables")<CR>'
	exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.Local\ &Prototypes       :call C_InsertTemplate("comment.file-section-cpp-prototypes")<CR>'
	exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.&Exp\.\ Function\ Def\.  :call C_InsertTemplate("comment.file-section-cpp-function-defs-exported")<CR>'
	exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.&Local\ Function\ Def\.  :call C_InsertTemplate("comment.file-section-cpp-function-defs-local")<CR>'
	exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.-SEP6-                   :'
	exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.Local\ &Class\ Def\.     :call C_InsertTemplate("comment.file-section-cpp-class-defs")<CR>'
	exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.E&xp\.\ Class\ Impl\.    :call C_InsertTemplate("comment.file-section-cpp-class-implementations-exported")<CR>'
	exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.L&ocal\ Class\ Impl\.    :call C_InsertTemplate("comment.file-section-cpp-class-implementations-local")<CR>'
	exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.-SEP7-                   :'
	exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.&All\ sections,\ C       :call C_Comment_C_SectionAll("c")<CR>'
	exe "amenu  ".MenuComments.'.&C\/C\+\+-file\ sections<Tab>\\ccs.All\ &sections,\ C++     :call C_Comment_C_SectionAll("cpp")<CR>'
	"
	"----- Submenu : H-Comments : file sections  -------------------------------------------------------------
	"
	"'
	exe "amenu  ".MenuComments.'.&H-file\ sections<Tab>\\chs.&Header\ File\ Includes    :call C_InsertTemplate("comment.file-section-hpp-header-includes")<CR>'
	exe "amenu  ".MenuComments.'.&H-file\ sections<Tab>\\chs.Exported\ &Macros          :call C_InsertTemplate("comment.file-section-hpp-macros")<CR>'
	exe "amenu  ".MenuComments.'.&H-file\ sections<Tab>\\chs.Exported\ &Type\ Def\.     :call C_InsertTemplate("comment.file-section-hpp-exported-typedefs")<CR>'
	exe "amenu  ".MenuComments.'.&H-file\ sections<Tab>\\chs.Exported\ &Data\ Types     :call C_InsertTemplate("comment.file-section-hpp-exported-data-types")<CR>'
	exe "amenu  ".MenuComments.'.&H-file\ sections<Tab>\\chs.Exported\ &Variables       :call C_InsertTemplate("comment.file-section-hpp-exported-variables")<CR>'
	exe "amenu  ".MenuComments.'.&H-file\ sections<Tab>\\chs.Exported\ &Funct\.\ Decl\. :call C_InsertTemplate("comment.file-section-hpp-exported-function-declarations")<CR>'
	exe "amenu  ".MenuComments.'.&H-file\ sections<Tab>\\chs.-SEP4-                     :'
	exe "amenu  ".MenuComments.'.&H-file\ sections<Tab>\\chs.E&xported\ Class\ Def\.    :call C_InsertTemplate("comment.file-section-hpp-exported-class-defs")<CR>'

	exe "amenu  ".MenuComments.'.&H-file\ sections<Tab>\\chs.-SEP5-                     :'
	exe "amenu  ".MenuComments.'.&H-file\ sections<Tab>\\chs.&All\ sections,\ C         :call C_Comment_H_SectionAll("c")<CR>'
	exe "amenu  ".MenuComments.'.&H-file\ sections<Tab>\\chs.All\ &sections,\ C++       :call C_Comment_H_SectionAll("cpp")<CR>'
	"
	exe "amenu  ".MenuComments.'.-SEP8-                        :'
	"
	"----- Submenu : C-Comments : keyword comments  ----------------------------------------------------------
	"
"
	exe "amenu  ".MenuComments.'.&keyword\ comm\.<Tab>\\ckc.\:&BUG\:               $:call C_InsertTemplate("comment.keyword-bug")<CR>'
	exe "amenu  ".MenuComments.'.&keyword\ comm\.<Tab>\\ckc.\:&COMPILER\:          $:call C_InsertTemplate("comment.keyword-compiler")<CR>'
	exe "amenu  ".MenuComments.'.&keyword\ comm\.<Tab>\\ckc.\:&TODO\:              $:call C_InsertTemplate("comment.keyword-todo")<CR>'
	exe "amenu  ".MenuComments.'.&keyword\ comm\.<Tab>\\ckc.\:&WARNING\:           $:call C_InsertTemplate("comment.keyword-warning")<CR>'
	exe "amenu  ".MenuComments.'.&keyword\ comm\.<Tab>\\ckc.\:W&ORKAROUND\:        $:call C_InsertTemplate("comment.keyword-workaround")<CR>'
	exe "amenu  ".MenuComments.'.&keyword\ comm\.<Tab>\\ckc.\:&new\ keyword\:      $:call C_InsertTemplate("comment.keyword-keyword")<CR>'
"
	exe "imenu  ".MenuComments.'.&keyword\ comm\.<Tab>\\ckc.\:&BUG\:          <Esc>$:call C_InsertTemplate("comment.keyword-bug")<CR>'
	exe "imenu  ".MenuComments.'.&keyword\ comm\.<Tab>\\ckc.\:&COMPILER\:     <Esc>$:call C_InsertTemplate("comment.keyword-compiler")<CR>'
	exe "imenu  ".MenuComments.'.&keyword\ comm\.<Tab>\\ckc.\:&TODO\:         <Esc>$:call C_InsertTemplate("comment.keyword-todo")<CR>'
	exe "imenu  ".MenuComments.'.&keyword\ comm\.<Tab>\\ckc.\:&WARNING\:      <Esc>$:call C_InsertTemplate("comment.keyword-warning")<CR>'
	exe "imenu  ".MenuComments.'.&keyword\ comm\.<Tab>\\ckc.\:W&ORKAROUND\:   <Esc>$:call C_InsertTemplate("comment.keyword-workaround")<CR>'
	exe "imenu  ".MenuComments.'.&keyword\ comm\.<Tab>\\ckc.\:&new\ keyword\: <Esc>$:call C_InsertTemplate("comment.keyword-keyword")<CR>'
	"
	"----- Submenu : C-Comments : special comments  ----------------------------------------------------------
	"
	exe "amenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.&EMPTY                													$:call C_InsertTemplate("comment.special-empty")<CR>'
	exe "amenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.&FALL\ THROUGH        													$:call C_InsertTemplate("comment.special-fall-through")             <CR>'
	exe "amenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.&IMPL\.\ TYPE\ CONV   													$:call C_InsertTemplate("comment.special-implicit-type-conversion") <CR>'
	exe "amenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.&NO\ RETURN           													$:call C_InsertTemplate("comment.special-no-return")                <CR>'
	exe "amenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.NOT\ &REACHED         													$:call C_InsertTemplate("comment.special-not-reached")              <CR>'
	exe "amenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.&TO\ BE\ IMPL\.       													$:call C_InsertTemplate("comment.special-remains-to-be-implemented")<CR>'
	exe "amenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.-SEP81-               :'
	exe "amenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.constant\ type\ is\ &long\ (L)              		$:call C_InsertTemplate("comment.special-constant-type-is-long")<CR>'
	exe "amenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.constant\ type\ is\ &unsigned\ (U)          		$:call C_InsertTemplate("comment.special-constant-type-is-unsigned")<CR>'
	exe "amenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.constant\ type\ is\ unsigned\ l&ong\ (UL)   		$:call C_InsertTemplate("comment.special-constant-type-is-unsigned-long")<CR>'
	"
	exe "imenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.&EMPTY                										 <Esc>$:call C_InsertTemplate("comment.special-empty")<CR>'
	exe "imenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.&FALL\ THROUGH        										 <Esc>$:call C_InsertTemplate("comment.special-fall-through")             <CR>'
	exe "imenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.&IMPL\.\ TYPE\ CONV   										 <Esc>$:call C_InsertTemplate("comment.special-implicit-type-conversion") <CR>'
	exe "imenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.&NO\ RETURN           										 <Esc>$:call C_InsertTemplate("comment.special-no-return")                <CR>'
	exe "imenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.NOT\ &REACHED         										 <Esc>$:call C_InsertTemplate("comment.special-not-reached")              <CR>'
	exe "imenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.&TO\ BE\ IMPL\.       										 <Esc>$:call C_InsertTemplate("comment.special-remains-to-be-implemented")<CR>'
	exe "imenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.-SEP81-               :'
	exe "imenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.constant\ type\ is\ &long\ (L)             <Esc>$:call C_InsertTemplate("comment.special-constant-type-is-long")<CR>'
	exe "imenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.constant\ type\ is\ &unsigned\ (U)         <Esc>$:call C_InsertTemplate("comment.special-constant-type-is-unsigned")<CR>'
	exe "imenu  ".MenuComments.'.&special\ comm\.<Tab>\\csc.constant\ type\ is\ unsigned\ l&ong\ (UL)  <Esc>$:call C_InsertTemplate("comment.special-constant-type-is-unsigned-long")<CR>'
	"
	"----- Submenu : C-Comments : Tags  ----------------------------------------------------------
	"
	"
	exe "anoremenu  ".MenuComments.'.ta&gs\ (plugin).&AUTHOR                :call C_InsertMacroValue("AUTHOR")<CR>'
	exe "anoremenu  ".MenuComments.'.ta&gs\ (plugin).&AUTHORREF             :call C_InsertMacroValue("AUTHORREF")<CR>'
	exe "anoremenu  ".MenuComments.'.ta&gs\ (plugin).&COMPANY               :call C_InsertMacroValue("COMPANY")<CR>'
	exe "anoremenu  ".MenuComments.'.ta&gs\ (plugin).&COPYRIGHTHOLDER       :call C_InsertMacroValue("COPYRIGHTHOLDER")<CR>'
	exe "anoremenu  ".MenuComments.'.ta&gs\ (plugin).&EMAIL                 :call C_InsertMacroValue("EMAIL")<CR>'
	exe "anoremenu  ".MenuComments.'.ta&gs\ (plugin).&LICENSE               :call C_InsertMacroValue("LICENSE")<CR>'
	exe "anoremenu  ".MenuComments.'.ta&gs\ (plugin).&ORGANIZATION          :call C_InsertMacroValue("ORGANIZATION")<CR>'
	exe "anoremenu  ".MenuComments.'.ta&gs\ (plugin).&PROJECT               :call C_InsertMacroValue("PROJECT")<CR>'
	"
	exe "inoremenu  ".MenuComments.'.ta&gs\ (plugin).&AUTHOR           <Esc>:call C_InsertMacroValue("AUTHOR")<CR>a'
	exe "inoremenu  ".MenuComments.'.ta&gs\ (plugin).&AUTHORREF        <Esc>:call C_InsertMacroValue("AUTHORREF")<CR>a'
	exe "inoremenu  ".MenuComments.'.ta&gs\ (plugin).&COMPANY          <Esc>:call C_InsertMacroValue("COMPANY")<CR>a'
	exe "inoremenu  ".MenuComments.'.ta&gs\ (plugin).&COPYRIGHTHOLDER  <Esc>:call C_InsertMacroValue("COPYRIGHTHOLDER")<CR>a'
	exe "inoremenu  ".MenuComments.'.ta&gs\ (plugin).&EMAIL            <Esc>:call C_InsertMacroValue("EMAIL")<CR>a'
	exe "inoremenu  ".MenuComments.'.ta&gs\ (plugin).&LICENSE          <Esc>:call C_InsertMacroValue("LICENSE")<CR>a'
	exe "inoremenu  ".MenuComments.'.ta&gs\ (plugin).&ORGANIZATION     <Esc>:call C_InsertMacroValue("ORGANIZATION")<CR>a'
	exe "inoremenu  ".MenuComments.'.ta&gs\ (plugin).&PROJECT          <Esc>:call C_InsertMacroValue("PROJECT")<CR>a'
	"
	exe "vnoremenu  ".MenuComments.'.ta&gs\ (plugin).&AUTHOR          s<Esc>:call C_InsertMacroValue("AUTHOR")<CR>a'
	exe "vnoremenu  ".MenuComments.'.ta&gs\ (plugin).&AUTHORREF       s<Esc>:call C_InsertMacroValue("AUTHORREF")<CR>a'
	exe "vnoremenu  ".MenuComments.'.ta&gs\ (plugin).&COMPANY         s<Esc>:call C_InsertMacroValue("COMPANY")<CR>a'
	exe "vnoremenu  ".MenuComments.'.ta&gs\ (plugin).&COPYRIGHTHOLDER s<Esc>:call C_InsertMacroValue("COPYRIGHTHOLDER")<CR>a'
	exe "vnoremenu  ".MenuComments.'.ta&gs\ (plugin).&EMAIL           s<Esc>:call C_InsertMacroValue("EMAIL")<CR>a'
	exe "vnoremenu  ".MenuComments.'.ta&gs\ (plugin).&LICENSE         s<Esc>:call C_InsertMacroValue("LICENSE")<CR>a'
	exe "vnoremenu  ".MenuComments.'.ta&gs\ (plugin).&ORGANIZATION    s<Esc>:call C_InsertMacroValue("ORGANIZATION")<CR>a'
	exe "vnoremenu  ".MenuComments.'.ta&gs\ (plugin).&PROJECT         s<Esc>:call C_InsertMacroValue("PROJECT")<CR>a'
	"
	exe " menu  ".MenuComments.'.&date<Tab>\\cd                       <Esc>:call C_InsertDateAndTime("d")<CR>'
	exe "imenu  ".MenuComments.'.&date<Tab>\\cd                       <Esc>:call C_InsertDateAndTime("d")<CR>a'
	exe "vmenu  ".MenuComments.'.&date<Tab>\\cd                      s<Esc>:call C_InsertDateAndTime("d")<CR>a'
	exe " menu  ".MenuComments.'.date\ &time<Tab>\\ct                 <Esc>:call C_InsertDateAndTime("dt")<CR>'
	exe "imenu  ".MenuComments.'.date\ &time<Tab>\\ct                 <Esc>:call C_InsertDateAndTime("dt")<CR>a'
	exe "vmenu  ".MenuComments.'.date\ &time<Tab>\\ct                s<Esc>:call C_InsertDateAndTime("dt")<CR>a'

	exe "amenu  ".MenuComments.'.-SEP12-                    :'
	exe "amenu <silent> ".MenuComments.'.\/*\ &xxx\ *\/\ \ <->\ \ \/\/\ xxx<Tab>\\cx   :call C_CommentToggle()<CR>'
	exe "vmenu <silent> ".MenuComments.'.\/*\ &xxx\ *\/\ \ <->\ \ \/\/\ xxx<Tab>\\cx   :call C_CommentToggle()<CR>'
	"
	"===============================================================================================
	"----- Menu : C-Statements-------------------------------------------------   {{{2
	"===============================================================================================
	"
	exe "amenu <silent>".MenuStatements.'.&do\ \{\ \}\ while<Tab>\\sd               :call C_InsertTemplate("statements.do-while")<CR>'
	exe "vmenu <silent>".MenuStatements.'.&do\ \{\ \}\ while<Tab>\\sd          <Esc>:call C_InsertTemplate("statements.do-while", "v")<CR>'
	exe "imenu <silent>".MenuStatements.'.&do\ \{\ \}\ while<Tab>\\sd          <Esc>:call C_InsertTemplate("statements.do-while")<CR>'
	"
	exe "amenu <silent>".MenuStatements.'.f&or<Tab>\\sf                             :call C_InsertTemplate("statements.for")<CR>'
	exe "imenu <silent>".MenuStatements.'.f&or<Tab>\\sf                        <Esc>:call C_InsertTemplate("statements.for")<CR>'
	"
	exe "amenu <silent>".MenuStatements.'.fo&r\ \{\ \}<Tab>\\sfo                    :call C_InsertTemplate("statements.for-block")<CR>'
	exe "vmenu <silent>".MenuStatements.'.fo&r\ \{\ \}<Tab>\\sfo               <Esc>:call C_InsertTemplate("statements.for-block", "v")<CR>'
	exe "imenu <silent>".MenuStatements.'.fo&r\ \{\ \}<Tab>\\sfo               <Esc>:call C_InsertTemplate("statements.for-block")<CR>'
	"
	exe "amenu <silent>".MenuStatements.'.&if<Tab>\\si                              :call C_InsertTemplate("statements.if")<CR>'
	exe "imenu <silent>".MenuStatements.'.&if<Tab>\\si                         <Esc>:call C_InsertTemplate("statements.if")<CR>'
	"
	exe "amenu <silent>".MenuStatements.'.i&f\ \{\ \}<Tab>\\sif                     :call C_InsertTemplate("statements.if-block")<CR>'
	exe "vmenu <silent>".MenuStatements.'.i&f\ \{\ \}<Tab>\\sif                <Esc>:call C_InsertTemplate("statements.if-block", "v")<CR>'
	exe "imenu <silent>".MenuStatements.'.i&f\ \{\ \}<Tab>\\sif                <Esc>:call C_InsertTemplate("statements.if-block")<CR>'

	exe "amenu <silent>".MenuStatements.'.if\ &else<Tab>\\sie                       :call C_InsertTemplate("statements.if-else")<CR>'
	exe "vmenu <silent>".MenuStatements.'.if\ &else<Tab>\\sie                  <Esc>:call C_InsertTemplate("statements.if-else", "v")<CR>'
	exe "imenu <silent>".MenuStatements.'.if\ &else<Tab>\\sie                  <Esc>:call C_InsertTemplate("statements.if-else")<CR>'
	"
	exe "amenu <silent>".MenuStatements.'.if\ \{\ \}\ e&lse\ \{\ \}<Tab>\\sife      :call C_InsertTemplate("statements.if-block-else")<CR>'
	exe "vmenu <silent>".MenuStatements.'.if\ \{\ \}\ e&lse\ \{\ \}<Tab>\\sife <Esc>:call C_InsertTemplate("statements.if-block-else", "v")<CR>'
	exe "imenu <silent>".MenuStatements.'.if\ \{\ \}\ e&lse\ \{\ \}<Tab>\\sife <Esc>:call C_InsertTemplate("statements.if-block-else")<CR>'
	"
	exe "amenu <silent>".MenuStatements.'.&else\ \{\ \}<Tab>\\se                    :call C_InsertTemplate("statements.else-block")<CR>'
	exe "vmenu <silent>".MenuStatements.'.&else\ \{\ \}<Tab>\\se               <Esc>:call C_InsertTemplate("statements.else-block", "v")<CR>'
	exe "imenu <silent>".MenuStatements.'.&else\ \{\ \}<Tab>\\se               <Esc>:call C_InsertTemplate("statements.else-block")<CR>'
	"
	exe "amenu <silent>".MenuStatements.'.&while<Tab>\\sw                           :call C_InsertTemplate("statements.while")<CR>'
	exe "imenu <silent>".MenuStatements.'.&while<Tab>\\sw                      <Esc>:call C_InsertTemplate("statements.while")<CR>'
	"
	exe "amenu <silent>".MenuStatements.'.w&hile\ \{\ \}<Tab>\\swh                  :call C_InsertTemplate("statements.while-block")<CR>'
	exe "vmenu <silent>".MenuStatements.'.w&hile\ \{\ \}<Tab>\\swh             <Esc>:call C_InsertTemplate("statements.while-block", "v")<CR>'
	exe "imenu <silent>".MenuStatements.'.w&hile\ \{\ \}<Tab>\\swh             <Esc>:call C_InsertTemplate("statements.while-block")<CR>'
	"
	exe "amenu <silent>".MenuStatements.'.&switch\ \{\ \}<Tab>\\ss                  :call C_InsertTemplate("statements.switch")<CR>'
	exe "vmenu <silent>".MenuStatements.'.&switch\ \{\ \}<Tab>\\ss             <Esc>:call C_InsertTemplate("statements.switch", "v")<CR>'
	exe "imenu <silent>".MenuStatements.'.&switch\ \{\ \}<Tab>\\ss             <Esc>:call C_InsertTemplate("statements.switch")<CR>'
	"
	exe "amenu  ".MenuStatements.'.&case\ \.\.\.\ break<Tab>\\sc                    :call C_InsertTemplate("statements.case")<CR>'
	exe "imenu  ".MenuStatements.'.&case\ \.\.\.\ break<Tab>\\sc               <Esc>:call C_InsertTemplate("statements.case")<CR>'
	"
	"
	exe "amenu <silent>".MenuStatements.'.&\{\ \}<Tab>\\sb                          :call C_InsertTemplate("statements.block")<CR>'
	exe "vmenu <silent>".MenuStatements.'.&\{\ \}<Tab>\\sb                     <Esc>:call C_InsertTemplate("statements.block", "v")<CR>'
	exe "imenu <silent>".MenuStatements.'.&\{\ \}<Tab>\\sb                     <Esc>:call C_InsertTemplate("statements.block")<CR>'
	"
	"
	"===============================================================================================
	"----- Menu : C-Idioms ----------------------------------------------------   {{{2
	"===============================================================================================
	"
	exe "amenu <silent> ".MenuIdioms.'.&function<Tab>\\if                        :call C_InsertTemplate("idioms.function")<CR>'
	exe "vmenu <silent> ".MenuIdioms.'.&function<Tab>\\if                   <Esc>:call C_InsertTemplate("idioms.function", "v")<CR>'
	exe "imenu <silent> ".MenuIdioms.'.&function<Tab>\\if                   <Esc>:call C_InsertTemplate("idioms.function")<CR>'
	exe "amenu <silent> ".MenuIdioms.'.s&tatic\ function<Tab>\\isf               :call C_InsertTemplate("idioms.function-static")<CR>'
	exe "vmenu <silent> ".MenuIdioms.'.s&tatic\ function<Tab>\\isf          <Esc>:call C_InsertTemplate("idioms.function-static", "v")<CR>'
	exe "imenu <silent> ".MenuIdioms.'.s&tatic\ function<Tab>\\isf          <Esc>:call C_InsertTemplate("idioms.function-static")<CR>'
	exe "amenu <silent> ".MenuIdioms.'.&main<Tab>\\im                            :call C_InsertTemplate("idioms.main")<CR>'
	exe "vmenu <silent> ".MenuIdioms.'.&main<Tab>\\im                       <Esc>:call C_InsertTemplate("idioms.main", "v")<CR>'
	exe "imenu <silent> ".MenuIdioms.'.&main<Tab>\\im                      <Esc>:call C_InsertTemplate("idioms.main")<CR>'

	exe "amenu          ".MenuIdioms.'.-SEP1-                      :'
	exe "amenu          ".MenuIdioms.'.for(x=&0;\ x<n;\ x\+=1)<Tab>\\i0          :call C_CodeFor("up"  )<CR>'
	exe "vmenu          ".MenuIdioms.'.for(x=&0;\ x<n;\ x\+=1)<Tab>\\i0          :call C_CodeFor("up"  )<CR>'
	exe "imenu          ".MenuIdioms.'.for(x=&0;\ x<n;\ x\+=1)<Tab>\\i0     <Esc>:call C_CodeFor("up"  )<CR>i'
	exe "amenu          ".MenuIdioms.'.for(x=&n-1;\ x>=0;\ x\-=1)<Tab>\\in       :call C_CodeFor("down")<CR>'
	exe "vmenu          ".MenuIdioms.'.for(x=&n-1;\ x>=0;\ x\-=1)<Tab>\\in       :call C_CodeFor("down")<CR>'
	exe "imenu          ".MenuIdioms.'.for(x=&n-1;\ x>=0;\ x\-=1)<Tab>\\in  <Esc>:call C_CodeFor("down")<CR>i'

	exe "amenu          ".MenuIdioms.'.-SEP2-                      :'
	exe "amenu <silent> ".MenuIdioms.'.&enum<Tab>\\ie                            :call C_InsertTemplate("idioms.enum")<CR>'
	exe "vmenu <silent> ".MenuIdioms.'.&enum<Tab>\\ie                       <Esc>:call C_InsertTemplate("idioms.enum"  , "v")<CR>'
	exe "imenu <silent> ".MenuIdioms.'.&enum<Tab>\\ie                       <Esc>:call C_InsertTemplate("idioms.enum"  )<CR>'
	exe "amenu <silent> ".MenuIdioms.'.&struct<Tab>\\is                          :call C_InsertTemplate("idioms.struct")<CR>'
	exe "vmenu <silent> ".MenuIdioms.'.&struct<Tab>\\is                     <Esc>:call C_InsertTemplate("idioms.struct", "v")<CR>'
	exe "imenu <silent> ".MenuIdioms.'.&struct<Tab>\\is                     <Esc>:call C_InsertTemplate("idioms.struct")<CR>'
	exe "amenu <silent> ".MenuIdioms.'.&union<Tab>\\iu                           :call C_InsertTemplate("idioms.union")<CR>'
	exe "vmenu <silent> ".MenuIdioms.'.&union<Tab>\\iu                      <Esc>:call C_InsertTemplate("idioms.union" , "v")<CR>'
	exe "imenu <silent> ".MenuIdioms.'.&union<Tab>\\iu                      <Esc>:call C_InsertTemplate("idioms.union" )<CR>'
	exe "amenu          ".MenuIdioms.'.-SEP3-                      :'
	"
	exe "amenu <silent> ".MenuIdioms.'.&scanf<Tab>\\isc                            :call C_InsertTemplate("idioms.scanf")<CR>'
	exe "imenu <silent> ".MenuIdioms.'.&scanf<Tab>\\isc                       <Esc>:call C_InsertTemplate("idioms.scanf")<CR>'
	exe "amenu <silent> ".MenuIdioms.'.&printf<Tab>\\ip                            :call C_InsertTemplate("idioms.printf")<CR>'
	exe "imenu <silent> ".MenuIdioms.'.&printf<Tab>\\ip                       <Esc>:call C_InsertTemplate("idioms.printf")<CR>'
	"
	exe "amenu          ".MenuIdioms.'.-SEP4-                       :'
	exe "amenu <silent> ".MenuIdioms.'.p=&calloc\(n,sizeof(type)\)<Tab>\\ica       :call C_InsertTemplate("idioms.calloc")<CR>'
	exe "imenu <silent> ".MenuIdioms.'.p=&calloc\(n,sizeof(type)\)<Tab>\\ica  <Esc>:call C_InsertTemplate("idioms.calloc")<CR>'
	exe "amenu <silent> ".MenuIdioms.'.p=&malloc\(sizeof(type)\)<Tab>\\ima         :call C_InsertTemplate("idioms.malloc")<CR>'
	exe "imenu <silent> ".MenuIdioms.'.p=&malloc\(sizeof(type)\)<Tab>\\ima    <Esc>:call C_InsertTemplate("idioms.malloc")<CR>'
	exe "amenu <silent> ".MenuIdioms.'.p=&realloc\(p,sizeof(type)\)<Tab>\\ire      :call C_InsertTemplate("idioms.realloc")<CR>'
	exe "imenu <silent> ".MenuIdioms.'.p=&realloc\(p,sizeof(type)\)<Tab>\\ire <Esc>:call C_InsertTemplate("idioms.realloc")<CR>'
	"
	exe "anoremenu <silent> ".MenuIdioms.'.&sizeof(\ \)<Tab>\\isi                 :call C_InsertTemplate("idioms.sizeof")<CR>'
	exe "inoremenu <silent> ".MenuIdioms.'.&sizeof(\ \)<Tab>\\isi            <Esc>:call C_InsertTemplate("idioms.sizeof")<CR>'
	exe "vnoremenu <silent> ".MenuIdioms.'.&sizeof(\ \)<Tab>\\isi            <Esc>:call C_InsertTemplate("idioms.sizeof", "v")<CR>'
	"
	exe "anoremenu <silent> ".MenuIdioms.'.&assert(\ \)<Tab>\\ias                 :call C_InsertTemplate("idioms.assert")<CR>'
	exe "inoremenu <silent> ".MenuIdioms.'.&assert(\ \)<Tab>\\ias            <Esc>:call C_InsertTemplate("idioms.assert")<CR>'
	exe "vnoremenu <silent> ".MenuIdioms.'.&assert(\ \)<Tab>\\ias            <Esc>:call C_InsertTemplate("idioms.assert", "v")<CR>'

	exe "amenu          ".MenuIdioms.'.-SEP5-                      :'
	exe "amenu <silent> ".MenuIdioms.'.open\ &input\ file<Tab>\\ii               :call C_InsertTemplate("idioms.open-input-file")<CR>'
	exe "imenu <silent> ".MenuIdioms.'.open\ &input\ file<Tab>\\ii          <Esc>:call C_InsertTemplate("idioms.open-input-file")<CR>'
	exe "vmenu <silent> ".MenuIdioms.'.open\ &input\ file<Tab>\\ii          <Esc>:call C_InsertTemplate("idioms.open-input-file", "v")<CR>'
	exe "amenu <silent> ".MenuIdioms.'.open\ &output\ file<Tab>\\io              :call C_InsertTemplate("idioms.open-output-file")<CR>'
	exe "imenu <silent> ".MenuIdioms.'.open\ &output\ file<Tab>\\io         <Esc>:call C_InsertTemplate("idioms.open-output-file")<CR>'
	exe "vmenu <silent> ".MenuIdioms.'.open\ &output\ file<Tab>\\io         <Esc>:call C_InsertTemplate("idioms.open-output-file", "v")<CR>'
	"
	exe "amenu <silent> ".MenuIdioms.'.fscanf<Tab>\\ifs                          :call C_InsertTemplate("idioms.fscanf")<CR>'
	exe "imenu <silent> ".MenuIdioms.'.fscanf<Tab>\\ifs                     <Esc>:call C_InsertTemplate("idioms.fscanf")<CR>'
	exe "amenu <silent> ".MenuIdioms.'.fprintf<Tab>\\ifp                         :call C_InsertTemplate("idioms.fprintf")<CR>'
	exe "imenu <silent> ".MenuIdioms.'.fprintf<Tab>\\ifp                    <Esc>:call C_InsertTemplate("idioms.fprintf")<CR>'
	"
	"===============================================================================================
	"----- Menu : C-Preprocessor ----------------------------------------------   {{{2
	"===============================================================================================
	"
	"----- Submenu : C-Idioms: standard library -------------------------------------------------------
	"'
	call C_CIncludeMenus ( MenuPreprocessor.'.#include\ &Std\.Lib\.<Tab>\\ps', s:C_StandardLibs )
	"
	call C_CIncludeMenus ( MenuPreprocessor.'.#include\ C&99<Tab>\\pc', s:C_C99Libs )
	"
	exe "amenu  ".MenuPreprocessor.'.-SEP2-                        :'
	exe "anoremenu  ".MenuPreprocessor.'.#include\ &\<\.\.\.\><Tab>\\p<           :call C_InsertTemplate("preprocessor.include-global")<CR>'
	exe "inoremenu  ".MenuPreprocessor.'.#include\ &\<\.\.\.\><Tab>\\p<      <Esc>:call C_InsertTemplate("preprocessor.include-global")<CR>'
	exe "anoremenu  ".MenuPreprocessor.'.#include\ &\"\.\.\.\"<Tab>\\p"           :call C_InsertTemplate("preprocessor.include-local")<CR>'
	exe "inoremenu  ".MenuPreprocessor.'.#include\ &\"\.\.\.\"<Tab>\\p"      <Esc>:call C_InsertTemplate("preprocessor.include-local")<CR>'
	exe "amenu  ".MenuPreprocessor.'.#&define<Tab>\\pd                            :call C_InsertTemplate("preprocessor.define")<CR>'
	exe "imenu  ".MenuPreprocessor.'.#&define<Tab>\\pd                       <Esc>:call C_InsertTemplate("preprocessor.define")<CR>'
	exe "amenu  ".MenuPreprocessor.'.&#undef<Tab>\\pu                             :call C_InsertTemplate("preprocessor.undefine")<CR>'
	exe "imenu  ".MenuPreprocessor.'.&#undef<Tab>\\pu                        <Esc>:call C_InsertTemplate("preprocessor.undefine")<CR>'
	"
	exe "amenu  ".MenuPreprocessor.'.#&if\ #endif<Tab>\\pif                        :call C_InsertTemplate("preprocessor.if-endif")<CR>'
	exe "imenu  ".MenuPreprocessor.'.#&if\ #endif<Tab>\\pif                   <Esc>:call C_InsertTemplate("preprocessor.if-endif")<CR>'
	exe "vmenu  ".MenuPreprocessor.'.#&if\ #endif<Tab>\\pif                   <Esc>:call C_InsertTemplate("preprocessor.if-endif", "v")<CR>'
	exe "amenu  ".MenuPreprocessor.'.#&if\ #else\ #endif<Tab>\\pie                 :call C_InsertTemplate("preprocessor.if-else-endif")<CR>'
	exe "imenu  ".MenuPreprocessor.'.#&if\ #else\ #endif<Tab>\\pie            <Esc>:call C_InsertTemplate("preprocessor.if-else-endif")<CR>'
	exe "vmenu  ".MenuPreprocessor.'.#&if\ #else\ #endif<Tab>\\pie            <Esc>:call C_InsertTemplate("preprocessor.if-else-endif", "v")<CR>'
	exe "amenu  ".MenuPreprocessor.'.#i&fdef\ #else\ #endif<Tab>\\pid              :call C_InsertTemplate("preprocessor.ifdef-else-endif")<CR>'
	exe "imenu  ".MenuPreprocessor.'.#i&fdef\ #else\ #endif<Tab>\\pid         <Esc>:call C_InsertTemplate("preprocessor.ifdef-else-endif")<CR>'
	exe "vmenu  ".MenuPreprocessor.'.#i&fdef\ #else\ #endif<Tab>\\pid         <Esc>:call C_InsertTemplate("preprocessor.ifdef-else-endif", "v")<CR>'
	exe "amenu  ".MenuPreprocessor.'.#if&ndef\ #else\ #endif<Tab>\\pin             :call C_InsertTemplate("preprocessor.ifndef-else-endif")<CR>'
	exe "imenu  ".MenuPreprocessor.'.#if&ndef\ #else\ #endif<Tab>\\pin        <Esc>:call C_InsertTemplate("preprocessor.ifndef-else-endif")<CR>'
	exe "vmenu  ".MenuPreprocessor.'.#if&ndef\ #else\ #endif<Tab>\\pin        <Esc>:call C_InsertTemplate("preprocessor.ifndef-else-endif", "v")<CR>'
	exe "amenu  ".MenuPreprocessor.'.#ifnd&ef\ #def\ #endif<Tab>\\pind             :call C_InsertTemplate("preprocessor.ifndef-def-endif")<CR>'
	exe "imenu  ".MenuPreprocessor.'.#ifnd&ef\ #def\ #endif<Tab>\\pind        <Esc>:call C_InsertTemplate("preprocessor.ifndef-def-endif")<CR>'
	exe "vmenu  ".MenuPreprocessor.'.#ifnd&ef\ #def\ #endif<Tab>\\pind        <Esc>:call C_InsertTemplate("preprocessor.ifndef-def-endif", "v")<CR>'

	exe "amenu  ".MenuPreprocessor.'.#if\ &0\ #endif<Tab>\\pi0                     :call C_PPIf0("a")<CR>2ji'
	exe "imenu  ".MenuPreprocessor.'.#if\ &0\ #endif<Tab>\\pi0                <Esc>:call C_PPIf0("a")<CR>2ji'
	exe "vmenu  ".MenuPreprocessor.'.#if\ &0\ #endif<Tab>\\pi0                <Esc>:call C_PPIf0("v")<CR>'
	"
	exe "amenu <silent> ".MenuPreprocessor.'.&remove\ #if\ 0\ #endif<Tab>\\pr0             :call C_PPIf0Remove()<CR>'
	exe "imenu <silent> ".MenuPreprocessor.'.&remove\ #if\ 0\ #endif<Tab>\\pr0        <Esc>:call C_PPIf0Remove()<CR>'
	"
	exe "amenu  ".MenuPreprocessor.'.#err&or<Tab>\\pe                             :call C_InsertTemplate("preprocessor.error")<CR>'
	exe "imenu  ".MenuPreprocessor.'.#err&or<Tab>\\pe                        <C-C>:call C_InsertTemplate("preprocessor.error")<CR>'
	exe "amenu  ".MenuPreprocessor.'.#&line<Tab>\\pl                              :call C_InsertTemplate("preprocessor.line")<CR>'
	exe "imenu  ".MenuPreprocessor.'.#&line<Tab>\\pl                         <C-C>:call C_InsertTemplate("preprocessor.line")<CR>'
	exe "amenu  ".MenuPreprocessor.'.#&pragma<Tab>\\pp                            :call C_InsertTemplate("preprocessor.pragma")<CR>'
	exe "imenu  ".MenuPreprocessor.'.#&pragma<Tab>\\pp                       <C-C>:call C_InsertTemplate("preprocessor.pragma")<CR>'
	"
	"===============================================================================================
	"----- Menu : Snippets ----------------------------------------------------   {{{2
	"===============================================================================================
	"
	if !empty(s:C_CodeSnippets)
		exe "amenu  <silent> ".MenuSnippets.'.&read\ code\ snippet<Tab>\\nr       :call C_CodeSnippet("r")<CR>'
		exe "imenu  <silent> ".MenuSnippets.'.&read\ code\ snippet<Tab>\\nr  <C-C>:call C_CodeSnippet("r")<CR>'
		exe "amenu  <silent> ".MenuSnippets.'.&write\ code\ snippet<Tab>\\nw      :call C_CodeSnippet("w")<CR>'
		exe "vmenu  <silent> ".MenuSnippets.'.&write\ code\ snippet<Tab>\\nw <C-C>:call C_CodeSnippet("wv")<CR>'
		exe "imenu  <silent> ".MenuSnippets.'.&write\ code\ snippet<Tab>\\nw <C-C>:call C_CodeSnippet("w")<CR>'
		exe "amenu  <silent> ".MenuSnippets.'.&edit\ code\ snippet<Tab>\\ne       :call C_CodeSnippet("e")<CR>'
		exe "imenu  <silent> ".MenuSnippets.'.&edit\ code\ snippet<Tab>\\ne  <C-C>:call C_CodeSnippet("e")<CR>'
		exe " menu  <silent> ".MenuSnippets.'.-SEP1-								:'
	endif
	exe " menu  <silent> ".MenuSnippets.'.&pick\ up\ func\.\ prototype<Tab>\\nf,\ \\np         :call C_ProtoPick("function")<CR>'
	exe "vmenu  <silent> ".MenuSnippets.'.&pick\ up\ func\.\ prototype<Tab>\\nf,\ \\np         :call C_ProtoPick("function")<CR>'
	exe "imenu  <silent> ".MenuSnippets.'.&pick\ up\ func\.\ prototype<Tab>\\nf,\ \\np    <C-C>:call C_ProtoPick("function")<CR>'
	exe " menu  <silent> ".MenuSnippets.'.&pick\ up\ method\ prototype<Tab>\\nm                :call C_ProtoPick("method")<CR>'
	exe "vmenu  <silent> ".MenuSnippets.'.&pick\ up\ method\ prototype<Tab>\\nm                :call C_ProtoPick("method")<CR>'
	exe "imenu  <silent> ".MenuSnippets.'.&pick\ up\ method\ prototype<Tab>\\nm           <C-C>:call C_ProtoPick("method")<CR>'
	exe " menu  <silent> ".MenuSnippets.'.&insert\ prototype(s)<Tab>\\ni        :call C_ProtoInsert()<CR>'
	exe "imenu  <silent> ".MenuSnippets.'.&insert\ prototype(s)<Tab>\\ni   <C-C>:call C_ProtoInsert()<CR>'
	exe " menu  <silent> ".MenuSnippets.'.&clear\ prototype(s)<Tab>\\nc         :call C_ProtoClear()<CR>'
	exe "imenu  <silent> ".MenuSnippets.'.&clear\ prototype(s)<Tab>\\nc 	 <C-C>:call C_ProtoClear()<CR>'
	exe " menu  <silent> ".MenuSnippets.'.&show\ prototype(s)<Tab>\\ns		      :call C_ProtoShow()<CR>'
	exe "imenu  <silent> ".MenuSnippets.'.&show\ prototype(s)<Tab>\\ns		 <C-C>:call C_ProtoShow()<CR>'

	exe " menu  <silent> ".MenuSnippets.'.-SEP2-									     :'
	exe "amenu  <silent>  ".MenuSnippets.'.edit\ &local\ templates<Tab>\\ntl        :call C_BrowseTemplateFiles("Local")<CR>'
	exe "imenu  <silent>  ".MenuSnippets.'.edit\ &local\ templates<Tab>\\ntl   <C-C>:call C_BrowseTemplateFiles("Local")<CR>'
	if s:installation == 'system'
		exe "amenu  <silent>  ".MenuSnippets.'.edit\ &global\ templates<Tab>\\ntg       :call C_BrowseTemplateFiles("Global")<CR>'
		exe "imenu  <silent>  ".MenuSnippets.'.edit\ &global\ templates<Tab>\\ntg  <C-C>:call C_BrowseTemplateFiles("Global")<CR>'
	endif
	exe "amenu  <silent>  ".MenuSnippets.'.reread\ &templates<Tab>\\ntr             :call C_RereadTemplates("yes")<CR>'
	exe "imenu  <silent>  ".MenuSnippets.'.reread\ &templates<Tab>\\ntr        <C-C>:call C_RereadTemplates("yes")<CR>'
	exe "amenu            ".MenuSnippets.'.switch\ template\ st&yle<Tab>\\nts       :CStyle<Space>'
	exe "imenu            ".MenuSnippets.'.switch\ template\ st&yle<Tab>\\nts  <C-C>:CStyle<Space>'
	"
	"===============================================================================================
	"----- Menu : C++ ---------------------------------------------------------   {{{2
	"===============================================================================================
	"
	exe "anoremenu ".MenuCpp.'.c&in                      :call C_InsertTemplate("cpp.cin")<CR>'
	exe "inoremenu ".MenuCpp.'.c&in                 <Esc>:call C_InsertTemplate("cpp.cin")<CR>'
	exe "anoremenu ".MenuCpp.'.c&out<Tab>\\+co           :call C_InsertTemplate("cpp.cout")<CR>'
	exe "inoremenu ".MenuCpp.'.c&out<Tab>\\+co      <Esc>:call C_InsertTemplate("cpp.cout")<CR>'
	exe "anoremenu ".MenuCpp.'.<<\ &\"\"<Tab>\\+"        :call C_InsertTemplate("cpp.cout-operator")<CR>'
	exe "inoremenu ".MenuCpp.'.<<\ &\"\"<Tab>\\+"   <Esc>:call C_InsertTemplate("cpp.cout-operator")<CR>'
	"
	"----- Submenu : C++ : output manipulators  -------------------------------------------------------
	"
	if s:C_MenuHeader == 'yes'
		exe "amenu ".MenuCpp.'.&output\ manipulators.output\ manip\.<Tab>C\/C\+\+         :call C_MenuTitle()<CR>'
		exe "amenu ".MenuCpp.'.&output\ manipulators.-Sep0-                     		      <Nop>'
		exe "amenu ".MenuCpp.'.ios\ flag&bits.ios\ flags<Tab>C\/C\+\+                     :call C_MenuTitle()<CR>'
		exe "amenu ".MenuCpp.'.ios\ flag&bits.-Sep0-               					              <Nop>'
		exe "amenu ".MenuCpp.'.&#include\ <alg\.\.vec><Tab>\\+ps.alg\.\.vec<Tab>C\/C\+\+  :call C_MenuTitle()<CR>'
		exe "amenu ".MenuCpp.'.&#include\ <alg\.\.vec><Tab>\\+ps.-Sep0-          					<Nop>'
		exe "amenu ".MenuCpp.'.&#include\ <cX><Tab>\\+pc.cX<Tab>C\/C\+\+ 	                :call C_MenuTitle()<CR>'
		exe "amenu ".MenuCpp.'.&#include\ <cX><Tab>\\+pc.-Sep0-        		                <Nop>'
	endif
	"
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &boolalpha                :call C_InsertTemplate("cpp.output-manipulator-boolalpha")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &dec                      :call C_InsertTemplate("cpp.output-manipulator-dec")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &endl                     :call C_InsertTemplate("cpp.output-manipulator-endl")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &fixed                    :call C_InsertTemplate("cpp.output-manipulator-fixed")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ fl&ush                    :call C_InsertTemplate("cpp.output-manipulator-flush")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &hex                      :call C_InsertTemplate("cpp.output-manipulator-hex")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &internal                 :call C_InsertTemplate("cpp.output-manipulator-internal")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &left                     :call C_InsertTemplate("cpp.output-manipulator-left")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &oct                      :call C_InsertTemplate("cpp.output-manipulator-oct")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &right                    :call C_InsertTemplate("cpp.output-manipulator-right")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ s&cientific               :call C_InsertTemplate("cpp.output-manipulator-scientific")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &setbase\(\ \)            :call C_InsertTemplate("cpp.output-manipulator-setbase")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ se&tfill\(\ \)            :call C_InsertTemplate("cpp.output-manipulator-setfill")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ setiosfla&g\(\ \)         :call C_InsertTemplate("cpp.output-manipulator-setiosflags")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ set&precision\(\ \)       :call C_InsertTemplate("cpp.output-manipulator-setprecision")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ set&w\(\ \)               :call C_InsertTemplate("cpp.output-manipulator-setw")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ showb&ase                 :call C_InsertTemplate("cpp.output-manipulator-showbase")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ showpoi&nt                :call C_InsertTemplate("cpp.output-manipulator-showpoint")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ showpos\ \(&1\)           :call C_InsertTemplate("cpp.output-manipulator-showpos")<CR>'
	exe "anoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ uppercase\ \(&2\)         :call C_InsertTemplate("cpp.output-manipulator-uppercase")<CR>'
	"
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &boolalpha           <Esc>:call C_InsertTemplate("cpp.output-manipulator-boolalpha")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &dec                 <Esc>:call C_InsertTemplate("cpp.output-manipulator-dec")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &endl                <Esc>:call C_InsertTemplate("cpp.output-manipulator-endl")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &fixed               <Esc>:call C_InsertTemplate("cpp.output-manipulator-fixed")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ fl&ush               <Esc>:call C_InsertTemplate("cpp.output-manipulator-flush")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &hex                 <Esc>:call C_InsertTemplate("cpp.output-manipulator-hex")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &internal            <Esc>:call C_InsertTemplate("cpp.output-manipulator-internal")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &left                <Esc>:call C_InsertTemplate("cpp.output-manipulator-left")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &oct                 <Esc>:call C_InsertTemplate("cpp.output-manipulator-oct")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &right               <Esc>:call C_InsertTemplate("cpp.output-manipulator-right")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ s&cientific          <Esc>:call C_InsertTemplate("cpp.output-manipulator-scientific")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ &setbase\(\ \)       <Esc>:call C_InsertTemplate("cpp.output-manipulator-setbase")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ se&tfill\(\ \)       <Esc>:call C_InsertTemplate("cpp.output-manipulator-setfill")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ setiosfla&g\(\ \)    <Esc>:call C_InsertTemplate("cpp.output-manipulator-setiosflags")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ set&precision\(\ \)  <Esc>:call C_InsertTemplate("cpp.output-manipulator-setprecision")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ set&w\(\ \)          <Esc>:call C_InsertTemplate("cpp.output-manipulator-setw")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ showb&ase            <Esc>:call C_InsertTemplate("cpp.output-manipulator-showbase")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ showpoi&nt           <Esc>:call C_InsertTemplate("cpp.output-manipulator-showpoint")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ showpos\ \(&1\)      <Esc>:call C_InsertTemplate("cpp.output-manipulator-showpos")<CR>'
	exe "inoremenu ".MenuCpp.'.&output\ manipulators.\<\<\ uppercase\ \(&2\)    <Esc>:call C_InsertTemplate("cpp.output-manipulator-uppercase")<CR>'
	"
	"----- Submenu : C++ : ios flag bits  -------------------------------------------------------------
	"
	"
	call C_CIosFlagMenus ( MenuCpp.'.ios\ flag&bits', s:Cpp_IosFlagBits )
	"
	"----- Submenu : C++   library  (algorithm - locale) ----------------------------------------------
	"
	call C_CIncludeMenus ( MenuCpp.'.&#include\ <alg\.\.vec><Tab>\\+ps', s:Cpp_StandardLibs )
	"
	"----- Submenu : C     library  (cassert - ctime) -------------------------------------------------
	"
	call C_CIncludeMenus ( MenuCpp.'.&#include\ <cX><Tab>\\+pc', s:Cpp_CStandardLibs )
	"
	"----- End Submenu : C     library  (cassert - ctime) ---------------------------------------------
	"
	exe "amenu <silent> ".MenuCpp.'.-SEP2-                        :'

	exe "amenu <silent> ".MenuCpp.'.&class<Tab>\\+c                              :call C_InsertTemplate("cpp.class-definition")<CR>'
	exe "imenu <silent> ".MenuCpp.'.&class<Tab>\\+c                         <Esc>:call C_InsertTemplate("cpp.class-definition")<CR>'
	exe "amenu <silent> ".MenuCpp.'.class\ (w\.\ &new)<Tab>\\+cn                 :call C_InsertTemplate("cpp.class-using-new-definition")<CR>'
	exe "imenu <silent> ".MenuCpp.'.class\ (w\.\ &new)<Tab>\\+cn            <Esc>:call C_InsertTemplate("cpp.class-using-new-definition")<CR>'
	exe "amenu <silent> ".MenuCpp.'.&templ\.\ class<Tab>\\+tc                    :call C_InsertTemplate("cpp.template-class-definition")<CR>'
	exe "imenu <silent> ".MenuCpp.'.&templ\.\ class<Tab>\\+tc               <Esc>:call C_InsertTemplate("cpp.template-class-definition")<CR>'
	exe "amenu <silent> ".MenuCpp.'.templ\.\ class\ (w\.\ ne&w)<Tab>\\+tcn       :call C_InsertTemplate("cpp.template-class-using-new-definition")<CR>'
	exe "imenu <silent> ".MenuCpp.'.templ\.\ class\ (w\.\ ne&w)<Tab>\\+tcn  <Esc>:call C_InsertTemplate("cpp.template-class-using-new-definition")<CR>'

	"
	"----- Submenu : C++ : IMPLEMENTATION  -------------------------------------------------------
	"
	if s:C_MenuHeader == 'yes'
		exe "amenu ".MenuCpp.'.IM&PLEMENTATION.IMPLEMENT\.<Tab>C\/C\+\+   :call C_MenuTitle()<CR>'
		exe "amenu ".MenuCpp.'.IM&PLEMENTATION.-Sep0-                     <Nop>'
	endif
	"
	exe "amenu <silent> ".MenuCpp.'.IM&PLEMENTATION.&class<Tab>\\+ci             					     :call C_InsertTemplate("cpp.class-implementation")<CR>'
	exe "imenu <silent> ".MenuCpp.'.IM&PLEMENTATION.&class<Tab>\\+ci             					<Esc>:call C_InsertTemplate("cpp.class-implementation")<CR>'
	exe "amenu <silent> ".MenuCpp.'.IM&PLEMENTATION.class\ (w\.\ &new)<Tab>\\+cni    			     :call C_InsertTemplate("cpp.class-using-new-implementation")<CR>'
	exe "imenu <silent> ".MenuCpp.'.IM&PLEMENTATION.class\ (w\.\ &new)<Tab>\\+cni    			<Esc>:call C_InsertTemplate("cpp.class-using-new-implementation")<CR>'
	exe "amenu <silent> ".MenuCpp.'.IM&PLEMENTATION.&method<Tab>\\+mi                   	     :call C_InsertTemplate("cpp.method-implementation")<CR>'
	exe "imenu <silent> ".MenuCpp.'.IM&PLEMENTATION.&method<Tab>\\+mi                   	<Esc>:call C_InsertTemplate("cpp.method-implementation")<CR>'
	exe "amenu <silent> ".MenuCpp.'.IM&PLEMENTATION.&accessor<Tab>\\+ai                		     :call C_InsertTemplate("cpp.accessor-implementation")<CR>'
	exe "imenu <silent> ".MenuCpp.'.IM&PLEMENTATION.&accessor<Tab>\\+ai                		<Esc>:call C_InsertTemplate("cpp.accessor-implementation")<CR>'
	"
	exe "amenu <silent> ".MenuCpp.'.IM&PLEMENTATION.-SEP21-                   	:'
	exe "imenu <silent> ".MenuCpp.'.IM&PLEMENTATION.&templ\.\ class<Tab>\\+tci            	<Esc>:call C_InsertTemplate("cpp.template-class-implementation")<CR>'
	exe "amenu <silent> ".MenuCpp.'.IM&PLEMENTATION.&templ\.\ class<Tab>\\+tci            	     :call C_InsertTemplate("cpp.template-class-implementation")<CR>'
	exe "imenu <silent> ".MenuCpp.'.IM&PLEMENTATION.templ\.\ class\ (w\.\ ne&w)<Tab>\\+tcni <Esc>:call C_InsertTemplate("cpp.template-class-using-new-implementation")<CR>'
	exe "amenu <silent> ".MenuCpp.'.IM&PLEMENTATION.templ\.\ class\ (w\.\ ne&w)<Tab>\\+tcni      :call C_InsertTemplate("cpp.template-class-using-new-implementation")<CR>'
	exe "amenu <silent> ".MenuCpp.'.IM&PLEMENTATION.templ\.\ m&ethod<Tab>\\+tmi           	     :call C_InsertTemplate("cpp.template-method-implementation")<CR>'
	exe "imenu <silent> ".MenuCpp.'.IM&PLEMENTATION.templ\.\ m&ethod<Tab>\\+tmi           	<Esc>:call C_InsertTemplate("cpp.template-method-implementation")<CR>'
	exe "amenu <silent> ".MenuCpp.'.IM&PLEMENTATION.templ\.\ a&ccessor<Tab>\\+tai         	     :call C_InsertTemplate("cpp.template-accessor-implementation")<CR>'
	exe "imenu <silent> ".MenuCpp.'.IM&PLEMENTATION.templ\.\ a&ccessor<Tab>\\+tai         	<Esc>:call C_InsertTemplate("cpp.template-accessor-implementation")<CR>'
	"
	exe "amenu <silent> ".MenuCpp.'.IM&PLEMENTATION.-SEP22-                     :'
	exe "amenu <silent> ".MenuCpp.'.IM&PLEMENTATION.operator\ &<<                    :call C_InsertTemplate("cpp.operator-in")<CR>'
	exe "imenu <silent> ".MenuCpp.'.IM&PLEMENTATION.operator\ &<<               <Esc>:call C_InsertTemplate("cpp.operator-in")<CR>'
	exe "amenu <silent> ".MenuCpp.'.IM&PLEMENTATION.operator\ &>>                    :call C_InsertTemplate("cpp.operator-out")<CR>'
	exe "imenu <silent> ".MenuCpp.'.IM&PLEMENTATION.operator\ &>>               <Esc>:call C_InsertTemplate("cpp.operator-out")<CR>'
	"
	"----- End Submenu : C++ : IMPLEMENTATION  -------------------------------------------------------
	"
	exe "amenu <silent> ".MenuCpp.'.-SEP31-                       :'
	exe "amenu <silent> ".MenuCpp.'.templ\.\ &function<Tab>\\+tf                 :call C_InsertTemplate("cpp.template-function")<CR>'
	exe "imenu <silent> ".MenuCpp.'.templ\.\ &function<Tab>\\+tf            <Esc>:call C_InsertTemplate("cpp.template-function")<CR>'
	exe "amenu <silent> ".MenuCpp.'.&error\ class<Tab>\\+ec                      :call C_InsertTemplate("cpp.error-class")<CR>'
	exe "imenu <silent> ".MenuCpp.'.&error\ class<Tab>\\+ec                 <Esc>:call C_InsertTemplate("cpp.error-class")<CR>'

	exe "amenu <silent> ".MenuCpp.'.-SEP5-                        :'
	exe "amenu <silent> ".MenuCpp.'.tr&y\ \.\.\ catch<Tab>\\+tr                  :call C_InsertTemplate("cpp.try-catch")<CR>'
	exe "imenu <silent> ".MenuCpp.'.tr&y\ \.\.\ catch<Tab>\\+tr             <Esc>:call C_InsertTemplate("cpp.try-catch")<CR>'
	exe "vmenu <silent> ".MenuCpp.'.tr&y\ \.\.\ catch<Tab>\\+tr             <Esc>:call C_InsertTemplate("cpp.try-catch", "v")<CR>'
	exe "amenu <silent> ".MenuCpp.'.catc&h<Tab>\\+ca                             :call C_InsertTemplate("cpp.catch")<CR>'
	exe "imenu <silent> ".MenuCpp.'.catc&h<Tab>\\+ca                        <Esc>:call C_InsertTemplate("cpp.catch")<CR>'
	exe "vmenu <silent> ".MenuCpp.'.catc&h<Tab>\\+ca                        <Esc>:call C_InsertTemplate("cpp.catch", "v")<CR>'

	exe "amenu <silent> ".MenuCpp.'.catch\(&\.\.\.\)<Tab>\\+c\.                   :call C_InsertTemplate("cpp.catch-points")<CR>'
	exe "imenu <silent> ".MenuCpp.'.catch\(&\.\.\.\)<Tab>\\+c\.              <Esc>:call C_InsertTemplate("cpp.catch-points")<CR>'
	exe "vmenu <silent> ".MenuCpp.'.catch\(&\.\.\.\)<Tab>\\+c\.              <Esc>:call C_InsertTemplate("cpp.catch-points", "v")<CR>'

	exe "amenu <silent> ".MenuCpp.'.-SEP6-                        :'
	exe "amenu <silent> ".MenuCpp.'.open\ input\ file\ \ \(&4\)        :call C_InsertTemplate("cpp.open-input-file")<CR>'
	exe "imenu <silent> ".MenuCpp.'.open\ input\ file\ \ \(&4\)   <Esc>:call C_InsertTemplate("cpp.open-input-file")<CR>'
	exe "vmenu <silent> ".MenuCpp.'.open\ input\ file\ \ \(&4\)   <Esc>:call C_InsertTemplate("cpp.open-input-file", "v")<CR>'
	exe "amenu <silent> ".MenuCpp.'.open\ output\ file\ \(&5\)         :call C_InsertTemplate("cpp.open-output-file")<CR>'
	exe "imenu <silent> ".MenuCpp.'.open\ output\ file\ \(&5\)    <Esc>:call C_InsertTemplate("cpp.open-output-file")<CR>'
	exe "vmenu <silent> ".MenuCpp.'.open\ output\ file\ \(&5\)    <Esc>:call C_InsertTemplate("cpp.open-output-file", "v")<CR>'
	exe "amenu <silent> ".MenuCpp.'.-SEP7-                        :'

	exe "amenu <silent> ".MenuCpp.'.&using\ namespace\ std;            :call C_InsertTemplate("cpp.namespace-std")<CR>'
	exe "imenu <silent> ".MenuCpp.'.&using\ namespace\ std;       <Esc>:call C_InsertTemplate("cpp.namespace-std")<CR>'
	exe "amenu <silent> ".MenuCpp.'.u&sing\ namespace\ ???;            :call C_InsertTemplate("cpp.namespace")<CR>'
	exe "imenu <silent> ".MenuCpp.'.u&sing\ namespace\ ???;       <Esc>:call C_InsertTemplate("cpp.namespace")<CR>'

	exe "amenu <silent> ".MenuCpp.'.names&pace\ ???\ \{\ \}            :call C_InsertTemplate("cpp.namespace-block")<CR>'
	exe "imenu <silent> ".MenuCpp.'.names&pace\ ???\ \{\ \}       <Esc>:call C_InsertTemplate("cpp.namespace-block")<CR>'
	exe "vmenu <silent> ".MenuCpp.'.names&pace\ ???\ \{\ \}       <Esc>:call C_InsertTemplate("cpp.namespace-block", "v")<CR>'
	exe "amenu <silent> ".MenuCpp.'.namespace\ &alias\ =\ ???          :call C_InsertTemplate("cpp.namespace-alias")<CR>'
	exe "imenu <silent> ".MenuCpp.'.namespace\ &alias\ =\ ???     <Esc>:call C_InsertTemplate("cpp.namespace-alias")<CR>'

	exe "amenu <silent> ".MenuCpp.'.-SEP8-              :'
	"
	"----- Submenu : RTTI  ----------------------------------------------------------------------------
	"
	if s:C_MenuHeader == 'yes'
		exe "amenu ".MenuCpp.'.&RTTI.RTTI<Tab>C\/C\+\+      :call C_MenuTitle()<CR>'
		exe "amenu ".MenuCpp.'.&RTTI.-Sep0-                 <Nop>'
	endif
	"
	exe "anoremenu ".MenuCpp.'.&RTTI.&typeid                     :call C_InsertTemplate("cpp.rtti-typeid")<CR>'
	exe "anoremenu ".MenuCpp.'.&RTTI.&static_cast                :call C_InsertTemplate("cpp.rtti-static-cast")<CR>'
	exe "anoremenu ".MenuCpp.'.&RTTI.&const_cast                 :call C_InsertTemplate("cpp.rtti-const-cast")<CR>'
	exe "anoremenu ".MenuCpp.'.&RTTI.&reinterpret_cast           :call C_InsertTemplate("cpp.rtti-reinterpret-cast")<CR>'
	exe "anoremenu ".MenuCpp.'.&RTTI.&dynamic_cast               :call C_InsertTemplate("cpp.rtti-dynamic-cast")<CR>'
	"
	exe "inoremenu ".MenuCpp.'.&RTTI.&typeid                <Esc>:call C_InsertTemplate("cpp.rtti-typeid")<CR>'
	exe "inoremenu ".MenuCpp.'.&RTTI.&static_cast           <Esc>:call C_InsertTemplate("cpp.rtti-static-cast")<CR>'
	exe "inoremenu ".MenuCpp.'.&RTTI.&const_cast            <Esc>:call C_InsertTemplate("cpp.rtti-const-cast")<CR>'
	exe "inoremenu ".MenuCpp.'.&RTTI.&reinterpret_cast      <Esc>:call C_InsertTemplate("cpp.rtti-reinterpret-cast")<CR>'
	exe "inoremenu ".MenuCpp.'.&RTTI.&dynamic_cast          <Esc>:call C_InsertTemplate("cpp.rtti-dynamic-cast")<CR>'
	"
	exe "vnoremenu ".MenuCpp.'.&RTTI.&typeid                <Esc>:call C_InsertTemplate("cpp.rtti-typeid", "v")<CR>'
	exe "vnoremenu ".MenuCpp.'.&RTTI.&static_cast           <Esc>:call C_InsertTemplate("cpp.rtti-static-cast", "v")<CR>'
	exe "vnoremenu ".MenuCpp.'.&RTTI.&const_cast            <Esc>:call C_InsertTemplate("cpp.rtti-const-cast", "v")<CR>'
	exe "vnoremenu ".MenuCpp.'.&RTTI.&reinterpret_cast      <Esc>:call C_InsertTemplate("cpp.rtti-reinterpret-cast", "v")<CR>'
	exe "vnoremenu ".MenuCpp.'.&RTTI.&dynamic_cast          <Esc>:call C_InsertTemplate("cpp.rtti-dynamic-cast", "v")<CR>'
	"
	"----- End Submenu : RTTI  ------------------------------------------------------------------------
	"
	exe "amenu  <silent>".MenuCpp.'.e&xtern\ \"C\"\ \{\ \}       :call C_InsertTemplate("cpp.extern")<CR>'
	exe "imenu  <silent>".MenuCpp.'.e&xtern\ \"C\"\ \{\ \}  <Esc>:call C_InsertTemplate("cpp.extern")<CR>'
	exe "vmenu  <silent>".MenuCpp.'.e&xtern\ \"C\"\ \{\ \}  <Esc>:call C_InsertTemplate("cpp.extern", "v")<CR>'
	"
	"===============================================================================================
	"----- Menu : run  ----- --------------------------------------------------   {{{2
	"===============================================================================================
	"
	exe "amenu  <silent>  ".s:MenuRun.'.save\ and\ &compile<Tab>\\rc\ \ \<A-F9\>         :call C_Compile()<CR>:call C_HlMessage()<CR>'
	exe "imenu  <silent>  ".s:MenuRun.'.save\ and\ &compile<Tab>\\rc\ \ \<A-F9\>    <C-C>:call C_Compile()<CR>:call C_HlMessage()<CR>'
	exe "amenu  <silent>  ".s:MenuRun.'.&link<Tab>\\rl\ \ \ \ \<F9\>                     :call C_Link()<CR>:call C_HlMessage()<CR>'
	exe "imenu  <silent>  ".s:MenuRun.'.&link<Tab>\\rl\ \ \ \ \<F9\>                <C-C>:call C_Link()<CR>:call C_HlMessage()<CR>'
	exe "amenu  <silent>  ".s:MenuRun.'.&run<Tab>\\rr\ \ \<C-F9\>                        :call C_Run()<CR>'
	exe "imenu  <silent>  ".s:MenuRun.'.&run<Tab>\\rr\ \ \<C-F9\>                   <C-C>:call C_Run()<CR>'
	exe "amenu  <silent>  ".s:MenuRun.'.cmd\.\ line\ &arg\.<Tab>\\ra\ \ \<S-F9\>         :call C_Arguments()<CR>'
	exe "imenu  <silent>  ".s:MenuRun.'.cmd\.\ line\ &arg\.<Tab>\\ra\ \ \<S-F9\>    <C-C>:call C_Arguments()<CR>'
	"
	exe "amenu  <silent>  ".s:MenuRun.'.-SEP0-                            :'
	exe "amenu  <silent>  ".s:MenuRun.'.&make<Tab>\\rm                                    :call C_Make()<CR>'
	exe "imenu  <silent>  ".s:MenuRun.'.&make<Tab>\\rm                               <C-C>:call C_Make()<CR>'
	exe "amenu  <silent>  ".s:MenuRun.'.&choose\ makefile<Tab>\\rcm                       :call C_ChooseMakefile()<CR>'
	exe "imenu  <silent>  ".s:MenuRun.'.&choose\ makefile<Tab>\\rcm                  <C-C>:call C_ChooseMakefile()<CR>'
	exe "amenu  <silent>  ".s:MenuRun.'.executable\ to\ run<Tab>\\rme                     :call C_MakeExeToRun()<CR>'
	exe "imenu  <silent>  ".s:MenuRun.'.executable\ to\ run<Tab>\\rme                <C-C>:call C_MakeExeToRun()<CR>'
	exe "amenu  <silent>  ".s:MenuRun.'.&make\ clean<Tab>\\rmc                            :call C_MakeClean()<CR>'
	exe "imenu  <silent>  ".s:MenuRun.'.&make\ clean<Tab>\\rmc                       <C-C>:call C_MakeClean()<CR>'
	exe "amenu  <silent>  ".s:MenuRun.'.cmd\.\ line\ ar&g\.\ for\ make<Tab>\\rma          :call C_MakeArguments()<CR>'
	exe "imenu  <silent>  ".s:MenuRun.'.cmd\.\ line\ ar&g\.\ for\ make<Tab>\\rma     <C-C>:call C_MakeArguments()<CR>'
	"
	exe "amenu  <silent>  ".s:MenuRun.'.-SEP1-                            :'
	"
	if s:C_SplintIsExecutable==1
		exe "amenu  <silent>  ".s:MenuRun.'.s&plint<Tab>\\rp                                :call C_SplintCheck()<CR>:call C_HlMessage()<CR>'
		exe "imenu  <silent>  ".s:MenuRun.'.s&plint<Tab>\\rp                           <C-C>:call C_SplintCheck()<CR>:call C_HlMessage()<CR>'
		exe "amenu  <silent>  ".s:MenuRun.'.cmd\.\ line\ arg\.\ for\ spl&int<Tab>\\rpa      :call C_SplintArguments()<CR>'
		exe "imenu  <silent>  ".s:MenuRun.'.cmd\.\ line\ arg\.\ for\ spl&int<Tab>\\rpa <C-C>:call C_SplintArguments()<CR>'
		exe "amenu  <silent>  ".s:MenuRun.'.-SEP2-                          :'
	endif
	"
	if s:C_CodeCheckIsExecutable==1
		exe "amenu  <silent>  ".s:MenuRun.'.CodeChec&k<Tab>\\rk                                :call C_CodeCheck()<CR>:call C_HlMessage()<CR>'
		exe "imenu  <silent>  ".s:MenuRun.'.CodeChec&k<Tab>\\rk                           <C-C>:call C_CodeCheck()<CR>:call C_HlMessage()<CR>'
		exe "amenu  <silent>  ".s:MenuRun.'.cmd\.\ line\ arg\.\ for\ Cod&eCheck<Tab>\\rka      :call C_CodeCheckArguments()<CR>'
		exe "imenu  <silent>  ".s:MenuRun.'.cmd\.\ line\ arg\.\ for\ Cod&eCheck<Tab>\\rka <C-C>:call C_CodeCheckArguments()<CR>'
		exe "amenu  <silent>  ".s:MenuRun.'.-SEP3-                          :'
	endif
	"
	exe "amenu    <silent>  ".s:MenuRun.'.in&dent<Tab>\\rd                                  :call C_Indent()<CR>'
	exe "imenu    <silent>  ".s:MenuRun.'.in&dent<Tab>\\rd                             <C-C>:call C_Indent()<CR>'
	if	s:MSWIN
		exe "amenu  <silent>  ".s:MenuRun.'.&hardcopy\ to\ printer<Tab>\\rh                 :call C_Hardcopy()<CR>'
		exe "imenu  <silent>  ".s:MenuRun.'.&hardcopy\ to\ printer<Tab>\\rh            <C-C>:call C_Hardcopy()<CR>'
		exe "vmenu  <silent>  ".s:MenuRun.'.&hardcopy\ to\ printer<Tab>\\rh                 :call C_Hardcopy()<CR>'
	else
		exe "amenu  <silent>  ".s:MenuRun.'.&hardcopy\ to\ FILENAME\.ps<Tab>\\rh            :call C_Hardcopy()<CR>'
		exe "imenu  <silent>  ".s:MenuRun.'.&hardcopy\ to\ FILENAME\.ps<Tab>\\rh       <C-C>:call C_Hardcopy()<CR>'
		exe "vmenu  <silent>  ".s:MenuRun.'.&hardcopy\ to\ FILENAME\.ps<Tab>\\rh            :call C_Hardcopy()<CR>'
	endif
	exe "imenu  <silent>  ".s:MenuRun.'.-SEP4-                           :'

	exe "amenu  <silent>  ".s:MenuRun.'.&settings<Tab>\\rs                                :call C_Settings()<CR>'
	exe "imenu  <silent>  ".s:MenuRun.'.&settings<Tab>\\rs                           <C-C>:call C_Settings()<CR>'
	exe "imenu  <silent>  ".s:MenuRun.'.-SEP5-                           :'

	if	!s:MSWIN
		exe "amenu  <silent>  ".s:MenuRun.'.&xterm\ size<Tab>\\rx                           :call C_XtermSize()<CR>'
		exe "imenu  <silent>  ".s:MenuRun.'.&xterm\ size<Tab>\\rx                      <C-C>:call C_XtermSize()<CR>'
	endif
	if s:C_OutputGvim == "vim"
		exe "amenu  <silent>  ".s:MenuRun.'.&output:\ VIM->buffer->xterm<Tab>\\ro           :call C_Toggle_Gvim_Xterm()<CR><CR>'
		exe "imenu  <silent>  ".s:MenuRun.'.&output:\ VIM->buffer->xterm<Tab>\\ro      <C-C>:call C_Toggle_Gvim_Xterm()<CR><CR>'
	else
		if s:C_OutputGvim == "buffer"
			exe "amenu  <silent>  ".s:MenuRun.'.&output:\ BUFFER->xterm->vim<Tab>\\ro         :call C_Toggle_Gvim_Xterm()<CR><CR>'
			exe "imenu  <silent>  ".s:MenuRun.'.&output:\ BUFFER->xterm->vim<Tab>\\ro    <C-C>:call C_Toggle_Gvim_Xterm()<CR><CR>'
		else
			exe "amenu  <silent>  ".s:MenuRun.'.&output:\ XTERM->vim->buffer<Tab>\\ro         :call C_Toggle_Gvim_Xterm()<CR><CR>'
			exe "imenu  <silent>  ".s:MenuRun.'.&output:\ XTERM->vim->buffer<Tab>\\ro    <C-C>:call C_Toggle_Gvim_Xterm()<CR><CR>'
		endif
	endif
	"
	"===============================================================================================
	"----- Menu : help  -------------------------------------------------------   {{{2
	"===============================================================================================
	"
	exe " menu  <silent>  ".s:C_Root.'&help\ (C-Support)<Tab>\\hp        :call C_HelpCsupport()<CR>'
	exe "imenu  <silent>  ".s:C_Root.'&help\ (C-Support)<Tab>\\hp   <C-C>:call C_HelpCsupport()<CR>'
	exe " menu  <silent>  ".s:C_Root.'show\ &manual<Tab>\\hm   		       :call C_Help("m")<CR>'
	exe "imenu  <silent>  ".s:C_Root.'show\ &manual<Tab>\\hm 		    <C-C>:call C_Help("m")<CR>'

endfunction    " ----------  end of function  C_InitMenus  ----------
"
function! C_MenuTitle ()
		echohl WarningMsg | echo "This is a menu title." | echohl None
endfunction    " ----------  end of function C_MenuTitle  ----------
"
"===============================================================================================
"----- Menu Functions --------------------------------------------------------------------------
"===============================================================================================
"
let s:C_StandardLibs       = [
  \ '&assert\.h' , '&ctype\.h' ,   '&errno\.h' ,
  \ '&float\.h' ,  '&limits\.h' ,  'l&ocale\.h' ,
  \ '&math\.h' ,   'set&jmp\.h' ,  's&ignal\.h' ,
  \ 'stdar&g\.h' , 'st&ddef\.h' ,  '&stdio\.h' ,
  \ 'stdli&b\.h' , 'st&ring\.h' ,  '&time\.h' ,
  \ ]
"
let s:C_C99Libs       = [
  \ '&complex\.h', '&fenv\.h',    '&inttypes\.h',
  \ 'is&o646\.h',  '&stdbool\.h', 's&tdint\.h',
  \ 'tg&math\.h',  '&wchar\.h',   'wct&ype\.h',
  \ ]
"
let s:Cpp_StandardLibs       = [
  \ '&algorithm', '&bitset',    '&complex',    '&deque',
  \ '&exception', '&fstream',   'f&unctional', 'iomani&p',
  \ '&ios',       'iosf&wd',    'io&stream',   'istrea&m',
  \ 'iterato&r',  '&limits',    'lis&t',       'l&ocale',
  \ '&map',       'memor&y',    '&new',        'numeri&c',
  \ '&ostream',   '&queue',     '&set',        'sst&ream',
  \ 'st&ack',     'stde&xcept', 'stream&buf',  'str&ing',
  \ '&typeinfo',  '&utility',   '&valarray',   'v&ector',
  \ ]
"
let s:Cpp_CStandardLibs       = [
  \ 'c&assert', 'c&ctype',  'c&errno',  'c&float',
  \ 'c&limits', 'cl&ocale', 'c&math',   'cset&jmp',
  \ 'cs&ignal', 'cstdar&g', 'cst&ddef', 'c&stdio',
  \ 'cstdli&b', 'cst&ring', 'c&time',
  \ ]

let s:Cpp_IosFlagBits       = [
	\	'ios::&adjustfield', 'ios::bas&efield',           'ios::&boolalpha',
	\	'ios::&dec',         'ios::&fixed',               'ios::floa&tfield',
	\	'ios::&hex',         'ios::&internal',            'ios::&left',
	\	'ios::&oct',         'ios::&right',               'ios::s&cientific',
	\	'ios::sho&wbase',    'ios::showpoint\ \(&1\)',    'ios::show&pos',
	\	'ios::&skipws',      'ios::u&nitbuf',             'ios::&uppercase',
  \ ]

"------------------------------------------------------------------------------
"  C_CIncludeMenus: generate the C/C++-standard library menu entries   {{{1
"------------------------------------------------------------------------------
function! C_CIncludeMenus ( menupath, liblist )
	for item in a:liblist
		let replacement	= substitute( item, '[&\\]*', '','g' )
		exe "anoremenu  ".a:menupath.'.'.item.'         i#include<Space><'.replacement.'>'
		exe "inoremenu  ".a:menupath.'.'.item.'          #include<Space><'.replacement.'>'
	endfor
	return
endfunction    " ----------  end of function C_CIncludeMenus  ----------

"------------------------------------------------------------------------------
"  C_CIosFlagMenus: generate the C++ ios flags menu entries   {{{1
"------------------------------------------------------------------------------
function! C_CIosFlagMenus ( menupath, flaglist )
	for item in a:flaglist
		let replacement	= substitute( item, '[^[:alpha:]:]', '','g' )
		exe " noremenu ".a:menupath.'.'.item.'     i'.replacement
		exe "inoremenu ".a:menupath.'.'.item.'      '.replacement
	endfor
	return
endfunction    " ----------  end of function C_CIosFlagMenus  ----------
"
"------------------------------------------------------------------------------
"  C_Input: Input after a highlighted prompt     {{{1
"           3. argument : optional completion
"------------------------------------------------------------------------------
function! C_Input ( promp, text, ... )
	echohl Search																					" highlight prompt
	call inputsave()																			" preserve typeahead
	if a:0 == 0 || empty(a:1)
		let retval	=input( a:promp, a:text )
	else
		let retval	=input( a:promp, a:text, a:1 )
	endif
	call inputrestore()																		" restore typeahead
	echohl None																						" reset highlighting
	let retval  = substitute( retval, '^\s\+', "", "" )		" remove leading whitespaces
	let retval  = substitute( retval, '\s\+$', "", "" )		" remove trailing whitespaces
	return retval
endfunction    " ----------  end of function C_Input ----------
"
"------------------------------------------------------------------------------
"  C_AdjustLineEndComm: adjust line-end comments     {{{1
"------------------------------------------------------------------------------
"
" C comment or C++ comment:
let	s:c_cppcomment= '\(\/\*.\{-}\*\/\|\/\/.*$\)'

function! C_AdjustLineEndComm ( ) range
	"
	if !exists("b:C_LineEndCommentColumn")
		let	b:C_LineEndCommentColumn	= s:C_LineEndCommColDefault
	endif

	let save_cursor = getpos(".")

	let	save_expandtab	= &expandtab
	exe	":set expandtab"

	let	linenumber	= a:firstline
	exe ":".a:firstline

	while linenumber <= a:lastline
		let	line= getline(".")

		" line is not a pure comment but contains one
		"
		if  match( line, '^\s*'.s:c_cppcomment ) < 0 &&  match( line, s:c_cppcomment ) > 0
      "
      " disregard comments starting in a string
      "
			let	idx1	      = -1
			let	idx2	      = -1
			let	commentstart= -2
			let	commentend	= 0
			while commentstart < idx2 && idx2 < commentend
				let start	      = commentend
				let idx2	      = match( line, s:c_cppcomment, start )
				let commentstart= match   ( line, '"[^"]\+"', start )
				let commentend	= matchend( line, '"[^"]\+"', start )
			endwhile
      "
      " try to adjust the comment
      "
			let idx1	= 1 + match( line, '\s*'.s:c_cppcomment, start )
			let idx2	= 1 + idx2
			call setpos(".", [ 0, linenumber, idx1, 0 ] )
			let vpos1	= virtcol(".")
			call setpos(".", [ 0, linenumber, idx2, 0 ] )
			let vpos2	= virtcol(".")

			if   ! (   vpos2 == b:C_LineEndCommentColumn
						\	|| vpos1 > b:C_LineEndCommentColumn
						\	|| idx2  == 0 )

				exe ":.,.retab"
				" insert some spaces
				if vpos2 < b:C_LineEndCommentColumn
					let	diff	= b:C_LineEndCommentColumn-vpos2
					call setpos(".", [ 0, linenumber, vpos2, 0 ] )
					let	@"	= ' '
					exe "normal	".diff."P"
				endif

				" remove some spaces
				if vpos1 < b:C_LineEndCommentColumn && vpos2 > b:C_LineEndCommentColumn
					let	diff	= vpos2 - b:C_LineEndCommentColumn
					call setpos(".", [ 0, linenumber, b:C_LineEndCommentColumn, 0 ] )
					exe "normal	".diff."x"
				endif

			endif
		endif
		let linenumber=linenumber+1
		normal j
	endwhile
	"
	" restore tab expansion settings and cursor position
	let &expandtab	= save_expandtab
	call setpos('.', save_cursor)

endfunction		" ---------- end of function  C_AdjustLineEndComm  ----------
"
"------------------------------------------------------------------------------
"  C_GetLineEndCommCol: get line-end comment position    {{{1
"------------------------------------------------------------------------------
function! C_GetLineEndCommCol ()
	let actcol	= virtcol(".")
	if actcol+1 == virtcol("$")
		let	b:C_LineEndCommentColumn	= ''
		while match( b:C_LineEndCommentColumn, '^\s*\d\+\s*$' ) < 0
			let b:C_LineEndCommentColumn = C_Input( 'start line-end comment at virtual column : ', actcol, '' )
		endwhile
	else
		let	b:C_LineEndCommentColumn	= virtcol(".")
	endif
  echomsg "line end comments will start at column  ".b:C_LineEndCommentColumn
endfunction		" ---------- end of function  C_GetLineEndCommCol  ----------
"
"------------------------------------------------------------------------------
"  C_EndOfLineComment: single line-end comment    {{{1
"------------------------------------------------------------------------------
function! C_EndOfLineComment ( ) range
	if !exists("b:C_LineEndCommentColumn")
		let	b:C_LineEndCommentColumn	= s:C_LineEndCommColDefault
	endif
	" ----- trim whitespaces -----
	exe a:firstline.','.a:lastline.'s/\s*$//'

	for line in range( a:lastline, a:firstline, -1 )
		let linelength	= virtcol( [line, "$"] ) - 1
		let	diff				= 1
		if linelength < b:C_LineEndCommentColumn
			let diff	= b:C_LineEndCommentColumn -1 -linelength
		endif
		exe "normal	".diff."A "
		call C_InsertTemplate('comment.end-of-line-comment')
		if line > a:firstline
			normal k
		endif
	endfor
endfunction		" ---------- end of function  C_EndOfLineComment  ----------
"
"------------------------------------------------------------------------------
"  C_Comment_C_SectionAll: Section Comments    {{{1
"------------------------------------------------------------------------------
function! C_Comment_C_SectionAll ( type )

	call C_InsertTemplate("comment.file-section-cpp-header-includes")
	call C_InsertTemplate("comment.file-section-cpp-macros")
	call C_InsertTemplate("comment.file-section-cpp-typedefs")
	call C_InsertTemplate("comment.file-section-cpp-data-types")
	if a:type=="cpp"
		call C_InsertTemplate("comment.file-section-cpp-class-defs")
	endif
	call C_InsertTemplate("comment.file-section-cpp-local-variables")
	call C_InsertTemplate("comment.file-section-cpp-prototypes")
	call C_InsertTemplate("comment.file-section-cpp-function-defs-exported")
	call C_InsertTemplate("comment.file-section-cpp-function-defs-local")
	if a:type=="cpp"
		call C_InsertTemplate("comment.file-section-cpp-class-implementations-exported")
		call C_InsertTemplate("comment.file-section-cpp-class-implementations-local")
	endif

endfunction    " ----------  end of function C_Comment_C_SectionAll ----------
"
function! C_Comment_H_SectionAll ( type )

	call C_InsertTemplate("comment.file-section-hpp-header-includes")
	call C_InsertTemplate("comment.file-section-hpp-macros")
	call C_InsertTemplate("comment.file-section-hpp-exported-typedefs")
	call C_InsertTemplate("comment.file-section-hpp-exported-data-types")
	if a:type=="cpp"
		call C_InsertTemplate("comment.file-section-hpp-exported-class-defs")
	endif
	call C_InsertTemplate("comment.file-section-hpp-exported-variables")
	call C_InsertTemplate("comment.file-section-hpp-exported-function-declarations")

endfunction    " ----------  end of function C_Comment_H_SectionAll ----------
"
"----------------------------------------------------------------------
"  C_CodeToCommentC : Code -> Comment   {{{1
"----------------------------------------------------------------------
function! C_CodeToCommentC ( ) range
	silent exe ':'.a:firstline.','.a:lastline."s/^/ \* /"
	silent exe ":".a:firstline."s'^ '\/'"
	silent exe ":".a:lastline
	silent put = ' */'
endfunction    " ----------  end of function  C_CodeToCommentC  ----------
"
"----------------------------------------------------------------------
"  C_CodeToCommentCpp : Code -> Comment   {{{1
"----------------------------------------------------------------------
function! C_CodeToCommentCpp ( ) range
	silent exe a:firstline.','.a:lastline.":s#^#//#"
endfunction    " ----------  end of function  C_CodeToCommentCpp  ----------
"
"----------------------------------------------------------------------
"  C_StartMultilineComment : Comment -> Code   {{{1
"----------------------------------------------------------------------
let s:C_StartMultilineComment	= '^\s*\/\*[\*! ]\='

function! C_RemoveCComment( start, end )

	if a:end-a:start<1
		return 0										" lines removed
	endif
	"
	" Is the C-comment complete ? Get length.
	"
	let check				= getline(	a:start ) =~ s:C_StartMultilineComment
	let	linenumber	= a:start+1
	while linenumber < a:end && getline(	linenumber ) !~ '^\s*\*\/'
		let check				= check && getline(	linenumber ) =~ '^\s*\*[ ]\='
		let linenumber	= linenumber+1
	endwhile
	let check = check && getline(	linenumber ) =~ '^\s*\*\/'
	"
	" remove a complete comment
	"
	if check
		exe "silent :".a:start.'   s/'.s:C_StartMultilineComment.'//'
		let	linenumber1	= a:start+1
		while linenumber1 < linenumber
			exe "silent :".linenumber1.' s/^\s*\*[ ]\=//'
			let linenumber1	= linenumber1+1
		endwhile
		exe "silent :".linenumber1.'   s/^\s*\*\///'
	endif

	return linenumber-a:start+1			" lines removed
endfunction    " ----------  end of function  C_RemoveCComment  ----------
"
"----------------------------------------------------------------------
"  C_CommentToCode : Comment -> Code       {{{1
"----------------------------------------------------------------------
function! C_CommentToCode( ) range

	let	removed	= 0
	"
	let	linenumber	= a:firstline
	while linenumber <= a:lastline
		" Do we have a C++ comment ?
		if getline(	linenumber ) =~ '^\s*//'
			exe "silent :".linenumber.' s#^\s*//##'
			let	removed    = 1
		endif
		" Do we have a C   comment ?
		if removed == 0 && getline(	linenumber ) =~ s:C_StartMultilineComment
			let removed = C_RemoveCComment( linenumber, a:lastline )
		endif

		if removed!=0
			let linenumber = linenumber+removed
			let	removed    = 0
		else
			let linenumber = linenumber+1
		endif
	endwhile
endfunction    " ----------  end of function  C_CommentToCode  ----------
"
"----------------------------------------------------------------------
"  C_CommentCToCpp : C Comment -> C++ Comment       {{{1
"  Changes the first comment in case of multiple C comments:
"    xxxx;               /* 1 */ /* 2 */
"    xxxx;               // 1 // 2
"----------------------------------------------------------------------
function! C_CommentToggle () range
	let	LineEndCommentC		= '\/\*\(.*\)\*\/'
	let	LineEndCommentCpp	= '\/\/\(.*\)$'
	"
	for linenumber in range( a:firstline, a:lastline )
		let line			= getline(linenumber)
		" ----------  C => C++  ----------
		if match( line, LineEndCommentC ) >= 0
			let	line	= substitute( line, '\/\*\s*\(.\{-}\)\*\/', '\/\/ \1', '' )
			call setline( linenumber, line )
			continue
		endif
		" ----------  C++ => C  ----------
		if match( line, LineEndCommentCpp ) >= 0
			let	line	= substitute( line, '\/\/\s*\(.*\)\s*$', '/* \1 */', '' )
			call setline( linenumber, line )
		endif
	endfor
endfunction    " ----------  end of function C_CommentToggle  ----------
"
"=====================================================================================
"----- Menu : Statements -----------------------------------------------------------
"=====================================================================================
"
"------------------------------------------------------------------------------
"  C_PPIf0 : #if 0 .. #endif        {{{1
"------------------------------------------------------------------------------
function! C_PPIf0 (mode)
	"
	let	s:C_If0_Counter	= 0
	let	save_line					= line(".")
	let	actual_line				= 0
	"
	" search for the maximum option number (if any)
	"
	normal gg
	while actual_line < search( s:C_If0_Txt."\\d\\+" )
		let actual_line	= line(".")
	 	let actual_opt  = matchstr( getline(actual_line), s:C_If0_Txt."\\d\\+" )
		let actual_opt  = strpart( actual_opt, strlen(s:C_If0_Txt),strlen(actual_opt)-strlen(s:C_If0_Txt))
		if s:C_If0_Counter < actual_opt
			let	s:C_If0_Counter = actual_opt
		endif
	endwhile
	let	s:C_If0_Counter = s:C_If0_Counter+1
	silent exe ":".save_line
	"
	if a:mode=='a'
		let zz=    "\n#if  0     ".s:C_Com1." ----- #if 0 : ".s:C_If0_Txt.s:C_If0_Counter." ----- ".s:C_Com2."\n"
		let zz= zz."\n#endif     ".s:C_Com1." ----- #if 0 : ".s:C_If0_Txt.s:C_If0_Counter." ----- ".s:C_Com2."\n\n"
		put =zz
		normal 4k
	endif

	if a:mode=='v'
		let	pos1	= line("'<")
		let	pos2	= line("'>")
		let zz=      "#endif     ".s:C_Com1." ----- #if 0 : ".s:C_If0_Txt.s:C_If0_Counter." ----- ".s:C_Com2."\n\n"
		exe ":".pos2."put =zz"
		let zz=    "\n#if  0     ".s:C_Com1." ----- #if 0 : ".s:C_If0_Txt.s:C_If0_Counter." ----- ".s:C_Com2."\n"
		exe ":".pos1."put! =zz"
		"
		if  &foldenable && foldclosed(".")
			normal zv
		endif
	endif

endfunction    " ----------  end of function C_PPIf0 ----------
"
"------------------------------------------------------------------------------
"  C_PPIf0Remove : remove  #if 0 .. #endif        {{{1
"------------------------------------------------------------------------------
function! C_PPIf0Remove ()
	"
	" cursor on fold: open fold first
	if  &foldenable && foldclosed(".")
		normal zv
	endif
	"
	let frstline	= searchpair( '^\s*#if\s\+0', '', '^\s*#endif\>.\+\<If0Label_', 'bn' )
  if frstline<=0
		echohl WarningMsg | echo 'no  #if 0 ... #endif  found or cursor not inside such a directive'| echohl None
    return
  endif
	let lastline	= searchpair( '^\s*#if\s\+0', '', '^\s*#endif\>.\+\<If0Label_', 'n' )
	if lastline<=0
		echohl WarningMsg | echo 'no  #if 0 ... #endif  found or cursor not inside such a directive'| echohl None
		return
	endif
  let actualnumber1  = matchstr( getline(frstline), s:C_If0_Txt."\\d\\+" )
  let actualnumber2  = matchstr( getline(lastline), s:C_If0_Txt."\\d\\+" )
	if actualnumber1 != actualnumber2
    echohl WarningMsg | echo 'lines '.frstline.', '.lastline.': comment tags do not match'| echohl None
		return
	endif

  silent exe ':'.lastline.','.lastline.'d'
	silent exe ':'.frstline.','.frstline.'d'

endfunction    " ----------  end of function C_PPIf0Remove ----------
"
"-------------------------------------------------------------------------------
"   C_LegalizeName : replace non-word characters by underscores
"   - multiple whitespaces
"   - multiple non-word characters
"   - multiple underscores
"-------------------------------------------------------------------------------
function! C_LegalizeName ( name )
	let identifier = substitute(     a:name, '\s\+',  '_', 'g' )
	let identifier = substitute( identifier, '\W\+',  '_', 'g' )
	let identifier = substitute( identifier, '_\+', '_', 'g' )
	return identifier
endfunction    " ----------  end of function C_LegalizeName  ----------

"------------------------------------------------------------------------------
"  C_CodeSnippet : read / edit code snippet       {{{1
"------------------------------------------------------------------------------
function! C_CodeSnippet(mode)

	if isdirectory(s:C_CodeSnippets)
		"
		" read snippet file, put content below current line and indent
		"
		if a:mode == "r"
			if has("browse") && s:C_GuiSnippetBrowser == 'gui'
				let	l:snippetfile=browse(0,"read a code snippet",s:C_CodeSnippets,"")
			else
				let	l:snippetfile=input("read snippet ", s:C_CodeSnippets, "file" )
			endif
			if filereadable(l:snippetfile)
				let	linesread= line("$")
				let l:old_cpoptions	= &cpoptions " Prevent the alternate buffer from being set to this files
				setlocal cpoptions-=a
				:execute "read ".l:snippetfile
				let &cpoptions	= l:old_cpoptions		" restore previous options
				let	linesread= line("$")-linesread-1
				if linesread>=0 && match( l:snippetfile, '\.\(ni\|noindent\)$' ) < 0
				endif
			endif
			if line(".")==2 && getline(1)=~"^$"
				silent exe ":1,1d"
			endif
		endif
		"
		" update current buffer / split window / edit snippet file
		"
		if a:mode == "e"
			if has("browse") && s:C_GuiSnippetBrowser == 'gui'
				let	l:snippetfile	= browse(0,"edit a code snippet",s:C_CodeSnippets,"")
			else
				let	l:snippetfile=input("edit snippet ", s:C_CodeSnippets, "file" )
			endif
			if !empty(l:snippetfile)
				:execute "update! | split | edit ".l:snippetfile
			endif
		endif
		"
		" write whole buffer into snippet file
		"
		if a:mode == "w" || a:mode == "wv"
			if has("browse") && s:C_GuiSnippetBrowser == 'gui'
				let	l:snippetfile	= browse(0,"write a code snippet",s:C_CodeSnippets,"")
			else
				let	l:snippetfile=input("write snippet ", s:C_CodeSnippets, "file" )
			endif
			if !empty(l:snippetfile)
				if filereadable(l:snippetfile)
					if confirm("File ".l:snippetfile." exists ! Overwrite ? ", "&Cancel\n&No\n&Yes") != 3
						return
					endif
				endif
				if a:mode == "w"
					:execute ":write! ".l:snippetfile
				else
					:execute ":*write! ".l:snippetfile
				endif
			endif
		endif

	else
		echo "code snippet directory ".s:C_CodeSnippets." does not exist (please create it)"
	endif
endfunction    " ----------  end of function C_CodeSnippets  ----------
"
"------------------------------------------------------------------------------
"  C_help : builtin completion    {{{1
"------------------------------------------------------------------------------
function!	C_ForTypeComplete ( ArgLead, CmdLine, CursorPos )
	"
	" show all types
	if empty(a:ArgLead)
		return s:C_ForTypes
	endif
	"
	" show types beginning with a:ArgLead
	let	expansions	= []
	for item in s:C_ForTypes
		if match( item, '\<'.a:ArgLead.'\s*\w*' ) == 0
			call add( expansions, item )
		endif
	endfor
	return	expansions
endfunction    " ----------  end of function C_ForTypeComplete  ----------
"
"------------------------------------------------------------------------------
"  C_CodeFor : for (idiom)       {{{1
"------------------------------------------------------------------------------
function! C_CodeFor( direction ) range
	"
	let updown	= ( a:direction == 'up' ? 'INCR.' : 'DECR.' )
	let	string	= C_Input( '[TYPE (expand)] VARIABLE [START [END ['.updown.']]] : ', '',
									\				'customlist,C_ForTypeComplete' )
	if empty(string)
		return
	endif
	"
	let string	= substitute( string, '\s\+', ' ', 'g' )
	let nextindex			= -1
	let loopvar_type	= ''
	for item in s:C_ForTypes_Check_Order
		let nextindex	= matchend( string, '^'.item )
		if nextindex > 0
			let loopvar_type	= item
			let	string				= strpart( string, nextindex )
		endif
	endfor
	if !empty(loopvar_type)
		let loopvar_type	.= ' '
		if empty(string)
			let	string	= C_Input( 'VARIABLE [START [END ['.updown.']]] : ', '' )
			if empty(string)
				return
			endif
		endif
	endif
	let part	= split( string )

	if len( part ) 	> 4
    echohl WarningMsg | echomsg "for loop construction : to many arguments " | echohl None
		return
	endif

	let missing	= 0
	while len(part) < 4
		let part	= part + ['']
		let missing	= missing+1
	endwhile

	let [ loopvar, startval, endval, incval ]	= part

	if empty(incval)
		let incval	= '1'
	endif

	if a:direction == 'up'
		if empty(endval)
			let endval	= 'n'
		endif
		if empty(startval)
			let startval	= '0'
		endif
		let zz= 'for ( '.loopvar_type.loopvar.' = '.startval.'; '.loopvar.' < '.endval.'; '.loopvar.' += '.incval." )"
	else
		if empty(endval)
			let endval	= '0'
		endif
		if empty(startval)
			let startval	= 'n-1'
		endif
		let zz= 'for ( '.loopvar_type.loopvar.' = '.startval.'; '.loopvar.' >= '.endval.'; '.loopvar.' -= '.incval." )"
	endif
	"
	" use internal formatting to avoid conficts when using == below
	let	equalprg_save	= &equalprg
	set equalprg=

	" ----- normal mode ----------------
	if a:firstline == a:lastline
		let zz	= zz." {\n}"
		put =zz
		normal k
		normal 2==
	endif
	" ----- visual mode ----------------
	if a:firstline < a:lastline
		let	zz	= zz.' {'
		let zz2=    '}'
		exe ":".a:lastline."put =zz2"
		exe ":".a:firstline."put! =zz"
		:exe 'normal ='.(a:lastline-a:firstline+2).'+'
	endif
	"
	" restore formatter programm
	let &equalprg	= equalprg_save
	"
	" position the cursor
	"
	normal ^
	if missing == 1
		let match	= search( '\<'.incval.'\>', 'W', line(".") )
	else
		if missing == 2
			let match	= search( '\<'.endval.'\>', 'W', line(".") )
		else
			if missing == 3
				let match	= search( '\<'.startval.'\>', 'W', line(".") )
			endif
		endif
	endif
	"
endfunction    " ----------  end of function C_CodeFor ----------
"
"------------------------------------------------------------------------------
"  Handle prototypes       {{{1
"------------------------------------------------------------------------------
"
let s:C_Prototype        = []
let s:C_PrototypeShow    = []
let s:C_PrototypeCounter = 0
let s:C_CComment         = '\/\*.\{-}\*\/\s*'		" C comment with trailing whitespaces
																								"  '.\{-}'  any character, non-greedy
let s:C_CppComment       = '\/\/.*$'						" C++ comment
"
"------------------------------------------------------------------------------
"  C_ProtoPick: pick up a method prototype (normal/visual)       {{{1
"  type : 'function', 'method'
"------------------------------------------------------------------------------
function! C_ProtoPick( type ) range
	"
	" remove C/C++-comments, leading and trailing whitespaces, squeeze whitespaces
	"
	let prototyp   = ''
	for linenumber in range( a:firstline, a:lastline )
		let newline			= getline(linenumber)
		let newline 	  = substitute( newline, s:C_CppComment, "", "" ) " remove C++ comment
		let prototyp		= prototyp." ".newline
	endfor
	"
	let prototyp  = substitute( prototyp, '^\s\+', "", "" )					" remove leading whitespaces
	let prototyp  = substitute( prototyp, s:C_CComment, "", "g" )		" remove (multiline) C comments
	let prototyp  = substitute( prototyp, '\s\+', " ", "g" )				" squeeze whitespaces
	let prototyp  = substitute( prototyp, '\s\+$', "", "" )					" remove trailing whitespaces
	"
	"-------------------------------------------------------------------------------
	" prototype for  methods
	"-------------------------------------------------------------------------------
	if a:type == 'method'
		"
		" remove template keyword
		"
		let prototyp  = substitute( prototyp, '^template\s*<\s*class \w\+\s*>\s*', "", "" )
		"
		let idx 		= stridx( prototyp, '(' )								    		" start of the parameter list
		let head   	= strpart( prototyp, 0, idx )
		let parlist = strpart( prototyp, idx )
		"
		" remove the scope resolution operator
		"
		let	template_id	= '\h\w*\s*<[^>]\+>'
		let	rgx2				= '\('.template_id.'\s*::\s*\)*\([~A-Za-z]\w*\|operator.\+\)\s*$'
		let idx 				= match( head, rgx2 )								    		" start of the function name
		let returntype	= strpart( head, 0, idx )
		let fctname	  	= strpart( head, idx )

		let resret	= matchstr( returntype, '\('.template_id.'\s*::\s*\)*'.template_id )
		let resret	= substitute( resret, '\s\+', '', 'g' )

		let resfct	= matchstr( fctname   , '\('.template_id.'\s*::\s*\)*'.template_id )
		let resfct	= substitute( resfct, '\s\+', '', 'g' )

		if  !empty(resret) && match( resfct, resret.'$' ) >= 0
			"-------------------------------------------------------------------------------
			" remove scope resolution from the return type (keep 'std::')
			"-------------------------------------------------------------------------------
			let returntype	= substitute( returntype, '<\s*\w\+\s*>', '', 'g' )
			let returntype 	= substitute( returntype, '\<std\s*::', 'std##', 'g' )	" remove the scope res. operator
			let returntype 	= substitute( returntype, '\<\h\w*\s*::', '', 'g' )			" remove the scope res. operator
			let returntype 	= substitute( returntype, '\<std##', 'std::', 'g' )			" remove the scope res. operator
		endif

		let fctname		  = substitute( fctname, '<\s*\w\+\s*>', "", "g" )
		let fctname   	= substitute( fctname, '\<std\s*::', 'std##', 'g' )	" remove the scope res. operator
		let fctname   	= substitute( fctname, '\<\h\w*\s*::', '', 'g' )		" remove the scope res. operator
		let fctname   	= substitute( fctname, '\<std##', 'std::', 'g' )		" remove the scope res. operator

		let	prototyp	= returntype.fctname.parlist
		"
		if empty(fctname) || empty(parlist)
			echon 'No prototype saved. Wrong selection ?'
			return
		endif
	endif
	"
	" remove trailing parts of the function body; add semicolon
	"
	let prototyp	= substitute( prototyp, '\s*{.*$', "", "" )
	let prototyp	= prototyp.";\n"

	"
	" bookkeeping
	"
	let s:C_PrototypeCounter += 1
	let s:C_Prototype        += [prototyp]
	let s:C_PrototypeShow    += ["(".s:C_PrototypeCounter.") ".bufname("%")." #  ".prototyp]
	"
	echon	s:C_PrototypeCounter.' prototype'
	if s:C_PrototypeCounter > 1
		echon	's'
	endif
	"
endfunction    " ---------  end of function C_ProtoPick ----------
"
"------------------------------------------------------------------------------
"  C_ProtoInsert : insert       {{{1
"------------------------------------------------------------------------------
function! C_ProtoInsert ()
	"
	" use internal formatting to avoid conficts when using == below
	let	equalprg_save	= &equalprg
	set equalprg=
	"
	if s:C_PrototypeCounter > 0
		for protytype in s:C_Prototype
			put =protytype
		endfor
		let	lines	= s:C_PrototypeCounter	- 1
		silent exe "normal =".lines."-"
		call C_ProtoClear()
	else
		echo "currently no prototypes available"
	endif
	"
	" restore formatter programm
	let &equalprg	= equalprg_save
	"
endfunction    " ---------  end of function C_ProtoInsert  ----------
"
"------------------------------------------------------------------------------
"  C_ProtoClear : clear       {{{1
"------------------------------------------------------------------------------
function! C_ProtoClear ()
	if s:C_PrototypeCounter > 0
		let s:C_Prototype        = []
		let s:C_PrototypeShow    = []
		if s:C_PrototypeCounter == 1
			echo	s:C_PrototypeCounter.' prototype deleted'
		else
			echo	s:C_PrototypeCounter.' prototypes deleted'
		endif
		let s:C_PrototypeCounter = 0
	else
		echo "currently no prototypes available"
	endif
endfunction    " ---------  end of function C_ProtoClear  ----------
"
"------------------------------------------------------------------------------
"  C_ProtoShow : show       {{{1
"------------------------------------------------------------------------------
function! C_ProtoShow ()
	if s:C_PrototypeCounter > 0
		for protytype in s:C_PrototypeShow
			echo protytype
		endfor
	else
		echo "currently no prototypes available"
	endif
endfunction    " ---------  end of function C_ProtoShow  ----------
"
"------------------------------------------------------------------------------
"  C_EscapeBlanks : C_EscapeBlanks       {{{1
"------------------------------------------------------------------------------
function! C_EscapeBlanks (arg)
	return  substitute( a:arg, " ", "\\ ", "g" )
endfunction    " ---------  end of function C_EscapeBlanks  ----------
"
"------------------------------------------------------------------------------
"  C_Compile : C_Compile       {{{1
"------------------------------------------------------------------------------
"  The standard make program 'make' called by vim is set to the C or C++ compiler
"  and reset after the compilation  (setlocal makeprg=... ).
"  The errorfile created by the compiler will now be read by gvim and
"  the commands cl, cp, cn, ... can be used.
"------------------------------------------------------------------------------
let s:LastShellReturnCode	= 0			" for compile / link / run only

function! C_Compile ()

	let s:C_HlMessage = ""
	exe	":cclose"
	let	Sou		= expand("%:p")											" name of the file in the current buffer
	let	Obj		= expand("%:p:r").s:C_ObjExtension	" name of the object
	let SouEsc= escape( Sou, s:C_FilenameEscChar )
	let ObjEsc= escape( Obj, s:C_FilenameEscChar )
	if s:MSWIN
		let	SouEsc	= '"'.SouEsc.'"'
		let	ObjEsc	= '"'.ObjEsc.'"'
	endif

	" update : write source file if necessary
	exe	":update"

	" compilation if object does not exist or object exists and is older then the source
	if !filereadable(Obj) || (filereadable(Obj) && (getftime(Obj) < getftime(Sou)))
		" &makeprg can be a string containing blanks
		let makeprg_saved	= '"'.&makeprg.'"'
		if expand("%:e") == s:C_CExtension
			exe		"setlocal makeprg=".s:C_CCompiler
		else
			exe		"setlocal makeprg=".s:C_CplusCompiler
		endif
		"
		" COMPILATION
		"
		exe ":compiler ".s:C_VimCompilerName
		let v:statusmsg = ''
		let	s:LastShellReturnCode	= 0
		exe		"make ".s:C_CFlags." ".SouEsc." -o ".ObjEsc
		exe	"setlocal makeprg=".makeprg_saved
		if empty(v:statusmsg)
			let s:C_HlMessage = "'".Obj."' : compilation successful"
		endif
		if v:shell_error != 0
			let	s:LastShellReturnCode	= v:shell_error
		endif
		"
		" open error window if necessary
		:redraw!
		exe	":botright cwindow"
	else
		let s:C_HlMessage = " '".Obj."' is up to date "
	endif

endfunction    " ----------  end of function C_Compile ----------

"===  FUNCTION  ================================================================
"          NAME:  C_CheckForMain
"   DESCRIPTION:  check if current buffer contains a main function
"    PARAMETERS:  
"       RETURNS:  0 : no main function
"===============================================================================
function! C_CheckForMain ()
	return  search( '^\(\s*int\s\+\)\=\s*main', "cnw" )
endfunction    " ----------  end of function C_CheckForMain  ----------
"
"------------------------------------------------------------------------------
"  C_Link : C_Link       {{{1
"------------------------------------------------------------------------------
"  The standard make program which is used by gvim is set to the compiler
"  (for linking) and reset after linking.
"
"  calls: C_Compile
"------------------------------------------------------------------------------
function! C_Link ()

	call	C_Compile()
	:redraw!
	if s:LastShellReturnCode != 0
		let	s:LastShellReturnCode	=  0
		return
	endif

	let s:C_HlMessage = ""
	let	Sou		= expand("%:p")						       		" name of the file (full path)
	let	Obj		= expand("%:p:r").s:C_ObjExtension	" name of the object file
	let	Exe		= expand("%:p:r").s:C_ExeExtension	" name of the executable
	let ObjEsc= escape( Obj, s:C_FilenameEscChar )
	let ExeEsc= escape( Exe, s:C_FilenameEscChar )
	if s:MSWIN
		let	ObjEsc	= '"'.ObjEsc.'"'
		let	ExeEsc	= '"'.ExeEsc.'"'
	endif

	if C_CheckForMain() == 0
		let s:C_HlMessage = "no main function in '".Sou."'"
		return
	endif

	" no linkage if:
	"   executable exists
	"   object exists
	"   source exists
	"   executable newer then object
	"   object newer then source

	if    filereadable(Exe)                &&
      \ filereadable(Obj)                &&
      \ filereadable(Sou)                &&
      \ (getftime(Exe) >= getftime(Obj)) &&
      \ (getftime(Obj) >= getftime(Sou))
		let s:C_HlMessage = " '".Exe."' is up to date "
		return
	endif

	" linkage if:
	"   object exists
	"   source exists
	"   object newer then source

	if filereadable(Obj) && (getftime(Obj) >= getftime(Sou))
		let makeprg_saved='"'.&makeprg.'"'
		if expand("%:e") == s:C_CExtension
			exe		"setlocal makeprg=".s:C_CCompiler
		else
			exe		"setlocal makeprg=".s:C_CplusCompiler
		endif
		exe ":compiler ".s:C_VimCompilerName
		let	s:LastShellReturnCode	= 0
		let v:statusmsg = ''
		silent exe "make ".s:C_LFlags." -o ".ExeEsc." ".ObjEsc." ".s:C_Libs
		if v:shell_error != 0
			let	s:LastShellReturnCode	= v:shell_error
		endif
		exe	"setlocal makeprg=".makeprg_saved
		"
		if empty(v:statusmsg)
			let s:C_HlMessage = "'".Exe."' : linking successful"
		" open error window if necessary
		:redraw!
		exe	":botright cwindow"
		else
			exe ":botright copen"
		endif
	endif
endfunction    " ----------  end of function C_Link ----------
"
"------------------------------------------------------------------------------
"  C_Run : 	C_Run       {{{1
"  calls: C_Link
"------------------------------------------------------------------------------
"
let s:C_OutputBufferName   = "C-Output"
let s:C_OutputBufferNumber = -1
let s:C_RunMsg1						 ="' does not exist or is not executable or object/source older then executable"
let s:C_RunMsg2						 ="' does not exist or is not executable"
"
function! C_Run ()
"
	let s:C_HlMessage = ""
	let Sou  					= expand("%:p")												" name of the source file
	let Obj  					= expand("%:p:r").s:C_ObjExtension		" name of the object file
	let Exe  					= expand("%:p:r").s:C_ExeExtension		" name of the executable
	let ExeEsc  			= escape( Exe, s:C_FilenameEscChar )	" name of the executable, escaped
	let Quote					= ''
	if s:MSWIN
		let Quote					= '"'
	endif
	"
	let l:arguments     = exists("b:C_CmdLineArgs") ? b:C_CmdLineArgs : ''
	"
	let	l:currentbuffer	= bufname("%")
	"
	"==============================================================================
	"  run : run from the vim command line
	"==============================================================================
	if s:C_OutputGvim == "vim"
		"
		if s:C_MakeExecutableToRun !~ "^\s*$"
			call C_HlMessage( "executable : '".s:C_MakeExecutableToRun."'" )
			exe		'!'.Quote.s:C_MakeExecutableToRun.Quote.' '.l:arguments
		else

			silent call C_Link()
			if s:LastShellReturnCode == 0
				" clear the last linking message if any"
				let s:C_HlMessage = ""
				call C_HlMessage()
			endif
			"
			if	executable(Exe) && getftime(Exe) >= getftime(Obj) && getftime(Obj) >= getftime(Sou)
				exe		"!".Quote.ExeEsc.Quote." ".l:arguments
			else
				echomsg "file '".Exe.s:C_RunMsg1
			endif
		endif

	endif
	"
	"==============================================================================
	"  run : redirect output to an output buffer
	"==============================================================================
	if s:C_OutputGvim == "buffer"
		let	l:currentbuffernr	= bufnr("%")
		"
		if s:C_MakeExecutableToRun =~ "^\s*$"
			call C_Link()
		endif
		if l:currentbuffer ==  bufname("%")
			"
			"
			if bufloaded(s:C_OutputBufferName) != 0 && bufwinnr(s:C_OutputBufferNumber)!=-1
				exe bufwinnr(s:C_OutputBufferNumber) . "wincmd w"
				" buffer number may have changed, e.g. after a 'save as'
				if bufnr("%") != s:C_OutputBufferNumber
					let s:C_OutputBufferNumber	= bufnr(s:C_OutputBufferName)
					exe ":bn ".s:C_OutputBufferNumber
				endif
			else
				silent exe ":new ".s:C_OutputBufferName
				let s:C_OutputBufferNumber=bufnr("%")
				setlocal buftype=nofile
				setlocal noswapfile
				setlocal syntax=none
				setlocal bufhidden=delete
				setlocal tabstop=8
			endif
			"
			" run programm
			"
			setlocal	modifiable
			if s:C_MakeExecutableToRun !~ "^\s*$"
				call C_HlMessage( "executable : '".s:C_MakeExecutableToRun."'" )
				exe		'%!'.Quote.s:C_MakeExecutableToRun.Quote.' '.l:arguments
				setlocal	nomodifiable
				"
				if winheight(winnr()) >= line("$")
					exe bufwinnr(l:currentbuffernr) . "wincmd w"
				endif
			else
				"
				if	executable(Exe) && getftime(Exe) >= getftime(Obj) && getftime(Obj) >= getftime(Sou)
					exe		"%!".Quote.ExeEsc.Quote." ".l:arguments
					setlocal	nomodifiable
					"
					if winheight(winnr()) >= line("$")
						exe bufwinnr(l:currentbuffernr) . "wincmd w"
					endif
				else
					setlocal	nomodifiable
					:close
					echomsg "file '".Exe.s:C_RunMsg1
				endif
			endif
			"
		endif
	endif
	"
	"==============================================================================
	"  run : run in a detached xterm  (not available for MS Windows)
	"==============================================================================
	if s:C_OutputGvim == "xterm"
		"
		if s:C_MakeExecutableToRun !~ "^\s*$"
			if s:MSWIN
				exe		'!'.Quote.s:C_MakeExecutableToRun.Quote.' '.l:arguments
			else
				silent exe '!xterm -title '.s:C_MakeExecutableToRun.' '.s:C_XtermDefaults.' -e '.s:C_Wrapper.' '.s:C_MakeExecutableToRun.' '.l:arguments.' &'
				:redraw!
				call C_HlMessage( "executable : '".s:C_MakeExecutableToRun."'" )
			endif
		else

			silent call C_Link()
			"
			if	executable(Exe) && getftime(Exe) >= getftime(Obj) && getftime(Obj) >= getftime(Sou)
				if s:MSWIN
					exe		"!".Quote.ExeEsc.Quote." ".l:arguments
				else
					silent exe '!xterm -title '.ExeEsc.' '.s:C_XtermDefaults.' -e '.s:C_Wrapper.' '.ExeEsc.' '.l:arguments.' &'
					:redraw!
				endif
			else
				echomsg "file '".Exe.s:C_RunMsg1
			endif
		endif
	endif

endfunction    " ----------  end of function C_Run ----------
"
"------------------------------------------------------------------------------
"  C_Arguments : Arguments for the executable       {{{1
"------------------------------------------------------------------------------
function! C_Arguments ()
	let	Exe		  = expand("%:r").s:C_ExeExtension
  if empty(Exe)
		redraw
		echohl WarningMsg | echo "no file name " | echohl None
		return
  endif
	let	prompt	= 'command line arguments for "'.Exe.'" : '
	if exists("b:C_CmdLineArgs")
		let	b:C_CmdLineArgs= C_Input( prompt, b:C_CmdLineArgs, 'file' )
	else
		let	b:C_CmdLineArgs= C_Input( prompt , "", 'file' )
	endif
endfunction    " ----------  end of function C_Arguments ----------
"
"----------------------------------------------------------------------
"  C_Toggle_Gvim_Xterm : change output destination       {{{1
"----------------------------------------------------------------------
function! C_Toggle_Gvim_Xterm ()

	if s:C_OutputGvim == "vim"
		exe "aunmenu  <silent>  ".s:MenuRun.'.&output:\ VIM->buffer->xterm'
		exe "amenu    <silent>  ".s:MenuRun.'.&output:\ BUFFER->xterm->vim<Tab>\\ro              :call C_Toggle_Gvim_Xterm()<CR><CR>'
		exe "imenu    <silent>  ".s:MenuRun.'.&output:\ BUFFER->xterm->vim<Tab>\\ro         <C-C>:call C_Toggle_Gvim_Xterm()<CR><CR>'
		let	s:C_OutputGvim	= "buffer"
	else
		if s:C_OutputGvim == "buffer"
				exe "aunmenu  <silent>  ".s:MenuRun.'.&output:\ BUFFER->xterm->vim'
				if (!s:MSWIN)
					exe "amenu    <silent>  ".s:MenuRun.'.&output:\ XTERM->vim->buffer<Tab>\\ro            :call C_Toggle_Gvim_Xterm()<CR><CR>'
					exe "imenu    <silent>  ".s:MenuRun.'.&output:\ XTERM->vim->buffer<Tab>\\ro       <C-C>:call C_Toggle_Gvim_Xterm()<CR><CR>'
				else
					exe "amenu    <silent>  ".s:MenuRun.'.&output:\ VIM->buffer->xterm<Tab>\\ro            :call C_Toggle_Gvim_Xterm()<CR><CR>'
					exe "imenu    <silent>  ".s:MenuRun.'.&output:\ VIM->buffer->xterm<Tab>\\ro       <C-C>:call C_Toggle_Gvim_Xterm()<CR><CR>'
				endif
			if (!s:MSWIN) && (!empty(s:C_Display))
				let	s:C_OutputGvim	= "xterm"
			else
				let	s:C_OutputGvim	= "vim"
			endif
		else
			" ---------- output : xterm -> gvim
				exe "aunmenu  <silent>  ".s:MenuRun.'.&output:\ XTERM->vim->buffer'
				exe "amenu    <silent>  ".s:MenuRun.'.&output:\ VIM->buffer->xterm<Tab>\\ro            :call C_Toggle_Gvim_Xterm()<CR><CR>'
				exe "imenu    <silent>  ".s:MenuRun.'.&output:\ VIM->buffer->xterm<Tab>\\ro       <C-C>:call C_Toggle_Gvim_Xterm()<CR><CR>'
			let	s:C_OutputGvim	= "vim"
		endif
	endif
	echomsg "output destination is '".s:C_OutputGvim."'"

endfunction    " ----------  end of function C_Toggle_Gvim_Xterm ----------
"
"------------------------------------------------------------------------------
"  C_XtermSize : xterm geometry       {{{1
"------------------------------------------------------------------------------
function! C_XtermSize ()
	let regex	= '-geometry\s\+\d\+x\d\+'
	let geom	= matchstr( s:C_XtermDefaults, regex )
	let geom	= matchstr( geom, '\d\+x\d\+' )
	let geom	= substitute( geom, 'x', ' ', "" )
	let	answer= C_Input("   xterm size (COLUMNS LINES) : ", geom )
	while match(answer, '^\s*\d\+\s\+\d\+\s*$' ) < 0
		let	answer= C_Input(" + xterm size (COLUMNS LINES) : ", geom )
	endwhile
	let answer  = substitute( answer, '\s\+', "x", "" )						" replace inner whitespaces
	let s:C_XtermDefaults	= substitute( s:C_XtermDefaults, regex, "-geometry ".answer , "" )
endfunction    " ----------  end of function C_XtermSize ----------
"
"------------------------------------------------------------------------------
"  run make(1)       {{{1
"------------------------------------------------------------------------------
let s:C_MakeCmdLineArgs   	= ''   " command line arguments for Run-make; initially empty
let s:C_MakeExecutableToRun	= ''
let s:C_Makefile						= ''
"
"------------------------------------------------------------------------------
"  C_ChooseMakefile : choose a makefile       {{{1
"------------------------------------------------------------------------------
function! C_ChooseMakefile ()
	let s:C_Makefile	= ''
	" the path will be escaped:
	let	s:C_Makefile	= C_Input ( "choose a Makefile: ", getcwd(), "file" )
	if  s:MSWIN
		let	s:C_Makefile	= substitute( s:C_Makefile, '\\ ', ' ', 'g' )
	endif
endfunction    " ----------  end of function C_ChooseMakefile  ----------
"
"------------------------------------------------------------------------------
"  C_Make : run make       {{{1
"------------------------------------------------------------------------------
function! C_Make()
	exe	":cclose"
	" update : write source file if necessary
	exe	":update"
	" run make
	if s:C_Makefile == ''
		exe	":make ".s:C_MakeCmdLineArgs
	else
		exe	':lchdir  '.fnamemodify( s:C_Makefile, ":p:h" )
		if  s:MSWIN
			exe	':make -f "'.s:C_Makefile.'" '.s:C_MakeCmdLineArgs
		else
			exe	':make -f '.s:C_Makefile.' '.s:C_MakeCmdLineArgs
		endif
		exe	":lchdir -"
	endif
	exe	":botright cwindow"
	"
endfunction    " ----------  end of function C_Make ----------
"
"------------------------------------------------------------------------------
"  C_MakeClean : run 'make clean'       {{{1
"------------------------------------------------------------------------------
function! C_MakeClean()
	" run make clean
	if s:C_Makefile == ''
		exe	":make clean"
	else
		exe	':lchdir  '.fnamemodify( s:C_Makefile, ":p:h" )
		if  s:MSWIN
			exe	':make -f "'.s:C_Makefile.'" clean'
		else
			exe	':make -f '.s:C_Makefile.' clean'
		endif
		exe	":lchdir -"
	endif
endfunction    " ----------  end of function C_MakeClean ----------

"------------------------------------------------------------------------------
"  C_MakeArguments : get make command line arguments       {{{1
"------------------------------------------------------------------------------
function! C_MakeArguments ()
	let	s:C_MakeCmdLineArgs= C_Input( 'make command line arguments : ', s:C_MakeCmdLineArgs, 'file' )
endfunction    " ----------  end of function C_MakeArguments ----------

"------------------------------------------------------------------------------
"  C_MakeExeToRun : choose executable to run       {{{1
"------------------------------------------------------------------------------
function! C_MakeExeToRun ()
	let	s:C_MakeExecutableToRun = C_Input( 'executable to run [tab compl.]: ', '', 'file' )
	if s:C_MakeExecutableToRun !~ "^\s*$"
		if s:MSWIN
			let s:C_MakeExecutableToRun = substitute(s:C_MakeExecutableToRun, '\\ ', ' ', 'g' )
		endif
		let	s:C_MakeExecutableToRun = escape( getcwd().'/', s:C_FilenameEscChar ).s:C_MakeExecutableToRun
	endif
endfunction    " ----------  end of function C_MakeExeToRun ----------
"
"------------------------------------------------------------------------------
"  C_SplintArguments : splint command line arguments       {{{1
"------------------------------------------------------------------------------
function! C_SplintArguments ()
	if s:C_SplintIsExecutable==0
		let s:C_HlMessage = ' Splint is not executable or not installed! '
	else
		let	prompt	= 'Splint command line arguments for "'.expand("%").'" : '
		if exists("b:C_SplintCmdLineArgs")
			let	b:C_SplintCmdLineArgs= C_Input( prompt, b:C_SplintCmdLineArgs )
		else
			let	b:C_SplintCmdLineArgs= C_Input( prompt , "" )
		endif
	endif
endfunction    " ----------  end of function C_SplintArguments ----------
"
"------------------------------------------------------------------------------
"  C_SplintCheck : run splint(1)        {{{1
"------------------------------------------------------------------------------
function! C_SplintCheck ()
	if s:C_SplintIsExecutable==0
		let s:C_HlMessage = ' Splint is not executable or not installed! '
		return
	endif
	let	l:currentbuffer=bufname("%")
	if &filetype != "c" && &filetype != "cpp"
		let s:C_HlMessage = ' "'.l:currentbuffer.'" seems not to be a C/C++ file '
		return
	endif
	let s:C_HlMessage = ""
	exe	":cclose"
	silent exe	":update"
	let makeprg_saved='"'.&makeprg.'"'
	" Windows seems to need this:
	if	s:MSWIN
		:compiler splint
	endif
	:setlocal makeprg=splint
	"
	let l:arguments  = exists("b:C_SplintCmdLineArgs") ? b:C_SplintCmdLineArgs : ' '
	silent exe	"make ".l:arguments." ".escape(l:currentbuffer,s:C_FilenameEscChar)
	exe	"setlocal makeprg=".makeprg_saved
	exe	":botright cwindow"
	"
	" message in case of success
	"
	if l:currentbuffer == bufname("%")
		let s:C_HlMessage = " Splint --- no warnings for : ".l:currentbuffer
	endif
endfunction    " ----------  end of function C_SplintCheck ----------
"
"------------------------------------------------------------------------------
"  C_CodeCheckArguments : CodeCheck command line arguments       {{{1
"------------------------------------------------------------------------------
function! C_CodeCheckArguments ()
	if s:C_CodeCheckIsExecutable==0
		let s:C_HlMessage = ' CodeCheck is not executable or not installed! '
	else
		let	prompt	= 'CodeCheck command line arguments for "'.expand("%").'" : '
		if exists("b:C_CodeCheckCmdLineArgs")
			let	b:C_CodeCheckCmdLineArgs= C_Input( prompt, b:C_CodeCheckCmdLineArgs )
		else
			let	b:C_CodeCheckCmdLineArgs= C_Input( prompt , s:C_CodeCheckOptions )
		endif
	endif
endfunction    " ----------  end of function C_CodeCheckArguments ----------
"
"------------------------------------------------------------------------------
"  C_CodeCheck : run CodeCheck       {{{1
"------------------------------------------------------------------------------
function! C_CodeCheck ()
	if s:C_CodeCheckIsExecutable==0
		let s:C_HlMessage = ' CodeCheck is not executable or not installed! '
		return
	endif
	let	l:currentbuffer=bufname("%")
	if &filetype != "c" && &filetype != "cpp"
		let s:C_HlMessage = ' "'.l:currentbuffer.'" seems not to be a C/C++ file '
		return
	endif
	let s:C_HlMessage = ""
	exe	":cclose"
	silent exe	":update"
	let makeprg_saved='"'.&makeprg.'"'
	exe	"setlocal makeprg=".s:C_CodeCheckExeName
	"
	" match the splint error messages (quickfix commands)
	" ignore any lines that didn't match one of the patterns
	"
	:setlocal errorformat=%f(%l)%m
	"
	let l:arguments  = exists("b:C_CodeCheckCmdLineArgs") ? b:C_CodeCheckCmdLineArgs : ""
	if empty( l:arguments )
		let l:arguments	=	s:C_CodeCheckOptions
	endif
	exe	":make ".l:arguments." ".escape( l:currentbuffer, s:C_FilenameEscChar )
	exe	':setlocal errorformat='
	exe	":setlocal makeprg=".makeprg_saved
	exe	":botright cwindow"
	"
	" message in case of success
	"
	if l:currentbuffer == bufname("%")
		let s:C_HlMessage = " CodeCheck --- no warnings for : ".l:currentbuffer
	endif
endfunction    " ----------  end of function C_CodeCheck ----------
"
"------------------------------------------------------------------------------
"  C_Indent : run indent(1)       {{{1
"------------------------------------------------------------------------------
"
function! C_Indent ( )
	if !executable("indent")
		echomsg 'indent is not executable or not installed!'
		return
	endif
	let	l:currentbuffer=expand("%:p")
	if &filetype != "c" && &filetype != "cpp"
		echomsg '"'.l:currentbuffer.'" seems not to be a C/C++ file '
		return
	endif
	if C_Input("indent whole file [y/n/Esc] : ", "y" ) != "y"
		return
	endif
	:update

	exe	":cclose"
	if s:MSWIN
		silent exe ":%!indent "
	else
		silent exe ":%!indent 2> ".s:C_IndentErrorLog
		redraw!
		if getfsize( s:C_IndentErrorLog ) > 0
			exe ':edit! '.s:C_IndentErrorLog
			let errorlogbuffer	= bufnr("%")
			exe ':%s/^indent: Standard input/indent: '.escape( l:currentbuffer, '/' ).'/'
			setlocal errorformat=indent:\ %f:%l:%m
			:cbuffer
			exe ':bdelete! '.errorlogbuffer
			exe	':botright cwindow'
		else
			echomsg 'File "'.l:currentbuffer.'" reformatted.'
		endif
		setlocal errorformat=
	endif

endfunction    " ----------  end of function C_Indent ----------
"
"------------------------------------------------------------------------------
"  C_HlMessage : indent message     {{{1
"------------------------------------------------------------------------------
function! C_HlMessage ( ... )
	redraw!
	echohl Search
	if a:0 == 0
		echo s:C_HlMessage
	else
		echo a:1
	endif
	echohl None
endfunction    " ----------  end of function C_HlMessage ----------
"
"------------------------------------------------------------------------------
"  C_Settings : settings     {{{1
"------------------------------------------------------------------------------
function! C_Settings ()
	let	txt =     " C/C++-Support settings\n\n"
	let txt = txt.'                   author :  "'.s:C_Macro['|AUTHOR|']."\"\n"
	let txt = txt.'                authorref :  "'.s:C_Macro['|AUTHORREF|']."\"\n"
	let txt = txt.'                  company :  "'.s:C_Macro['|COMPANY|']."\"\n"
	let txt = txt.'         copyright holder :  "'.s:C_Macro['|COPYRIGHTHOLDER|']."\"\n"
	let txt = txt.'                    email :  "'.s:C_Macro['|EMAIL|']."\"\n"
  let txt = txt.'                  licence :  "'.s:C_Macro['|LICENSE|']."\"\n"
  let txt = txt.'             organization :  "'.s:C_Macro['|ORGANIZATION|']."\"\n"
	let txt = txt.'                  project :  "'.s:C_Macro['|PROJECT|']."\"\n"
	let txt = txt.'         C / C++ compiler :  '.s:C_CCompiler.' / '.s:C_CplusCompiler."\n"
	let txt = txt.'         C file extension :  "'.s:C_CExtension.'"  (everything else is C++)'."\n"
	let txt = txt.'    extension for objects :  "'.s:C_ObjExtension."\"\n"
	let txt = txt.'extension for executables :  "'.s:C_ExeExtension."\"\n"
	let txt = txt.'           compiler flags :  "'.s:C_CFlags."\"\n"
	let txt = txt.'             linker flags :  "'.s:C_LFlags."\"\n"
	let txt = txt.'                libraries :  "'.s:C_Libs."\"\n"
	let txt = txt.'   code snippet directory :  "'.s:C_CodeSnippets."\"\n"
	" ----- template files  ------------------------
	let txt = txt.'           template style :  "'.s:C_ActualStyle."\"\n"
	let txt = txt.'      plugin installation :  "'.s:installation."\"\n"
	if s:installation == 'system'
		let txt = txt.'global template directory :  '.s:C_GlobalTemplateDir."\n"
		if filereadable( s:C_LocalTemplateFile )
			let txt = txt.' local template directory :  '.s:C_LocalTemplateDir."\n"
		endif
	else
		let txt = txt.' local template directory :  '.s:C_LocalTemplateDir."\n"
	endif
	if	!s:MSWIN
		let txt = txt.'           xterm defaults :  '.s:C_XtermDefaults."\n"
	endif
	" ----- dictionaries ------------------------
	if !empty(g:C_Dictionary_File)
		let ausgabe= &dictionary
		let ausgabe= substitute( ausgabe, ",", ",\n                           + ", "g" )
		let txt = txt."       dictionary file(s) :  ".ausgabe."\n"
	endif
	let txt = txt.'     current output dest. :  '.s:C_OutputGvim."\n"
	" ----- splint ------------------------------
	if s:C_SplintIsExecutable==1
		if exists("b:C_SplintCmdLineArgs")
			let ausgabe = b:C_SplintCmdLineArgs
		else
			let ausgabe = ""
		endif
		let txt = txt."        splint options(s) :  ".ausgabe."\n"
	endif
	" ----- code check --------------------------
	if s:C_CodeCheckIsExecutable==1
		if exists("b:C_CodeCheckCmdLineArgs")
			let ausgabe = b:C_CodeCheckCmdLineArgs
		else
			let ausgabe = s:C_CodeCheckOptions
		endif
		let txt = txt."CodeCheck (TM) options(s) :  ".ausgabe."\n"
	endif
	let txt = txt."\n"
	let	txt = txt."__________________________________________________________________________\n"
	let	txt = txt." C/C++-Support, Version ".g:C_Version." / Dr.-Ing. Fritz Mehner / mehner@fh-swf.de\n\n"
	echo txt
endfunction    " ----------  end of function C_Settings ----------
"
"------------------------------------------------------------------------------
"  C_Hardcopy : hardcopy     {{{1
"    MSWIN : a printer dialog is displayed
"    other : print PostScript to file
"------------------------------------------------------------------------------
function! C_Hardcopy () range
  let outfile = expand("%")
  if empty(outfile)
		let s:C_HlMessage = 'Buffer has no name.'
		call C_HlMessage()
  endif
	let outdir	= getcwd()
	if filewritable(outdir) != 2
		let outdir	= $HOME
	endif
	if  !s:MSWIN
		let outdir	= outdir.'/'
	endif
  let old_printheader=&printheader
  exe  ':set printheader='.s:C_Printheader
  " ----- normal mode ----------------
  if a:firstline == a:lastline
    silent exe  'hardcopy > '.outdir.outfile.'.ps'
    if  !s:MSWIN
      echo 'file "'.outfile.'" printed to "'.outdir.outfile.'.ps"'
    endif
  endif
  " ----- visual mode / range ----------------
  if a:firstline < a:lastline
    silent exe  a:firstline.','.a:lastline."hardcopy > ".outdir.outfile.".ps"
    if  !s:MSWIN
      echo 'file "'.outfile.'" (lines '.a:firstline.'-'.a:lastline.') printed to "'.outdir.outfile.'.ps"'
    endif
  endif
  exe  ':set printheader='.escape( old_printheader, ' %' )
endfunction   " ---------- end of function  C_Hardcopy  ----------
"
"------------------------------------------------------------------------------
"  C_HelpCsupport : help csupport     {{{1
"------------------------------------------------------------------------------
function! C_HelpCsupport ()
	try
		:help csupport
	catch
		exe ':helptags '.s:plugin_dir.'/doc'
		:help csupport
	endtry
endfunction    " ----------  end of function C_HelpCsupport ----------
"
"------------------------------------------------------------------------------
"  C_Help : lookup word under the cursor or ask    {{{1
"------------------------------------------------------------------------------
"
let s:C_DocBufferName       = "C_HELP"
let s:C_DocHelpBufferNumber = -1
"
function! C_Help( type )

	let cuc		= getline(".")[col(".") - 1]		" character under the cursor
	let	item	= expand("<cword>")							" word under the cursor
	if empty(cuc) || empty(item) || match( item, cuc ) == -1
		let	item=C_Input('name of the manual page : ', '' )
	endif

	if empty(item)
		return
	endif
	"------------------------------------------------------------------------------
	"  replace buffer content with bash help text
	"------------------------------------------------------------------------------
	"
	" jump to an already open bash help window or create one
	"
	if bufloaded(s:C_DocBufferName) != 0 && bufwinnr(s:C_DocHelpBufferNumber) != -1
		exe bufwinnr(s:C_DocHelpBufferNumber) . "wincmd w"
		" buffer number may have changed, e.g. after a 'save as'
		if bufnr("%") != s:C_DocHelpBufferNumber
			let s:C_DocHelpBufferNumber=bufnr(s:C_OutputBufferName)
			exe ":bn ".s:C_DocHelpBufferNumber
		endif
	else
		exe ":new ".s:C_DocBufferName
		let s:C_DocHelpBufferNumber=bufnr("%")
		setlocal buftype=nofile
		setlocal noswapfile
		setlocal bufhidden=delete
		setlocal filetype=sh		" allows repeated use of <S-F1>
		setlocal syntax=OFF
	endif
	setlocal	modifiable
	"
	if a:type == 'm' 
		"
		" Is there more than one manual ?
		"
		let manpages	= system( s:C_Man.' -k '.item )
		if v:shell_error
			echomsg	"Shell command '".s:C_Man." -k ".item."' failed."
			:close
			return
		endif
		let	catalogs	= split( manpages, '\n', )
		let	manual		= {}
		"
		" Select manuals where the name exactly matches
		"
		for line in catalogs
			if line =~ '^'.item.'\s\+(' 
				let	itempart	= split( line, '\s\+' )
				let	catalog		= itempart[1][1:-2]
				if match( catalog, '.p$' ) == -1
					let	manual[catalog]	= catalog
				endif
			endif
		endfor
		"
		" Build a selection list if there are more than one manual
		"
		let	catalog	= ""
		if len(keys(manual)) > 1
			for key in keys(manual)
				echo ' '.item.'  '.key
			endfor
			let defaultcatalog	= ''
			if has_key( manual, '3' )
				let defaultcatalog	= '3'
			else
				if has_key( manual, '2' )
					let defaultcatalog	= '2'
				endif
			endif
			let	catalog	= input( 'select manual section (<Enter> cancels) : ', defaultcatalog )
			if ! has_key( manual, catalog )
				:close
				:redraw
				echomsg	"no appropriate manual section '".catalog."'"
				return
			endif
		endif

		set filetype=man
		silent exe ":%!".s:C_Man." ".catalog." ".item

		if s:MSWIN
			call s:C_RemoveSpecialCharacters()
		endif
	endif

	setlocal nomodifiable
endfunction		" ---------- end of function  C_Help  ----------
"
"------------------------------------------------------------------------------
"  C_RemoveSpecialCharacters   {{{1
"  remove <backspace><any character> in CYGWIN man(1) output
"  remove           _<any character> in CYGWIN man(1) output
"------------------------------------------------------------------------------
"
function! s:C_RemoveSpecialCharacters ( )
	let	patternunderline	= '_\%x08'
	let	patternbold				= '\%x08.'
	setlocal modifiable
	if search(patternunderline) != 0
		silent exe ':%s/'.patternunderline.'//g'
	endif
	if search(patternbold) != 0
		silent exe ':%s/'.patternbold.'//g'
	endif
	setlocal nomodifiable
	silent normal gg
endfunction		" ---------- end of function  s:C_RemoveSpecialCharacters   ----------

"------------------------------------------------------------------------------
"  C_CreateMenusDelayed     {{{1
"------------------------------------------------------------------------------
let s:C_MenusVisible = 'no'								" state variable controlling the C-menus
"
function! C_CreateMenusDelayed ()
	if s:C_CreateMenusDelayed == 'yes' && s:C_MenusVisible == 'no'
		call C_CreateGuiMenus()
	endif
endfunction    " ----------  end of function C_CreateMenusDelayed  ----------
"
"------------------------------------------------------------------------------
"  C_CreateGuiMenus     {{{1
"------------------------------------------------------------------------------
function! C_CreateGuiMenus ()
	if s:C_MenusVisible == 'no'
		aunmenu <silent> &Tools.Load\ C\ Support
		amenu   <silent> 40.1000 &Tools.-SEP100- :
		amenu   <silent> 40.1030 &Tools.Unload\ C\ Support <C-C>:call C_RemoveGuiMenus()<CR>
		call C_InitMenus()
		let s:C_MenusVisible = 'yes'
	endif
endfunction    " ----------  end of function C_CreateGuiMenus  ----------

"------------------------------------------------------------------------------
"  C_ToolMenu     {{{1
"------------------------------------------------------------------------------
function! C_ToolMenu ()
	amenu   <silent> 40.1000 &Tools.-SEP100- :
	amenu   <silent> 40.1030 &Tools.Load\ C\ Support      :call C_CreateGuiMenus()<CR>
	imenu   <silent> 40.1030 &Tools.Load\ C\ Support <C-C>:call C_CreateGuiMenus()<CR>
endfunction    " ----------  end of function C_ToolMenu  ----------

"------------------------------------------------------------------------------
"  C_RemoveGuiMenus     {{{1
"------------------------------------------------------------------------------
function! C_RemoveGuiMenus ()
	if s:C_MenusVisible == 'yes'
		exe "aunmenu <silent> ".s:C_Root
		"
		aunmenu <silent> &Tools.Unload\ C\ Support
		call C_ToolMenu()
		"
		let s:C_MenusVisible = 'no'
	endif
endfunction    " ----------  end of function C_RemoveGuiMenus  ----------

"------------------------------------------------------------------------------
"  C_RereadTemplates     {{{1
"  rebuild commands and the menu from the (changed) template file
"------------------------------------------------------------------------------
function! C_RereadTemplates ( msg )
	let s:style						= 'default'
	let s:C_Template     	= { 'default' : {} }
	let s:C_FileVisited  	= []
	let	messsage					= ''
	"
	if s:installation == 'system'
		"-------------------------------------------------------------------------------
		" system installation
		"-------------------------------------------------------------------------------
		if filereadable( s:C_GlobalTemplateFile )
			call C_ReadTemplates( s:C_GlobalTemplateFile )
		else
			echomsg "Global template file '".s:C_GlobalTemplateFile."' not readable."
			return
		endif
		let	messsage	= "Templates read from '".s:C_GlobalTemplateFile."'"
		"
		if filereadable( s:C_LocalTemplateFile )
			call C_ReadTemplates( s:C_LocalTemplateFile )
			let messsage	= messsage." and '".s:C_LocalTemplateFile."'"
			if s:C_Macro['|AUTHOR|'] == 'YOUR NAME'
				echomsg "Please set your personal details in file '".s:C_LocalTemplateFile."'."
			endif
		else
			let template	= [ '|AUTHOR|    = YOUR NAME', 
						\						'|COPYRIGHT| = Copyright (c) |YEAR|, |AUTHOR|'
						\		]
			if finddir( s:C_LocalTemplateDir ) == ''
				" try to create a local template directory
				if exists("*mkdir")
					try 
						call mkdir( s:C_LocalTemplateDir, "p" )
						" write a default local template file
						call writefile( template, s:C_LocalTemplateFile )
					catch /.*/
					endtry
				endif
			else
				" write a default local template file
				call writefile( template, s:C_LocalTemplateFile )
			endif
		endif
		"
	else
		"-------------------------------------------------------------------------------
		" local installation
		"-------------------------------------------------------------------------------
		if filereadable( s:C_LocalTemplateFile )
			call C_ReadTemplates( s:C_LocalTemplateFile )
			let	messsage	= "Templates read from '".s:C_LocalTemplateFile."'"
		else
			echomsg "Local template file '".s:C_LocalTemplateFile."' not readable." 
			return
		endif
		"
	endif
	if a:msg == 'yes'
		echomsg messsage.'.'
	endif

endfunction    " ----------  end of function C_RereadTemplates  ----------
"
"------------------------------------------------------------------------------
"  C_BrowseTemplateFiles     {{{1
"------------------------------------------------------------------------------
function! C_BrowseTemplateFiles ( type )
	let	templatefile	= eval( 's:C_'.a:type.'TemplateFile' )
	let	templatedir		= eval( 's:C_'.a:type.'TemplateDir' )
	if isdirectory( templatedir )
		if has("browse") && s:C_GuiTemplateBrowser == 'gui'
			let	l:templatefile	= browse(0,"edit a template file", templatedir, "" )
		else
				let	l:templatefile	= ''
			if s:C_GuiTemplateBrowser == 'explorer'
				exe ':Explore '.templatedir
			endif
			if s:C_GuiTemplateBrowser == 'commandline'
				let	l:templatefile	= input("edit a template file", templatedir, "file" )
			endif
		endif
		if !empty(l:templatefile)
			:execute "update! | split | edit ".l:templatefile
		endif
	else
		echomsg "Template directory '".templatedir."' does not exist."
	endif
endfunction    " ----------  end of function C_BrowseTemplateFiles  ----------

"------------------------------------------------------------------------------
"  C_ReadTemplates     {{{1
"  read the template file(s), build the macro and the template dictionary
"
"------------------------------------------------------------------------------
let	s:style			= 'default'
function! C_ReadTemplates ( templatefile )

  if !filereadable( a:templatefile )
    echohl WarningMsg
    echomsg "C/C++ template file '".a:templatefile."' does not exist or is not readable"
    echohl None
    return
  endif

	let	skipmacros	= 0
  let s:C_FileVisited  += [a:templatefile]

  "------------------------------------------------------------------------------
  "  read template file, start with an empty template dictionary
  "------------------------------------------------------------------------------

  let item  = ''
	let	skipline	= 0
  for line in readfile( a:templatefile )
		" if not a comment :
    if line !~ s:C_MacroCommentRegex
      "
			"-------------------------------------------------------------------------------
			" IF |STYLE| IS ...
			"-------------------------------------------------------------------------------
      "
      let string  = matchlist( line, s:C_TemplateIf )
      if !empty(string) 
				if !has_key( s:C_Template, string[1] )
					" new s:style
					let	s:style	= string[1]
					let	s:C_Template[s:style]	= {}
					continue
				endif
			endif
			"
			"-------------------------------------------------------------------------------
			" ENDIF
			"-------------------------------------------------------------------------------
      "
      let string  = matchlist( line, s:C_TemplateEndif )
      if !empty(string)
				let	s:style	= 'default'
				continue
			endif
      "
      " macros and file includes
      "
      let string  = matchlist( line, s:C_MacroLineRegex )
      if !empty(string) && skipmacros == 0
        let key = '|'.string[1].'|'
        let val = string[2]
        let val = substitute( val, '\s\+$', '', '' )
        let val = substitute( val, "[\"\']$", '', '' )
        let val = substitute( val, "^[\"\']", '', '' )
        "
        if key == '|includefile|' && count( s:C_FileVisited, val ) == 0
					let path   = fnamemodify( a:templatefile, ":p:h" )
          call C_ReadTemplates( path.'/'.val )    " recursive call
        else
          let s:C_Macro[key] = escape( val, '&' )
        endif
        continue                                            " next line
      endif
      "
      " template header
      "
      let name  = matchstr( line, s:C_TemplateLineRegex )
      "
      if !empty(name)
        let part  = split( name, '\s*==\s*')
        let item  = part[0]
        if has_key( s:C_Template[s:style], item ) && s:C_TemplateOverriddenMsg == 'yes'
          echomsg "existing C/C++ template '".item."' overwritten"
        endif
        let s:C_Template[s:style][item] = ''
				let skipmacros	= 1
        "
        let s:C_Attribute[item] = 'below'
        if has_key( s:Attribute, get( part, 1, 'NONE' ) )
          let s:C_Attribute[item] = part[1]
        endif
      else
        if !empty(item)
          let s:C_Template[s:style][item] .= line."\n"
        endif
      endif
    endif
		"
  endfor	" ---------  read line  ---------

	let s:C_ActualStyle	= 'default'
	if !empty( s:C_Macro['|STYLE|'] )
		let s:C_ActualStyle	= s:C_Macro['|STYLE|']
	endif
	let s:C_ActualStyleLast	= s:C_ActualStyle

	call C_SetSmallCommentStyle()
endfunction    " ----------  end of function C_ReadTemplates  ----------

"------------------------------------------------------------------------------
" C_Style{{{1
" ex-command CStyle : callback function
"------------------------------------------------------------------------------
function! C_Style ( style )
	let lstyle  = substitute( a:style, '^\s\+', "", "" )	" remove leading whitespaces
	let lstyle  = substitute( lstyle, '\s\+$', "", "" )		" remove trailing whitespaces
	if has_key( s:C_Template, lstyle )
		if len( s:C_Template[lstyle] ) == 0
			echomsg "style '".lstyle."' : no templates defined"
			return
		endif
		let s:C_ActualStyleLast	= s:C_ActualStyle
		let s:C_ActualStyle	= lstyle
		if len( s:C_ActualStyle ) > 1 && s:C_ActualStyle != s:C_ActualStyleLast
			echomsg "template style is '".lstyle."'"
		endif
	else
		echomsg "style '".lstyle."' does not exist"
	endif
endfunction    " ----------  end of function C_Style  ----------

"------------------------------------------------------------------------------
" C_StyleList     {{{1
" ex-command CStyle
"------------------------------------------------------------------------------
function!	C_StyleList ( ArgLead, CmdLine, CursorPos )
	" show all types / types beginning with a:ArgLead
	return filter( copy(keys( s:C_Template) ), 'v:val =~ "\\<'.a:ArgLead.'\\w*"' )
endfunction    " ----------  end of function C_StyleList  ----------

"------------------------------------------------------------------------------
" C_OpenFold     {{{1
" Open fold and go to the first or last line of this fold. 
"------------------------------------------------------------------------------
function! C_OpenFold ( mode )
	if foldclosed(".") >= 0
		" we are on a closed  fold: get end position, open fold, jump to the
		" last line of the previously closed fold
		let	foldstart	= foldclosed(".")
		let	foldend		= foldclosedend(".")
		normal zv
		if a:mode == 'below'
			exe ":".foldend
		endif
		if a:mode == 'start'
			exe ":".foldstart
		endif
	endif
endfunction    " ----------  end of function C_OpenFold  ----------

"------------------------------------------------------------------------------
"  C_InsertTemplate     {{{1
"  insert a template from the template dictionary
"  do macro expansion
"------------------------------------------------------------------------------
function! C_InsertTemplate ( key, ... )

	if s:C_TemplatesLoaded == 'no'
		call C_RereadTemplates('no')        
		let s:C_TemplatesLoaded	= 'yes'
	endif

	if !has_key( s:C_Template[s:C_ActualStyle], a:key ) &&
	\  !has_key( s:C_Template['default'], a:key )
		echomsg "style '".a:key."' / template '".a:key
	\        ."' not found. Please check your template file in '".s:C_GlobalTemplateDir."'"
		return
	endif

	if &foldenable 
		let	foldmethod_save	= &foldmethod
		set foldmethod=manual
	endif
  "------------------------------------------------------------------------------
  "  insert the user macros
  "------------------------------------------------------------------------------

	" use internal formatting to avoid conficts when using == below
	"
	let	equalprg_save	= &equalprg
	set equalprg=

  let mode  = s:C_Attribute[a:key]

	" remove <SPLIT> and insert the complete macro
	"
	if a:0 == 0
		let val = C_ExpandUserMacros (a:key)
		if empty(val)
			return
		endif
		let val	= C_ExpandSingleMacro( val, '<SPLIT>', '' )

		if mode == 'below'
			call C_OpenFold('below')
			let pos1  = line(".")+1
			put  =val
			let pos2  = line(".")
			" proper indenting
			exe ":".pos1
			let ins	= pos2-pos1+1
			exe "normal ".ins."=="
			"
		elseif mode == 'above'
			let pos1  = line(".")
			put! =val
			let pos2  = line(".")
			" proper indenting
			exe ":".pos1
			let ins	= pos2-pos1+1
			exe "normal ".ins."=="
			"
		elseif mode == 'start'
			normal gg
			call C_OpenFold('start')
			let pos1  = 1
			put! =val
			let pos2  = line(".")
			" proper indenting
			exe ":".pos1
			let ins	= pos2-pos1+1
			exe "normal ".ins."=="
			"
		elseif mode == 'append'
			if &foldenable && foldclosed(".") >= 0
				echohl WarningMsg | echomsg s:MsgInsNotAvail  | echohl None
				exe "set foldmethod=".foldmethod_save
				return
			else
				let pos1  = line(".")
				put =val
				let pos2  = line(".")-1
				exe ":".pos1
				:join!
			endif
			"
		elseif mode == 'insert'
			if &foldenable && foldclosed(".") >= 0
				echohl WarningMsg | echomsg s:MsgInsNotAvail  | echohl None
				exe "set foldmethod=".foldmethod_save
				return
			else
				let val   = substitute( val, '\n$', '', '' )
				let currentline	= getline( "." )
				let pos1  = line(".")
				let pos2  = pos1 + count( split(val,'\zs'), "\n" )
				" assign to the unnamed register "" :
				exe 'normal! a'.val
				" reformat only multiline inserts and previously empty lines
				if pos2-pos1 > 0 || currentline =~ ''
					exe ":".pos1
					let ins	= pos2-pos1+1
					exe "normal ".ins."=="
				endif
			endif
			"
		endif
		"
	else
		"
		" =====  visual mode  ===============================
		"
		if  a:1 == 'v'
			let val = C_ExpandUserMacros (a:key)
			let val	= C_ExpandSingleMacro( val, s:C_TemplateJumpTarget2, '' )
			if empty(val)
				return
			endif

			if match( val, '<SPLIT>\s*\n' ) >= 0
				let part	= split( val, '<SPLIT>\s*\n' )
			else
				let part	= split( val, '<SPLIT>' )
			endif

			if len(part) < 2
				let part	= [ "" ] + part
				echomsg 'SPLIT missing in template '.a:key
			endif
			"
			" 'visual' and mode 'insert':
			"   <part0><marked area><part1>
			" part0 and part1 can consist of several lines
			"
			if mode == 'insert'
				let pos1  = line(".")
				let pos2  = pos1
				let	string= @*
				let replacement	= part[0].string.part[1]
				" remove trailing '\n'
				let replacement   = substitute( replacement, '\n$', '', '' )
				exe ':s/'.string.'/'.replacement.'/'
			endif
			"
			" 'visual' and mode 'below':
			"   <part0>
			"   <marked area>
			"   <part1>
			" part0 and part1 can consist of several lines
			"
			if mode == 'below'

				:'<put! =part[0]
				:'>put  =part[1]

				let pos1  = line("'<") - len(split(part[0], '\n' ))
				let pos2  = line("'>") + len(split(part[1], '\n' ))
				""			echo part[0] part[1] pos1 pos2
				"			" proper indenting
				exe ":".pos1
				let ins	= pos2-pos1+1
				exe "normal ".ins."=="
			endif
			"
		endif		" ---------- end visual mode
	endif

	" restore formatter programm
	let &equalprg	= equalprg_save

  "------------------------------------------------------------------------------
  "  position the cursor
  "------------------------------------------------------------------------------
  exe ":".pos1
  let mtch = search( '<CURSOR>\|{CURSOR}', 'c', pos2 )
	if mtch != 0
		let line	= getline(mtch)
		if line =~ '<CURSOR>$\|{CURSOR}$'
			call setline( mtch, substitute( line, '<CURSOR>\|{CURSOR}', '', '' ) )
			if  a:0 != 0 && a:1 == 'v' && getline(".") =~ '^\s*$'
				normal J
			else
				if getpos(".")[2] < len(getline(".")) || mode == 'insert'
					:startinsert
				else
					:startinsert!
				endif
			endif
		else
			call setline( mtch, substitute( line, '<CURSOR>\|{CURSOR}', '', '' ) )
			:startinsert
		endif
	else
		" to the end of the block; needed for repeated inserts
		if mode == 'below'
			exe ":".pos2
		endif
  endif

  "------------------------------------------------------------------------------
  "  marked words
  "------------------------------------------------------------------------------
	" define a pattern to highlight
	call C_HighlightJumpTargets ()

	if &foldenable 
		" restore folding method
		exe "set foldmethod=".foldmethod_save
		normal zv
	endif

endfunction    " ----------  end of function C_InsertTemplate  ----------

"------------------------------------------------------------------------------
"  C_HighlightJumpTargets
"------------------------------------------------------------------------------
function! C_HighlightJumpTargets ()
	if s:C_Ctrl_j == 'on'
		exe 'match Search /'.s:C_TemplateJumpTarget1.'\|'.s:C_TemplateJumpTarget2.'/'
	endif
endfunction    " ----------  end of function C_HighlightJumpTargets  ----------

"------------------------------------------------------------------------------
"  C_JumpCtrlJ     {{{1
"------------------------------------------------------------------------------
function! C_JumpCtrlJ ()
  let match	= search( s:C_TemplateJumpTarget1.'\|'.s:C_TemplateJumpTarget2, 'c' )
	if match > 0
		" remove the target
		call setline( match, substitute( getline('.'), s:C_TemplateJumpTarget1.'\|'.s:C_TemplateJumpTarget2, '', '' ) )
	else
		" try to jump behind parenthesis or strings in the current line 
		if match( getline(".")[col(".") - 1], "[\]})\"'`]"  ) != 0
			call search( "[\]})\"'`]", '', line(".") )
		endif
		normal l
	endif
	return ''
endfunction    " ----------  end of function C_JumpCtrlJ  ----------

"------------------------------------------------------------------------------
"  C_ExpandUserMacros     {{{1
"------------------------------------------------------------------------------
function! C_ExpandUserMacros ( key )

	if has_key( s:C_Template[s:C_ActualStyle], a:key )
		let template 								= s:C_Template[s:C_ActualStyle][ a:key ]
	else
		let template 								= s:C_Template['default'][ a:key ]
	endif
	let	s:C_ExpansionCounter		= {}										" reset the expansion counter

  "------------------------------------------------------------------------------
  "  renew the predefined macros and expand them
	"  can be replaced, with e.g. |?DATE|
  "------------------------------------------------------------------------------
	let	s:C_Macro['|BASENAME|']	= toupper(expand("%:t:r"))
  let s:C_Macro['|DATE|']  		= C_DateAndTime('d')
  let s:C_Macro['|FILENAME|'] = expand("%:t")
  let s:C_Macro['|PATH|']  		= expand("%:p:h")
  let s:C_Macro['|SUFFIX|'] 	= expand("%:e")
  let s:C_Macro['|TIME|']  		= C_DateAndTime('t')
  let s:C_Macro['|YEAR|']  		= C_DateAndTime('y')

  "------------------------------------------------------------------------------
  "  delete jump targets if mapping for C-j is off
  "------------------------------------------------------------------------------
	if s:C_Ctrl_j == 'off'
		let template	= substitute( template, s:C_TemplateJumpTarget1.'\|'.s:C_TemplateJumpTarget2, '', 'g' )
	endif

  "------------------------------------------------------------------------------
  "  look for replacements
  "------------------------------------------------------------------------------
	while match( template, s:C_ExpansionRegex ) != -1
		let macro				= matchstr( template, s:C_ExpansionRegex )
		let replacement	= substitute( macro, '?', '', '' )
		let template		= substitute( template, macro, replacement, "g" )

		let match	= matchlist( macro, s:C_ExpansionRegex )

		if !empty( match[1] )
			let macroname	= '|'.match[1].'|'
			"
			" notify flag action, if any
			let flagaction	= ''
			if has_key( s:C_MacroFlag, match[2] )
				let flagaction	= ' (-> '.s:C_MacroFlag[ match[2] ].')'
			endif
			"
			" ask for a replacement
			if has_key( s:C_Macro, macroname )
				let	name	= C_Input( match[1].flagaction.' : ', C_ApplyFlag( s:C_Macro[macroname], match[2] ) )
			else
				let	name	= C_Input( match[1].flagaction.' : ', '' )
			endif
			if empty(name)
				return ""
			endif
			"
			" keep the modified name
			let s:C_Macro[macroname]  			= C_ApplyFlag( name, match[2] )
		endif
	endwhile

  "------------------------------------------------------------------------------
  "  do the actual macro expansion
	"  loop over the macros found in the template
  "------------------------------------------------------------------------------
	while match( template, s:C_NonExpansionRegex ) != -1

		let macro			= matchstr( template, s:C_NonExpansionRegex )
		let match			= matchlist( macro, s:C_NonExpansionRegex )

		if !empty( match[1] )
			let macroname	= '|'.match[1].'|'

			if has_key( s:C_Macro, macroname )
				"-------------------------------------------------------------------------------
				"   check for recursion
				"-------------------------------------------------------------------------------
				if has_key( s:C_ExpansionCounter, macroname )
					let	s:C_ExpansionCounter[macroname]	+= 1
				else
					let	s:C_ExpansionCounter[macroname]	= 0
				endif
				if s:C_ExpansionCounter[macroname]	>= s:C_ExpansionLimit
					echomsg " recursion terminated for recursive macro ".macroname
					return template
				endif
				"-------------------------------------------------------------------------------
				"   replace
				"-------------------------------------------------------------------------------
				let replacement = C_ApplyFlag( s:C_Macro[macroname], match[2] )
				let replacement = escape( replacement, '&' )
				let template 		= substitute( template, macro, replacement, "g" )
			else
				"
				" macro not yet defined
				let s:C_Macro['|'.match[1].'|']  		= ''
			endif
		endif

	endwhile

  return template
endfunction    " ----------  end of function C_ExpandUserMacros  ----------

"------------------------------------------------------------------------------
"  C_ApplyFlag     {{{1
"------------------------------------------------------------------------------
function! C_ApplyFlag ( val, flag )
	"
	" l : lowercase
	if a:flag == ':l'
		return  tolower(a:val)
	endif
	"
	" u : uppercase
	if a:flag == ':u'
		return  toupper(a:val)
	endif
	"
	" c : capitalize
	if a:flag == ':c'
		return  toupper(a:val[0]).a:val[1:]
	endif
	"
	" L : legalized name
	if a:flag == ':L'
		return  C_LegalizeName(a:val)
	endif
	"
	" flag not valid
	return a:val
endfunction    " ----------  end of function C_ApplyFlag  ----------
"
"------------------------------------------------------------------------------
"  C_ExpandSingleMacro     {{{1
"------------------------------------------------------------------------------
function! C_ExpandSingleMacro ( val, macroname, replacement )
  return substitute( a:val, escape(a:macroname, '$' ), a:replacement, "g" )
endfunction    " ----------  end of function C_ExpandSingleMacro  ----------

"------------------------------------------------------------------------------
"  C_SetSmallCommentStyle     {{{1
"------------------------------------------------------------------------------
function! C_SetSmallCommentStyle ()
	if has_key( s:C_Template, 'comment.end-of-line-comment' )
		if match( s:C_Template['comment.end-of-line-comment'], '^\s*/\*' ) != -1
			let s:C_Com1          = '/*'     " C-style : comment start
			let s:C_Com2          = '*/'     " C-style : comment end
		else
			let s:C_Com1          = '//'     " C++style : comment start
			let s:C_Com2          = ''       " C++style : comment end
		endif
	endif
endfunction    " ----------  end of function C_SetSmallCommentStyle  ----------

"------------------------------------------------------------------------------
"  C_InsertMacroValue     {{{1
"------------------------------------------------------------------------------
function! C_InsertMacroValue ( key )
	if empty( s:C_Macro['|'.a:key.'|'] )
		echomsg 'the tag |'.a:key.'| is empty'
		return
	endif
	"
	if &foldenable && foldclosed(".") >= 0
		echohl WarningMsg | echomsg s:MsgInsNotAvail  | echohl None
		return
	endif
	if col(".") > 1
		exe 'normal! a'.s:C_Macro['|'.a:key.'|']
	else
		exe 'normal! i'.s:C_Macro['|'.a:key.'|']
	endif
endfunction    " ----------  end of function C_InsertMacroValue  ----------

"------------------------------------------------------------------------------
"  insert date and time     {{{1
"------------------------------------------------------------------------------
function! C_InsertDateAndTime ( format )
	if &foldenable && foldclosed(".") >= 0
		echohl WarningMsg | echomsg s:MsgInsNotAvail  | echohl None
		return ""
	endif
	if col(".") > 1
		exe 'normal a'.C_DateAndTime(a:format)
	else
		exe 'normal i'.C_DateAndTime(a:format)
	endif
endfunction    " ----------  end of function C_InsertDateAndTime  ----------

"------------------------------------------------------------------------------
"  generate date and time     {{{1
"------------------------------------------------------------------------------
function! C_DateAndTime ( format )
	if a:format == 'd'
		return strftime( s:C_FormatDate )
	elseif a:format == 't'
		return strftime( s:C_FormatTime )
	elseif a:format == 'dt'
		return strftime( s:C_FormatDate ).' '.strftime( s:C_FormatTime )
	elseif a:format == 'y'
		return strftime( s:C_FormatYear )
	endif
endfunction    " ----------  end of function C_DateAndTime  ----------

"------------------------------------------------------------------------------
"  check for header or implementation file     {{{1
"------------------------------------------------------------------------------
function! C_InsertTemplateWrapper ()
	" prevent insertion for a file generated from a link error:
	if isdirectory(expand('%:p:h'))
		if index( s:C_SourceCodeExtensionsList, expand('%:e') ) >= 0 
			call C_InsertTemplate("comment.file-description")
		else
			call C_InsertTemplate("comment.file-description-header")
		endif
		set modified
	endif
endfunction    " ----------  end of function C_InsertTemplateWrapper  ----------

"
"-------------------------------------------------------------------------------
"   Comment : C/C++ File Sections             {{{1
"-------------------------------------------------------------------------------
let s:CFileSection	= { 
	\ "Header\ File\ Includes" : "file-section-cpp-header-includes"               , 
	\ "Local\ Macros"					 : "file-section-cpp-macros"                        , 
	\ "Local\ Type\ Def\."		 : "file-section-cpp-typedefs"                      , 
	\ "Local\ Data\ Types"		 : "file-section-cpp-data-types"                    , 
	\ "Local\ Variables"			 : "file-section-cpp-local-variables"               , 
	\ "Local\ Prototypes"			 : "file-section-cpp-prototypes"                    , 
	\ "Exp\.\ Function\ Def\." : "file-section-cpp-function-defs-exported"        , 
	\ "Local\ Function\ Def\." : "file-section-cpp-function-defs-local"           , 
	\ "Local\ Class\ Def\."		 : "file-section-cpp-class-defs"                    , 
	\ "Exp\.\ Class\ Impl\."	 : "file-section-cpp-class-implementations-exported", 
	\ "Local\ Class\ Impl\."	 : "file-section-cpp-class-implementations-local"   , 
	\ "All\ sections,\ C"			 : "c",
	\ "All\ sections,\ C++"		 : "cpp",
	\ }

function!	C_CFileSectionList ( ArgLead, CmdLine, CursorPos )
	return filter( copy( sort(keys( s:CFileSection)) ), 'v:val =~ "\\<'.a:ArgLead.'\\w*"' )
endfunction    " ----------  end of function C_CFileSectionList  ----------

function! C_CFileSectionListInsert ( arg )
	if has_key( s:CFileSection, a:arg )
		if s:CFileSection[a:arg] == 'c' || s:CFileSection[a:arg] == 'cpp'
			call C_Comment_C_SectionAll( 'comment.'.s:CFileSection[a:arg] )
			return 
		endif
		call C_InsertTemplate( 'comment.'.s:CFileSection[a:arg] )
	else
		echomsg "entry '".a:arg."' does not exist"
	endif
endfunction    " ----------  end of function C_CFileSectionListInsert  ----------
"
"-------------------------------------------------------------------------------
"   Comment : H File Sections             {{{1
"-------------------------------------------------------------------------------
let s:HFileSection	= { 
	\	"Header\ File\ Includes"    : "file-section-hpp-header-includes"               ,
	\	"Exported\ Macros"          : "file-section-hpp-macros"                        ,
	\	"Exported\ Type\ Def\."     : "file-section-hpp-exported-typedefs"             ,
	\	"Exported\ Data\ Types"     : "file-section-hpp-exported-data-types"           ,
	\	"Exported\ Variables"       : "file-section-hpp-exported-variables"            ,
	\	"Exported\ Funct\.\ Decl\." : "file-section-hpp-exported-function-declarations",
	\	"Exported\ Class\ Def\."    : "file-section-hpp-exported-class-defs"           ,
	\	"All\ sections,\ C"         : "c"                                              ,
	\	"All\ sections,\ C++"       : "cpp"                                            ,
	\ }

function!	C_HFileSectionList ( ArgLead, CmdLine, CursorPos )
	return filter( copy( sort(keys( s:HFileSection)) ), 'v:val =~ "\\<'.a:ArgLead.'\\w*"' )
endfunction    " ----------  end of function C_HFileSectionList  ----------

function! C_HFileSectionListInsert ( arg )
	if has_key( s:HFileSection, a:arg )
		if s:HFileSection[a:arg] == 'c' || s:HFileSection[a:arg] == 'cpp'
			call C_Comment_C_SectionAll( 'comment.'.s:HFileSection[a:arg] )
			return 
		endif
		call C_InsertTemplate( 'comment.'.s:HFileSection[a:arg] )
	else
		echomsg "entry '".a:arg."' does not exist"
	endif
endfunction    " ----------  end of function C_HFileSectionListInsert  ----------
"
"-------------------------------------------------------------------------------
"   Comment : Keyword Comments             {{{1
"-------------------------------------------------------------------------------
let s:KeywordComment	= { 
	\	'BUG'          : 'keyword-bug',
	\	'COMPILER'     : 'keyword-compiler',
	\	'TODO'         : 'keyword-todo',
	\	'TRICKY'       : 'keyword-tricky',
	\	'WARNING'      : 'keyword-warning',
	\	'WORKAROUND'   : 'keyword-workaround',
	\	'new\ keyword' : 'keyword-keyword',
	\ }

function!	C_KeywordCommentList ( ArgLead, CmdLine, CursorPos )
	return filter( copy( sort(keys( s:KeywordComment)) ), 'v:val =~ "\\<'.a:ArgLead.'\\w*"' )
endfunction    " ----------  end of function C_KeywordCommentList  ----------

function! C_KeywordCommentListInsert ( arg )
	if has_key( s:KeywordComment, a:arg )
		if s:KeywordComment[a:arg] == 'c' || s:KeywordComment[a:arg] == 'cpp'
			call C_Comment_C_SectionAll( 'comment.'.s:KeywordComment[a:arg] )
			return 
		endif
		call C_InsertTemplate( 'comment.'.s:KeywordComment[a:arg] )
	else
		echomsg "entry '".a:arg."' does not exist"
	endif
endfunction    " ----------  end of function C_KeywordCommentListInsert  ----------
"
"-------------------------------------------------------------------------------
"   Comment : Special Comments             {{{1
"-------------------------------------------------------------------------------
let s:SpecialComment	= { 
	\	'EMPTY'                                    : 'special-empty' ,
	\	'FALL\ THROUGH'                            : 'special-fall-through' ,
	\	'IMPL\.\ TYPE\ CONV'                       : 'special-implicit-type-conversion")' ,
	\	'NO\ RETURN'                               : 'special-no-return' ,
	\	'NOT\ REACHED'                             : 'special-not-reached' ,
	\	'TO\ BE\ IMPL\.'                           : 'special-remains-to-be-implemented' ,
	\	'constant\ type\ is\ long\ (L)'            : 'special-constant-type-is-long' ,
	\	'constant\ type\ is\ unsigned\ (U)'        : 'special-constant-type-is-unsigned' ,
	\	'constant\ type\ is\ unsigned\ long\ (UL)' : 'special-constant-type-is-unsigned-long' ,
	\ }

function!	C_SpecialCommentList ( ArgLead, CmdLine, CursorPos )
	return filter( copy( sort(keys( s:SpecialComment)) ), 'v:val =~ "\\<'.a:ArgLead.'\\w*"' )
endfunction    " ----------  end of function C_SpecialCommentList  ----------

function! C_SpecialCommentListInsert ( arg )
	if has_key( s:SpecialComment, a:arg )
		if s:SpecialComment[a:arg] == 'c' || s:SpecialComment[a:arg] == 'cpp'
			call C_Comment_C_SectionAll( 'comment.'.s:SpecialComment[a:arg] )
			return 
		endif
		call C_InsertTemplate( 'comment.'.s:SpecialComment[a:arg] )
	else
		echomsg "entry '".a:arg."' does not exist"
	endif
endfunction    " ----------  end of function C_SpecialCommentListInsert  ----------

"-------------------------------------------------------------------------------
" Standard Library Includes
"-------------------------------------------------------------------------------
function! C_CleanDirNameList ( list )
	let	result	= copy( a:list )
	let	index		= 0
	while index < len( result )
		let result[index]	= substitute( result[index], '[&\\]', '', 'g' )
		let index 				= index + 1
	endwhile
	return result
endfunction    " ----------  end of function C_CleanDirNameList  ----------

let	s:C_StandardLibsClean			= C_CleanDirNameList( s:C_StandardLibs )
let	s:C_C99LibsClean					= C_CleanDirNameList( s:C_C99Libs )
let	s:Cpp_StandardLibsClean		= C_CleanDirNameList( s:Cpp_StandardLibs )
let	s:Cpp_CStandardLibsClean	= C_CleanDirNameList( s:Cpp_CStandardLibs )

"-------------------------------------------------------------------------------
" callback functions used in the filetype plugin ftplugin/c.vim
" callback functions
"-------------------------------------------------------------------------------

function! C_IncludesInsert ( arg, List )
	if index( a:List, a:arg ) >= 0
		exe 'normal a#include <'.a:arg.'>'
	else
		echomsg "entry '".a:arg."' does not exist"
	endif
endfunction    " ----------  end of function C_IncludesInsert
"
function! C_StdLibraryIncludesInsert ( arg )
	call C_IncludesInsert ( a:arg, s:C_StandardLibsClean )
endfunction    " ----------  end of function C_StdLibraryIncludesInsert

function! C_C99LibraryIncludesInsert ( arg )
	call C_IncludesInsert ( a:arg, s:C_C99LibsClean )
endfunction    " ----------  end of function C_C99LibraryIncludesInsert

function! C_CppLibraryIncludesInsert ( arg )
	call C_IncludesInsert ( a:arg, s:Cpp_StandardLibsClean )
endfunction    " ----------  end of function C_CppLibraryIncludesInsert

function! C_CppCLibraryIncludesInsert ( arg )
	call C_IncludesInsert ( a:arg, s:Cpp_CStandardLibsClean )
endfunction    " ----------  end of function C_CppCLibraryIncludesInsert

"-------------------------------------------------------------------------------
" callback functions used in the filetype plugin ftplugin/c.vim
" custom completion
"-------------------------------------------------------------------------------

function!	C_IncludesList ( ArgLead, CmdLine, CursorPos, List )
	" show all libs
	if empty(a:ArgLead)
		return a:List
	endif
	" show libs beginning with a:ArgLead
	let	expansions	= []
	for item in a:List
		if match( item, '\<'.a:ArgLead.'\w*' ) == 0
			call add( expansions, item )
		endif
	endfor
	return	expansions
endfunction    " ----------  end of function C_IncludesList  ----------
"
function!	C_StdLibraryIncludesList ( ArgLead, CmdLine, CursorPos )
	return C_IncludesList ( a:ArgLead, a:CmdLine, a:CursorPos, s:C_StandardLibsClean )
endfunction    " ----------  end of function C_StdLibraryIncludesList  ----------

function!	C_C99LibraryIncludesList ( ArgLead, CmdLine, CursorPos )
	return C_IncludesList ( a:ArgLead, a:CmdLine, a:CursorPos, s:C_C99LibsClean )
endfunction    " ----------  end of function C_C99LibraryIncludesList  ----------

function!	C_CppLibraryIncludesList ( ArgLead, CmdLine, CursorPos )
	return C_IncludesList ( a:ArgLead, a:CmdLine, a:CursorPos, s:Cpp_StandardLibsClean )
endfunction    " ----------  end of function C_CppLibraryIncludesList  ----------

function!	C_CppCLibraryIncludesList ( ArgLead, CmdLine, CursorPos )
	return C_IncludesList ( a:ArgLead, a:CmdLine, a:CursorPos, s:Cpp_CStandardLibsClean )
endfunction    " ----------  end of function C_CppCLibraryIncludesList  ----------

"------------------------------------------------------------------------------
"  show / hide the c-support menus
"  define key mappings (gVim only)
"------------------------------------------------------------------------------
"
call C_ToolMenu()
"
if s:C_LoadMenus == 'yes' && s:C_CreateMenusDelayed == 'no'
	call C_CreateGuiMenus()
endif
"
nmap  <unique>  <silent>  <Leader>lcs   :call C_CreateGuiMenus()<CR>
nmap  <unique>  <silent>  <Leader>ucs   :call C_RemoveGuiMenus()<CR>
"
"------------------------------------------------------------------------------
"  Automated header insertion
"  Local settings for the quickfix window
"
"			Vim always adds the {cmd} after existing autocommands,
"			so that the autocommands execute in the order in which
"			they were given. The order matters!
"------------------------------------------------------------------------------

if has("autocmd")
	"
	"  *.h has filetype 'cpp' by default; this can be changed to 'c' :
	"
	if s:C_TypeOfH=='c'
		autocmd BufNewFile,BufEnter  *.h  :set filetype=c
	endif
	"
	" C/C++ source code files which should not be preprocessed.
	"
	autocmd BufNewFile,BufRead  *.i  :set filetype=c
	autocmd BufNewFile,BufRead  *.ii :set filetype=cpp
	"
	"
	" DELAYED LOADING OF THE TEMPLATE DEFINITIONS
	"
	autocmd BufNewFile,BufRead  *                   
				\	if (&filetype=='cpp' || &filetype=='c') |
				\	  call C_CreateMenusDelayed()           |
				\ endif
	"
	"  Automated header insertion (suffixes from the gcc manual)
	"
	if !exists( 'g:C_Styles' )
		"-------------------------------------------------------------------------------
		" template styles are the default settings
		"-------------------------------------------------------------------------------
		autocmd BufNewFile  * if &filetype =~ '^\(c\|cpp\)$' && expand("%:e") !~ 'ii\?' |
					\     call C_InsertTemplateWrapper() | endif
		"
	else
		"-------------------------------------------------------------------------------
		" template styles are related to file extensions 
		"-------------------------------------------------------------------------------
		for [ pattern, stl ] in items( g:C_Styles )
			exe "autocmd BufNewFile,BufRead,BufEnter ".pattern." call C_Style( '".stl."' )"
			exe "autocmd BufNewFile                  ".pattern." call C_InsertTemplateWrapper()"
		endfor
		"
	endif
	"
	" Wrap error descriptions in the quickfix window.
	"
	autocmd BufReadPost quickfix  setlocal wrap | setlocal linebreak
	"
	exe 'autocmd BufRead *.'.join( s:C_SourceCodeExtensionsList, '\|*.' )
				\     .' call C_HighlightJumpTargets()'
	"
endif " has("autocmd")
"
"=====================================================================================
" vim: tabstop=2 shiftwidth=2 foldmethod=marker
