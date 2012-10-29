" txtbrowser.vim:	Utilities to browser plain text file.
" Release:		1.3.5
" Maintainer:		ypguo<guoyoooping@163.com>
" Last modified:	2011.08.07
" License:		GPL

" Line continuation used here
let s:cpo_save = &cpo
set cpo&vim

" ****************** Options *******************************************
"How many title level to support, default is 3.
if !exists('TxtBrowser_Title_Level')
	let TxtBrowser_Title_Level = 3
endif

"When this file reload, only load TBrowser_Ctags_Cmd once.

if !exists('Tlist_Ctags_Cmd')
	echomsg 'TxtBrowser: Taglist(http://www.vim.org/scripts/script.php?script_id=273) ' .
				\ 'not found. Plugin is not loaded.'
	" Skip loading the plugin
	let loaded_taglist = 'no'
	let &cpo = s:cpo_save
	finish
endif

if !exists('TBrowser_Ctags_Cmd')
	let TBrowser_Ctags_Cmd = Tlist_Ctags_Cmd
endif

"Txt tag definition start.
let s:TBrowser_Config = ' --langdef=txt --langmap=txt:.txt '

"Title tag definition
let s:TBrowser_Config .= '--regex-txt="/^([0-9]+\.?[ \t]+)(.+$)/\1\2/c,content/" '
if (TxtBrowser_Title_Level >= 2)
	let s:TBrowser_Config .= '--regex-txt="/^(([0-9]+\.){1}([0-9]+\.?)[ \t]+)(.+$)/.   \1\4/c,content/" '
endif
if (TxtBrowser_Title_Level >= 3)
	let s:TBrowser_Config .= '--regex-txt="/^(([0-9]+\.){2}([0-9]+\.?)[ \t]+)(.+$)/.       \1\4/c,content/" '
endif
if (TxtBrowser_Title_Level >= 4)
	let s:TBrowser_Config .= '--regex-txt="/^(([0-9]+\.){3}([0-9]+\.?)[ \t]+)(.+$)/.           \1\4/c,content/" '
endif

"Table and Figure tag definition
let s:TBrowser_Config .= '--regex-txt="/^[ \t]+(table[ \t]+[0-9a-zA-Z]+([.: ]([ \t]*.+)?)?$)/\1/t,tables/i" '
let s:TBrowser_Config .= '--regex-txt="/^[ \t]+(figure[ \t]+[0-9a-zA-Z]+([.: ]([ \t]*.+)?)?$)/\1/f,figures/i" '

"Special process of Chinese(or CJK) tag.
if (exists('Tlist_Enc_Patch') || has("unix"))
	let s:TBrowser_Config .= '--regex-txt="/^[ \t]*(图[ \t]*[0-9a-zA-Z]+[.: ][ \t]*.+$)/\1/f,figures/i" '
	let s:TBrowser_Config .= '--regex-txt="/^[ \t]*(表[ \t]*[0-9a-zA-Z]+[.: ][ \t]*.+$)/\1/t,tables/i" '

	if ('utf8' != &fenc)
		let s:TBrowser_Config = iconv(s:TBrowser_Config, 'utf8', &fenc)
	endif
endif

"Pass parameters to taglist
let tlist_txt_settings = 'txt;c:content;f:figures;t:tables'
let Tlist_Ctags_Cmd = TBrowser_Ctags_Cmd . s:TBrowser_Config

" restore 'cpo'
let &cpo = s:cpo_save
unlet s:cpo_save

