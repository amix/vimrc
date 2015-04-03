" AlignMapsPlugin:   Alignment maps based upon <Align.vim> and <AlignMaps.vim>
" Maintainer:        Dr. Charles E. Campbell. <NdrOchipS@PcampbellAfamily.Mbiz>
" Date:              Jan 07, 2013
"
" NOTE: the code herein needs vim 7.0 or later
" Copyright:    Copyright (C) 1999-2012 Charles E. Campbell {{{1
"               Permission is hereby granted to use and distribute this code,
"               with or without modifications, provided that this copyright
"               notice is copied with it. Like anything else that's free,
"               AlignMaps.vim is provided *as is* and comes with no warranty
"               of any kind, either expressed or implied. By using this
"               plugin, you agree that in no event will the copyright
"               holder be liable for any damages resulting from the use
"               of this software.
" Romans 1:20 For the invisible things of Him since the creation of the {{{1
" world are clearly seen, being perceived through the things that are
" made, even His everlasting power and divinity; that they may be
" without excuse.

" ---------------------------------------------------------------------
" Usage: {{{1
" Use 'a to mark beginning of to-be-aligned region,   Alternative:  use V
" move cursor to end of region, and execute map.      (linewise visual mode) to
" The maps also set up marks 'y and 'z, and retain    mark region, execute same
" 'a at the beginning of region.                      map.  Uses 'a, 'y, and 'z.
"
" The start/end wrappers save and restore marks 'y and 'z.
"
" Although the comments indicate the maps use a leading backslash,
" actually they use <Leader> (:he mapleader), so the user can
" specify that the maps start how he or she prefers.
"
" Note: these maps all use <Align.vim>.
"
" Load Once: {{{1
if &cp || exists("g:loaded_AlignMapsPlugin")
 finish
endif
let s:keepcpo                = &cpo
let g:loaded_AlignMapsPlugin = "v43"
set cpo&vim

" =====================================================================
"  Public Interface: {{{1
com! AlignMapsClean	:call AlignMaps#AlignMapsClean()

" =====================================================================
"  Maps: {{{1

" ---------------------------------------------------------------------
" WS: wrapper start map (internal)  {{{2
" Produces a blank line above and below, marks with 'y and 'z
if !hasmapto('<Plug>WrapperStart')
 map <unique> <SID>WS	<Plug>AlignMapsWrapperStart
endif
nnoremap <silent> <script> <Plug>AlignMapsWrapperStart	:set lz<CR>:call AlignMaps#WrapperStart(0)<CR>
vnoremap <silent> <script> <Plug>AlignMapsWrapperStart	:<c-u>set lz<CR>:call AlignMaps#WrapperStart(1)<CR>

" ---------------------------------------------------------------------
" WE: wrapper end (internal)   {{{2
" Removes guard lines, restores marks y and z, and restores search pattern
if !hasmapto('<Plug>WrapperEnd')
 nmap <unique> <SID>WE	<Plug>AlignMapsWrapperEnd
endif
nnoremap <silent> <script> <Plug>AlignMapsWrapperEnd	:call AlignMaps#WrapperEnd()<CR>:set nolz<CR>

" ---------------------------------------------------------------------
" Complex C-code alignment maps: {{{2
if !hasmapto('<Plug>AM_a?')   |call AlignMaps#MakeMap("a?")|endif
if !hasmapto('<Plug>AM_a,')   |call AlignMaps#MakeMap("a,")|endif
if !hasmapto('<Plug>AM_a<')   |call AlignMaps#MakeMap("a<")|endif
if !hasmapto('<Plug>AM_a=')   |call AlignMaps#MakeMap("a=")|endif
if !hasmapto('<Plug>AM_a(')   |call AlignMaps#MakeMap("a(")|endif
if !hasmapto('<Plug>AM_abox') |call AlignMaps#MakeMap("abox")|endif
if !hasmapto('<Plug>AM_acom') |call AlignMaps#MakeMap("acom")|endif
if !hasmapto('<Plug>AM_adcom')|call AlignMaps#MakeMap("adcom")|endif
if !hasmapto('<Plug>AM_aocom')|call AlignMaps#MakeMap("aocom")|endif
if !hasmapto('<Plug>AM_ascom')|call AlignMaps#MakeMap("ascom")|endif
if !hasmapto('<Plug>AM_adec') |call AlignMaps#MakeMap("adec")|endif
if !hasmapto('<Plug>AM_adef') |call AlignMaps#MakeMap("adef")|endif
if !hasmapto('<Plug>AM_afnc') |call AlignMaps#MakeMap("afnc")|endif
if !hasmapto('<Plug>AM_afnc') |call AlignMaps#MakeMap("afnc")|endif

" ---------------------------------------------------------------------
" Number alignment maps: {{{2
if !hasmapto('<Plug>AM_aunum')|call AlignMaps#MakeMap("aunum")|endif
if !hasmapto('<Plug>AM_aenum')|call AlignMaps#MakeMap("aenum")|endif
if exists("g:alignmaps_euronumber") && !exists("g:alignmaps_usanumber")
 if !hasmapto('<Plug>AM_anum')|call AlignMaps#MakeMap("anum")|endif
else
 if !hasmapto('<Plug>AM_anum')|call AlignMaps#MakeMap("anum")|endif
endif

" ---------------------------------------------------------------------
" Plug maps: (the real thing) {{{2
nnoremap <silent> <script> <Plug>AM_a?		<SID>WS:AlignCtrl mIp1P1lC ? : : : : <CR>:'a,.Align<CR>:'a,'z-1s/\(\s\+\)? /?\1/e<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_a,		<SID>WS:'y,'zs/\(\S\)\s\+/\1 /ge<CR>'yjma'zk:call AlignMaps#CharJoiner(",")<cr>:silent 'y,'zg/,/call AlignMaps#FixMultiDec()<CR>'z:exe "norm \<Plug>AM_adec"<cr><SID>WE
nnoremap <silent> <script> <Plug>AM_a<		<SID>WS:AlignCtrl mIp1P1=l << >><CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_a(      <SID>WS:AlignCtrl mIp0P1=l<CR>:'a,.Align [(,]<CR>:sil 'y+1,'z-1s/\(\s\+\),/,\1/ge<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_a=		<SID>WS:AlignCtrl mIp1P1=l<CR>:AlignCtrl g :=<CR>:'a,'zAlign :\==<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_abox	<SID>WS:let g:alignmaps_iws=substitute(getline("'a"),'^\(\s*\).*$','\1','e')<CR>:'a,'z-1s/^\s\+//e<CR>:'a,'z-1s/^.*$/@&@/<CR>:AlignCtrl m=p01P0w @<CR>:'a,.Align<CR>:'a,'z-1s/@/ * /<CR>:'a,'z-1s/@$/*/<CR>'aYP:s/./*/g<CR>0r/'zkYp:s/./*/g<CR>0r A/<Esc>:exe "'a-1,'z-1s/^/".g:alignmaps_iws."/e"<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_acom	<SID>WS:'a,.s/\/[*/]\/\=/@&@/e<CR>:'a,.s/\*\//@&/e<CR>:'y,'zs/^\( *\) @/\1@/e<CR>'zk:call AlignMaps#StdAlign(2)<CR>:'y,'zs/^\(\s*\) @/\1/e<CR>:'y,'zs/ @//eg<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_adcom	<SID>WS:'a,.v/^\s*\/[/*]/s/\/[*/]\*\=/@&@/e<CR>:'a,.v/^\s*\/[/*]/s/\*\//@&/e<CR>:'y,'zv/^\s*\/[/*]/s/^\( *\) @/\1@/e<CR>'zk:call AlignMaps#StdAlign(3)<cr>:'y,'zv/^\s*\/[/*]/s/^\(\s*\) @/\1/e<CR>:'y,'zs/ @//eg<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_aocom	<SID>WS:AlignPush<CR>:AlignCtrl g /[*/]<CR>:exe "norm \<Plug>AM_acom"<cr>:AlignPop<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_ascom	<SID>WS:'a,.s/\/[*/]/@&@/e<CR>:'a,.s/\*\//@&/e<CR>:silent! 'a,.g/^\s*@\/[*/]/s/@//ge<CR>:AlignCtrl v ^\s*\/[*/]<CR>:AlignCtrl g \/[*/]<CR>'zk:call AlignMaps#StdAlign(2)<cr>:'y,'zs/^\(\s*\) @/\1/e<CR>:'y,'zs/ @//eg<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_adec	<SID>WS:'a,'zs/\([^ \t/(]\)\([*&]\)/\1 \2/e<CR>:'y,'zv/^\//s/\([^ \t]\)\s\+/\1 /ge<CR>:'y,'zv/^\s*[*/]/s/\([^/][*&]\)\s\+/\1/ge<CR>:'y,'zv/^\s*[*/]/s/^\(\s*\%([a-zA-Z_][a-zA-Z_0-9<>:]*\s\+\%([a-zA-Z_*(&]\)\@=\)\+\)\([*(&]*\)\s*\([a-zA-Z0-9_()<>:]\+\)\s*\(\(\[.\{-}]\)*\)\s*\(=\)\=\s*\(.\{-}\)\=\s*;/\1@\2#@\3\4@\6@\7;@/e<CR>:'y,'zv/^\s*[*/]/s/\*\/\s*$/@*\//e<CR>:'y,'zv/^\s*[*/]/s/^\s\+\*/@@@@@* /e<CR>:'y,'zv/^\s*[*/]/s/^@@@@@\*\(.*[^*/]\)$/&@*/e<CR>'yjma'zk:AlignCtrl v ^\s*[*/#]<CR>:call AlignMaps#StdAlign(1)<cr>:'y,'zv/^\s*[*/]/s/@ //ge<CR>:'y,'zv/^\s*[*/]/s/\(\s*\);/;\1/e<CR>:'y,'zv/^#/s/# //e<CR>:'y,'zv/^\s\+[*/#]/s/\([^/*]\)\(\*\+\)\( \+\)/\1\3\2/e<CR>:'y,'zv/^\s\+[*/#]/s/\((\+\)\( \+\)\*/\2\1*/e<CR>:'y,'zv/^\s\+[*/#]/s/^\(\s\+\) \*/\1*/e<CR>:'y,'zv/^\s\+[*/#]/s/[ \t@]*$//e<CR>:'y,'zs/^[*]/ */e<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_adef	<SID>WS:AlignPush<CR>:AlignCtrl v ^\s*\(\/\*\<bar>\/\/\)<CR>:'a,.v/^\s*\(\/\*\<bar>\/\/\)/s/^\(\s*\)#\(\s\)*define\s*\(\I[a-zA-Z_0-9(),]*\)\s*\(.\{-}\)\($\<Bar>\/\*\)/#\1\2define @\3@\4@\5/e<CR>:'a,.v/^\s*\(\/\*\<bar>\/\/\)/s/\($\<Bar>\*\/\)/@&/e<CR>'zk:call AlignMaps#StdAlign(1)<cr>'yjma'zk:'a,.v/^\s*\(\/\*\<bar>\/\/\)/s/ @//g<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_afnc	:<c-u>set lz<CR>:silent call AlignMaps#Afnc()<CR>:set nolz<CR>
nnoremap <silent> <script> <Plug>AM_aunum	<SID>WS:'a,'zs/\([-+]\=\d\+\)\([eE][-+]\d\+\)\=/\1#\2/ge<CR>:'a,'zs/\([.eE][-+]\=\d\+\)#/\1/ge<CR>:'a,'zs/#\././ge<CR>:'a,'zs/[-+]\=\%(\d\+\%([.#]\d*\)\=\<bar>[.#]\d\+\)\%([eE][-+]\=\d\+\)\=/@&@/ge<CR>:AlignCtrl Imp0P0r<CR>:'a,'zAlign [@#.]<CR>:'a,'zs/\([.#]\)\(\s\+\)\(\d*\%([eE][-+]\=\d\+\)\=\)@/\1\3\2@/ge<CR>:'a,'zs/@//<CR>:'a,'zs/[#@]/ /ge<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_aenum	<SID>WS:'a,'zs/\([-+]\=\d\+\)\([eE][-+]\d\+\)\=/\1#\2/ge<CR>:'a,'zs/\([,eE][-+]\=\d\+\)#/\1/ge<CR>:'a,'zs/#,/,/ge<CR>:'a,'zs/[-+]\=\%(\d\+\%([,#]\d*\)\=\<bar>[,#]\d\+\)\%([eE][-+]\=\d\+\)\=/@&@/ge<CR>:AlignCtrl Imp0P0r<CR>:'a,'zAlign [@#,]<CR>:'a,'zs/\([,#]\)\(\s\+\)\(\d*\%([eE][-+]\=\d\+\)\=\)@/\1\3\2@/ge<CR>:'a,'zs/@//<CR>:'a,'zs/[#@]/ /ge<CR><SID>WE
" ---------------------------------------------------------------------
" html table alignment	{{{2
if !hasmapto('<Plug>AM_Htd')|map <unique> <Leader>Htd	<Plug>AM_Htd|endif
map <silent> <script> <Plug>AM_Htd <SID>WS:'y,'zs%<[tT][rR]><[tT][dD][^>]\{-}>\<Bar></[tT][dD]><[tT][dD][^>]\{-}>\<Bar></[tT][dD]></[tT][rR]>%@&@%g<CR>'yjma'zk:AlignCtrl m=Ilp1P0 @<CR>:'a,.Align<CR>:'y,'zs/ @/@/<CR>:'y,'zs/@ <[tT][rR]>/<[tT][rR]>/ge<CR>:'y,'zs/@//ge<CR><SID>WE

" ---------------------------------------------------------------------
" character-based right-justified alignment maps {{{2
if !hasmapto('<Plug>AM_T|')|call AlignMaps#MakeMap("T|")|endif
if !hasmapto('<Plug>AM_T#')	 |call AlignMaps#MakeMap("T#")|endif
if !hasmapto('<Plug>AM_T,')	 |call AlignMaps#MakeMap("T,")|endif
if !hasmapto('<Plug>AM_Ts,') |call AlignMaps#MakeMap("Ts,")|endif
if !hasmapto('<Plug>AM_T:')	 |call AlignMaps#MakeMap("T:")|endif
if !hasmapto('<Plug>AM_T;')	 |call AlignMaps#MakeMap("T;")|endif
if !hasmapto('<Plug>AM_T<')	 |call AlignMaps#MakeMap("T<")|endif
if !hasmapto('<Plug>AM_T=')	 |call AlignMaps#MakeMap("T=")|endif
if !hasmapto('<Plug>AM_T?')	 |call AlignMaps#MakeMap("T?")|endif
if !hasmapto('<Plug>AM_T@')	 |call AlignMaps#MakeMap("T@")|endif
if !hasmapto('<Plug>AM_TW@') |call AlignMaps#MakeMap("TW@")|endif
if !hasmapto('<Plug>AM_Tab') |call AlignMaps#MakeMap("Tab")|endif
if !hasmapto('<Plug>AM_Tsp') |call AlignMaps#MakeMap("Tsp")|endif
if !hasmapto('<Plug>AM_T~')	 |call AlignMaps#MakeMap("T~")|endif

nnoremap <silent> <script> <Plug>AM_T| <SID>WS:AlignCtrl mIp0P0=r <Bar><CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_T#   <SID>WS:AlignCtrl mIp0P0=r #<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_T,   <SID>WS:AlignCtrl mIp0P1=r ,<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_Ts,  <SID>WS:AlignCtrl mIp0P1=r ,<CR>:'a,.Align<CR>:'a,.s/\(\s*\),/,\1/ge<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_T:   <SID>WS:AlignCtrl mIp1P1=r :<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_T;   <SID>WS:AlignCtrl mIp0P0=r ;<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_T<   <SID>WS:AlignCtrl mIp0P0=r <<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_T=   <SID>WS:'a,'z-1s/\s\+\([*/+\-%<Bar>&\~^]\==\)/ \1/e<CR>:'a,'z-1s@ \+\([*/+\-%<Bar>&\~^]\)=@\1=@ge<CR>:'a,'z-1s/; */;@/e<CR>:'a,'z-1s/==/\="\<Char-0x0f>\<Char-0x0f>"/ge<CR>:'a,'z-1s/!=/\x="!\<Char-0x0f>"/ge<CR>:AlignCtrl mIp1P1=r = @<CR>:AlignCtrl g =<CR>:'a,'z-1Align<CR>:'a,'z-1s/; *@/;/e<CR>:'a,'z-1s/; *$/;/e<CR>:'a,'z-1s@\([*/+\-%<Bar>&\~^]\)\( \+\)=@\2\1=@ge<CR>:'a,'z-1s/\( \+\);/;\1/ge<CR>:'a,'z-1s/\xff/=/ge<CR><SID>WE:exe "norm <Plug>acom"
nnoremap <silent> <script> <Plug>AM_T?   <SID>WS:AlignCtrl mIp0P0=r ?<CR>:'a,.Align<CR>:'y,'zs/ \( *\);/;\1/ge<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_T@   <SID>WS:AlignCtrl mIp0P0=r @<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_TW@  <SID>WS:AlignCtrl mWp0P0=r @<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_Tab  <SID>WS:'a,.s/^\(\t*\)\(.*\)/\=submatch(1).escape(substitute(submatch(2),'\t','@','g'),'\')/<CR>:AlignCtrl mI=r @<CR>:'a,.Align<CR>:'y+1,'z-1s/@/ /g<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_Tsp  <SID>WS:'a,.s/^\(\s*\)\(.*\)/\=submatch(1).escape(substitute(submatch(2),'\s\+','@','g'),'\')/<CR>:AlignCtrl mI=r @<CR>:'a,.Align<CR>:'y+1,'z-1s/@/ /g<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_T~   <SID>WS:AlignCtrl mIp0P0=r ~<CR>:'a,.Align<CR>:'y,'zs/ \( *\);/;\1/ge<CR><SID>WE

" ---------------------------------------------------------------------
" character-based left-justified alignment maps {{{2
if !hasmapto('<Plug>AM_t|','n')	|call AlignMaps#MakeMap("t|")|endif
if !hasmapto('<Plug>AM_t#','n')		|call AlignMaps#MakeMap("t#")|endif
if !hasmapto('<Plug>AM_t,,','n')		|call AlignMaps#MakeMap("t,,")|endif
if !hasmapto('<Plug>AM_t:','n')		|call AlignMaps#MakeMap("t:")|endif
if !hasmapto('<Plug>AM_t;','n')		|call AlignMaps#MakeMap("t;")|endif
if !hasmapto('<Plug>AM_t<','n')		|call AlignMaps#MakeMap("t<")|endif
if !hasmapto('<Plug>AM_t=','n')		|call AlignMaps#MakeMap("t=")|endif
if !hasmapto('<Plug>AM_ts,','n')	|call AlignMaps#MakeMap("ts,")|endif
if !hasmapto('<Plug>AM_ts:','n')	|call AlignMaps#MakeMap("ts:")|endif
if !hasmapto('<Plug>AM_ts;','n')	|call AlignMaps#MakeMap("ts;")|endif
if !hasmapto('<Plug>AM_ts<','n')	|call AlignMaps#MakeMap("ts<")|endif
if !hasmapto('<Plug>AM_ts=','n')	|call AlignMaps#MakeMap("ts=")|endif
if !hasmapto('<Plug>AM_w=','n')		|call AlignMaps#MakeMap("w=")|endif
if !hasmapto('<Plug>AM_t?','n')		|call AlignMaps#MakeMap("t?")|endif
if !hasmapto('<Plug>AM_t~','n')		|call AlignMaps#MakeMap("t~")|endif
if !hasmapto('<Plug>AM_t@','n')		|call AlignMaps#MakeMap("t@")|endif
if !hasmapto('<Plug>AM_tW@','n')	|call AlignMaps#MakeMap("tW@")|endif
if !hasmapto('<Plug>AM_m=','n')		|call AlignMaps#MakeMap("m=")|endif
if !hasmapto('<Plug>AM_tab','n')	|call AlignMaps#MakeMap("tab")|endif
if !hasmapto('<Plug>AM_tml','n')	|call AlignMaps#MakeMap("tml")|endif
if !hasmapto('<Plug>AM_tsp','n')	|call AlignMaps#MakeMap("tsp")|endif
if !hasmapto('<Plug>AM_tsq','n')	|call AlignMaps#MakeMap("tsq")|endif
if !hasmapto('<Plug>AM_tt','n')		|call AlignMaps#MakeMap("tt")|endif
if !hasmapto('<Plug>AM_tab','n')	|call AlignMaps#MakeMap("tab")|endif

" <Plug> normal mode mappings
nnoremap <silent> <script> <Plug>AM_t|	<SID>WS:AlignCtrl mIp0P0=l <Bar><CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_t#		<SID>WS:AlignCtrl mIp0P0=l #<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_t,		<SID>WS:AlignCtrl mIp0P1=l ,<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_t:		<SID>WS:AlignCtrl mIp1P1=l :<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_t;		<SID>WS:AlignCtrl mIp0P1=l ;<CR>:'a,.Align<CR>:sil 'y,'zs/\( *\);/;\1/ge<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_t<		<SID>WS:AlignCtrl mIp0P0=l <<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_t=		<SID>WS:call AlignMaps#Equals()<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_ts,		<SID>WS:AlignCtrl mIp0P1=l #\zs<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_ts,		<SID>WS:AlignCtrl mIp0P1=l ,\zs<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_ts:		<SID>WS:AlignCtrl mIp1P1=l :\zs<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_ts;		<SID>WS:AlignCtrl mIp1P1=l ;\zs<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_ts<		<SID>WS:AlignCtrl mIp1P1=l <\zs<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_ts=		<SID>WS:AlignCtrl mIp1P1=l =\zs<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_w=		<SID>WS:'a,'zg/=/s/\s\+\([*/+\-%<Bar>&\~^]\==\)/ \1/e<CR>:'a,'zg/=/s@ \+\([*/+\-%<Bar>&\~^]\)=@\1=@ge<CR>:'a,'zg/=/s/==/\="\<Char-0x0f>\<Char-0x0f>"/ge<CR>:'a,'zg/=/s/!=/\="!\<Char-0x0f>"/ge<CR>'zk:AlignCtrl mWp1P1=l =<CR>:AlignCtrl g =<CR>:'a,'z-1g/=/Align<CR>:'a,'z-1g/=/s@\([*/+\-%<Bar>&\~^!=]\)\( \+\)=@\2\1=@ge<CR>:'a,'z-1g/=/s/\( \+\);/;\1/ge<CR>:'a,'z-1v/^\s*\/[*/]/s/\/[*/]/@&@/e<CR>:'a,'z-1v/^\s*\/[*/]/s/\*\//@&/e<CR>'zk:call AlignMaps#StdAlign(1)<cr>:'y,'zs/^\(\s*\) @/\1/e<CR>:'a,'z-1g/=/s/\xff/=/ge<CR>:'y,'zg/=/s/ @//eg<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_t?		<SID>WS:AlignCtrl mIp0P0=l ?<CR>:'a,.Align<CR>:.,'zs/ \( *\);/;\1/ge<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_t~		<SID>WS:AlignCtrl mIp0P0=l ~<CR>:'a,.Align<CR>:'y,'zs/ \( *\);/;\1/ge<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_t@		<SID>WS:call AlignMaps#StdAlign(1)<cr><SID>WE
nnoremap <silent> <script> <Plug>AM_tW@		<SID>WS:call AlignMaps#StdAlign(2)<cr><SID>WE
nnoremap <silent> <script> <Plug>AM_m=		<SID>WS:'a,'zs/\s\+\([*/+\-%<Bar>&\~^]\==\)/ \1/e<CR>:'a,'zs@ \+\([*/+\-%<Bar>&\~^]\)=@\1=@ge<CR>:'a,'zs/==/\="\<Char-0x0f>\<Char-0x0f>"/ge<CR>:'a,'zs/!=/\="!\<Char-0x0f>"/ge<CR>'zk:AlignCtrl mIp1P1=l =<CR>:AlignCtrl g =<CR>:'a,'z-1Align<CR>:'a,'z-1s@\([*/+\-%<Bar>&\~^!=]\)\( \+\)=@\2\1=@ge<CR>:'a,'z-1s/\( \+\);/;\1/ge<CR>:'a,'z-s/%\ze[^=]/ @%@ /e<CR>'zk:call AlignMaps#StdAlign(1)<cr>:'y,'zs/^\(\s*\) @/\1/e<CR>:'a,'z-1s/\xff/=/ge<CR>:'y,'zs/ @//eg<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_tab		<SID>WS:'a,.s/^\(\t*\)\(.*\)$/\=submatch(1).escape(substitute(submatch(2),'\t',"\<Char-0x0f>",'g'),'\')/<CR>:if &ts == 1<bar>exe "AlignCtrl mI=lp0P0 \<Char-0x0f>"<bar>else<bar>exe "AlignCtrl mI=l"<bar>endif<CR>:'a,.Align <Char-0x0f><CR>:exe "'y+1,'z-1s/\<Char-0x0f>/".((&ts == 1)? '\t' : ' ')."/g"<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_tml		<SID>WS:AlignCtrl mWp1P0=l \\\@<!\\\s*$<CR>:'a,.Align<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_tsp		<SID>WS:keepj 'a,.s/^\(\s*\)\(.*\)/\=submatch(1).escape(substitute(submatch(2),'\s\+','@','g'),'\')/<CR>:AlignCtrl mI=lp0P0 @<CR>:'a,.Align<CR>:keepj 'y+1,'z-1s/@/ /g<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_tsq		<SID>WS:'a,.AlignReplaceQuotedSpaces<CR>:keepj 'a,.s/^\(\s*\)\(.*\)/\=submatch(1).substitute(submatch(2),'\s\+','@','g')/<CR>:AlignCtrl mIp0P0=l @<CR>:'a,.Align<CR>:keepj 'y+1,'z-1s/[%@]/ /g<CR><SID>WE
nnoremap <silent> <script> <Plug>AM_tt		<SID>WS:AlignCtrl mIp1P1=l \\\@<!& \\\\<CR>:'a,.Align<CR><SID>WE

" =====================================================================
" Menu Support: {{{1
"   ma ..move.. use menu
"   v V or ctrl-v ..move.. use menu
if has("menu") && has("gui_running") && &go =~# 'm' && !exists("s:firstmenu")
 let s:firstmenu= 1
 if !exists("g:DrChipTopLvlMenu")
  let g:DrChipTopLvlMenu= "DrChip."
 endif
 if g:DrChipTopLvlMenu != ""
  let s:mapleader = exists("g:mapleader")? g:mapleader : '\'
  let s:emapleader= escape(s:mapleader,'\ ')
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.<<\ and\ >><tab>'.s:emapleader.'a<	'.s:mapleader.'a<'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Assignment\ =<tab>'.s:emapleader.'t=	'.s:mapleader.'t='
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Assignment\ :=<tab>'.s:emapleader.'a=	'.s:mapleader.'a='
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Backslashes<tab>'.s:emapleader.'tml	'.s:mapleader.'tml'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Breakup\ Comma\ Declarations<tab>'.s:emapleader.'a,	'.s:mapleader.'a,'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.C\ Comment\ Box<tab>'.s:emapleader.'abox	'.s:mapleader.'abox'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Commas<tab>'.s:emapleader.'t,	'.s:mapleader.'t,'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Commas<tab>'.s:emapleader.'ts,	'.s:mapleader.'ts,'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Commas\ With\ Strings<tab>'.s:emapleader.'tsq	'.s:mapleader.'tsq'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Comments<tab>'.s:emapleader.'acom	'.s:mapleader.'acom'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Comments\ Only<tab>'.s:emapleader.'aocom	'.s:mapleader.'aocom'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Declaration\ Comments<tab>'.s:emapleader.'adcom	'.s:mapleader.'adcom'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Declarations<tab>'.s:emapleader.'adec	'.s:mapleader.'adec'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Definitions<tab>'.s:emapleader.'adef	'.s:mapleader.'adef'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Function\ Header<tab>'.s:emapleader.'afnc	'.s:mapleader.'afnc'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Html\ Tables<tab>'.s:emapleader.'Htd	'.s:mapleader.'Htd'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.(\.\.\.)?\.\.\.\ :\ \.\.\.<tab>'.s:emapleader.'a?	'.s:mapleader.'a?'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Numbers<tab>'.s:emapleader.'anum	'.s:mapleader.'anum'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Numbers\ (American-Style)<tab>'.s:emapleader.'aunum	<Leader>aunum	'.s:mapleader.'aunum	<Leader>aunum'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Numbers\ (Euro-Style)<tab>'.s:emapleader.'aenum	'.s:mapleader.'aenum'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Spaces\ (Left\ Justified)<tab>'.s:emapleader.'tsp	'.s:mapleader.'tsp'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Spaces\ (Right\ Justified)<tab>'.s:emapleader.'Tsp	'.s:mapleader.'Tsp'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Statements\ With\ Percent\ Style\ Comments<tab>'.s:emapleader.'m=	'.s:mapleader.'m='
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Symbol\ <<tab>'.s:emapleader.'t<	'.s:mapleader.'t<'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Symbol\ \|<tab>'.s:emapleader.'t\|	'.s:mapleader.'t|'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Symbol\ @<tab>'.s:emapleader.'t@	'.s:mapleader.'t@'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Symbol\ #<tab>'.s:emapleader.'t#	'.s:mapleader.'t#'
  exe 'menu '.g:DrChipTopLvlMenu.'AlignMaps.Tabs<tab>'.s:emapleader.'tab	'.s:mapleader.'tab'
  unlet s:mapleader
  unlet s:emapleader
 endif
endif

" =====================================================================
"  Restore: {{{1
let &cpo= s:keepcpo
unlet s:keepcpo

" ==============================================================================
"  Modelines: {{{1
" vim: ts=4 nowrap fdm=marker
