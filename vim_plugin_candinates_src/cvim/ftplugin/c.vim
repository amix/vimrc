" ------------------------------------------------------------------------------
"
" Vim filetype plugin file
"
"   Language :  C / C++
"     Plugin :  c.vim 
" Maintainer :  Fritz Mehner <mehner@fh-swf.de>
"   Revision :  $Id: c.vim,v 1.71 2011/12/27 21:04:33 mehner Exp $
"
" ------------------------------------------------------------------------------
"
" Only do this when not done yet for this buffer
" 
if exists("b:did_C_ftplugin")
  finish
endif
let b:did_C_ftplugin = 1
"
" ---------- system installation or local installation ----------
"
let s:installation				= 'local'
if match( expand("<sfile>"), escape( $VIM, ' \' ) ) == 0
	let s:installation						= 'system'
endif
"
" ---------- Do we have a mapleader other than '\' ? ------------
"
if exists("g:C_MapLeader")
  let maplocalleader  = g:C_MapLeader
endif    
"
" ---------- C/C++ dictionary -----------------------------------
" This will enable keyword completion for C and C++
" using Vim's dictionary feature |i_CTRL-X_CTRL-K|.
" Set the new dictionaries in front of the existing ones
" 
if exists("g:C_Dictionary_File")
  let save=&dictionary
  silent! exe 'setlocal dictionary='.g:C_Dictionary_File
  silent! exe 'setlocal dictionary+='.save
endif    
"
" ---------- F-key mappings  ------------------------------------
"
"   Alt-F9   write buffer and compile
"       F9   compile and link
"  Ctrl-F9   run executable
" Shift-F9   command line arguments
"
 map  <buffer>  <silent>  <A-F9>       :call C_Compile()<CR>:call C_HlMessage()<CR>
imap  <buffer>  <silent>  <A-F9>  <C-C>:call C_Compile()<CR>:call C_HlMessage()<CR>
"
 map  <buffer>  <silent>    <F9>       :call C_Link()<CR>:call C_HlMessage()<CR>
imap  <buffer>  <silent>    <F9>  <C-C>:call C_Link()<CR>:call C_HlMessage()<CR>
"
 map  <buffer>  <silent>  <C-F9>       :call C_Run()<CR>
imap  <buffer>  <silent>  <C-F9>  <C-C>:call C_Run()<CR>
"
 map  <buffer>  <silent>  <S-F9>       :call C_Arguments()<CR>
imap  <buffer>  <silent>  <S-F9>  <C-C>:call C_Arguments()<CR>
"
" ---------- alternate file plugin (a.vim) ----------------------
"
if exists("loaded_alternateFile")
 map  <buffer>  <silent>  <S-F2>       :A<CR>
imap  <buffer>  <silent>  <S-F2>  <C-C>:A<CR>
endif
"
command! -nargs=1 -complete=customlist,C_CFileSectionList        CFileSection       call C_CFileSectionListInsert   (<f-args>)
command! -nargs=1 -complete=customlist,C_HFileSectionList        HFileSection       call C_HFileSectionListInsert   (<f-args>)
command! -nargs=1 -complete=customlist,C_KeywordCommentList      KeywordComment     call C_KeywordCommentListInsert (<f-args>)
command! -nargs=1 -complete=customlist,C_SpecialCommentList      SpecialComment     call C_SpecialCommentListInsert (<f-args>)
command! -nargs=1 -complete=customlist,C_StdLibraryIncludesList  IncludeStdLibrary  call C_StdLibraryIncludesInsert (<f-args>)
command! -nargs=1 -complete=customlist,C_C99LibraryIncludesList  IncludeC99Library  call C_C99LibraryIncludesInsert (<f-args>)
command! -nargs=1 -complete=customlist,C_CppLibraryIncludesList  IncludeCppLibrary  call C_CppLibraryIncludesInsert (<f-args>)
command! -nargs=1 -complete=customlist,C_CppCLibraryIncludesList IncludeCppCLibrary call C_CppCLibraryIncludesInsert(<f-args>)
command! -nargs=1 -complete=customlist,C_StyleList               CStyle             call C_Style                    (<f-args>)

" ---------- KEY MAPPINGS : MENU ENTRIES -------------------------------------
" ---------- comments menu  ------------------------------------------------
"
 noremap    <buffer>  <silent>  <LocalLeader>cl         :call C_EndOfLineComment()<CR>
inoremap    <buffer>  <silent>  <LocalLeader>cl    <Esc>:call C_EndOfLineComment()<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>cl         :call C_EndOfLineComment()<CR>
"
nnoremap    <buffer>  <silent>  <LocalLeader>cj         :call C_AdjustLineEndComm()<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>cj         :call C_AdjustLineEndComm()<CR>
inoremap    <buffer>  <silent>  <LocalLeader>cj    <Esc>:call C_AdjustLineEndComm()<CR>a
"
 noremap    <buffer>  <silent>  <LocalLeader>cs         :call C_GetLineEndCommCol()<CR>

 noremap    <buffer>  <silent>  <LocalLeader>c*         :call C_CodeToCommentC()<CR>:nohlsearch<CR>j
vnoremap    <buffer>  <silent>  <LocalLeader>c*         :call C_CodeToCommentC()<CR>:nohlsearch<CR>j

 noremap    <buffer>  <silent>  <LocalLeader>cc         :call C_CodeToCommentCpp()<CR>:nohlsearch<CR>j
vnoremap    <buffer>  <silent>  <LocalLeader>cc         :call C_CodeToCommentCpp()<CR>:nohlsearch<CR>j
 noremap    <buffer>  <silent>  <LocalLeader>co         :call C_CommentToCode()<CR>:nohlsearch<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>co         :call C_CommentToCode()<CR>:nohlsearch<CR>

 noremap    <buffer>  <silent>  <LocalLeader>cfr        :call C_InsertTemplate("comment.frame")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>cfu        :call C_InsertTemplate("comment.function")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>cme        :call C_InsertTemplate("comment.method")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>ccl        :call C_InsertTemplate("comment.class")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>cfdi       :call C_InsertTemplate("comment.file-description")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>cfdh       :call C_InsertTemplate("comment.file-description-header")<CR>

inoremap    <buffer>  <silent>  <LocalLeader>cfr   <Esc>:call C_InsertTemplate("comment.frame")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>cfu   <Esc>:call C_InsertTemplate("comment.function")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>cme   <Esc>:call C_InsertTemplate("comment.method")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ccl   <Esc>:call C_InsertTemplate("comment.class")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>cfdi  <Esc>:call C_InsertTemplate("comment.file-description")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>cfdh  <Esc>:call C_InsertTemplate("comment.file-description-header")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>cd    <Esc>:call C_InsertDateAndTime('d')<CR>
inoremap    <buffer>  <silent>  <LocalLeader>cd    <Esc>:call C_InsertDateAndTime('d')<CR>a
vnoremap    <buffer>  <silent>  <LocalLeader>cd   s<Esc>:call C_InsertDateAndTime('d')<CR>a
 noremap    <buffer>  <silent>  <LocalLeader>ct    <Esc>:call C_InsertDateAndTime('dt')<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ct    <Esc>:call C_InsertDateAndTime('dt')<CR>a
vnoremap    <buffer>  <silent>  <LocalLeader>ct   s<Esc>:call C_InsertDateAndTime('dt')<CR>a
" 
 noremap    <buffer>  <silent>  <LocalLeader>cx         :call C_CommentToggle( )<CR>
inoremap    <buffer>  <silent>  <LocalLeader>cx    <Esc>:call C_CommentToggle( )<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>cx         :call C_CommentToggle( )<CR>
"
" call the above defined commands:
"
 noremap    <buffer>            <LocalLeader>ccs   <Esc>:CFileSection<Space>
 noremap    <buffer>            <LocalLeader>chs   <Esc>:HFileSection<Space>
 noremap    <buffer>            <LocalLeader>ckc   <Esc>:KeywordComment<Space>
 noremap    <buffer>            <LocalLeader>csc   <Esc>:SpecialComment<Space>
"
inoremap    <buffer>            <LocalLeader>ccs   <Esc>:CFileSection<Space>
inoremap    <buffer>            <LocalLeader>chs   <Esc>:HFileSection<Space>
inoremap    <buffer>            <LocalLeader>ckc   <Esc>:KeywordComment<Space>
inoremap    <buffer>            <LocalLeader>csc   <Esc>:SpecialComment<Space>
" 
" ---------- statements menu  ------------------------------------------------
"
 noremap    <buffer>  <silent>  <LocalLeader>sd         :call C_InsertTemplate("statements.do-while")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>sd    <Esc>:call C_InsertTemplate("statements.do-while", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>sd    <Esc>:call C_InsertTemplate("statements.do-while")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>sf         :call C_InsertTemplate("statements.for")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>sf    <Esc>:call C_InsertTemplate("statements.for")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>sfo        :call C_InsertTemplate("statements.for-block")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>sfo   <Esc>:call C_InsertTemplate("statements.for-block", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>sfo   <Esc>:call C_InsertTemplate("statements.for-block")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>si         :call C_InsertTemplate("statements.if")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>si    <Esc>:call C_InsertTemplate("statements.if")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>sif        :call C_InsertTemplate("statements.if-block")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>sif   <Esc>:call C_InsertTemplate("statements.if-block", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>sif   <Esc>:call C_InsertTemplate("statements.if-block")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>sie        :call C_InsertTemplate("statements.if-else")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>sie   <Esc>:call C_InsertTemplate("statements.if-else", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>sie   <Esc>:call C_InsertTemplate("statements.if-else")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>sife       :call C_InsertTemplate("statements.if-block-else")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>sife  <Esc>:call C_InsertTemplate("statements.if-block-else", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>sife  <Esc>:call C_InsertTemplate("statements.if-block-else")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>se         :call C_InsertTemplate("statements.else-block")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>se    <Esc>:call C_InsertTemplate("statements.else-block", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>se    <Esc>:call C_InsertTemplate("statements.else-block")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>sw         :call C_InsertTemplate("statements.while")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>sw    <Esc>:call C_InsertTemplate("statements.while")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>swh        :call C_InsertTemplate("statements.while-block")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>swh   <Esc>:call C_InsertTemplate("statements.while-block", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>swh   <Esc>:call C_InsertTemplate("statements.while-block")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>ss         :call C_InsertTemplate("statements.switch")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>ss    <Esc>:call C_InsertTemplate("statements.switch", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ss    <Esc>:call C_InsertTemplate("statements.switch")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>sc         :call C_InsertTemplate("statements.case")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>sc    <Esc>:call C_InsertTemplate("statements.case")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>s{         :call C_InsertTemplate("statements.block")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>s{    <Esc>:call C_InsertTemplate("statements.block", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>s{    <Esc>:call C_InsertTemplate("statements.block")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>sb         :call C_InsertTemplate("statements.block")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>sb    <Esc>:call C_InsertTemplate("statements.block", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>sb    <Esc>:call C_InsertTemplate("statements.block")<CR>
"
" ---------- preprocessor menu  ----------------------------------------------
"
 noremap    <buffer>  <LocalLeader>ps                  :IncludeStdLibrary<Space>
inoremap    <buffer>  <LocalLeader>ps             <Esc>:IncludeStdLibrary<Space>
 noremap    <buffer>  <LocalLeader>pc                  :IncludeC99Library<Space>
inoremap    <buffer>  <LocalLeader>pc             <Esc>:IncludeC99Library<Space>
 noremap    <buffer>  <LocalLeader>+ps                 :IncludeCppLibrary<Space>
inoremap    <buffer>  <LocalLeader>+ps            <Esc>:IncludeCppLibrary<Space>
 noremap    <buffer>  <LocalLeader>+pc                 :IncludeCppCLibrary<Space>
inoremap    <buffer>  <LocalLeader>+pc            <Esc>:IncludeCppC9Library<Space>
"
 noremap    <buffer>  <silent>  <LocalLeader>p<        :call C_InsertTemplate("preprocessor.include-global")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>p"        :call C_InsertTemplate("preprocessor.include-local")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>pd        :call C_InsertTemplate("preprocessor.define")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>pu        :call C_InsertTemplate("preprocessor.undefine")<CR>
"
inoremap    <buffer>  <silent>  <LocalLeader>p<   <Esc>:call C_InsertTemplate("preprocessor.include-global")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>p"   <Esc>:call C_InsertTemplate("preprocessor.include-local")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>pd   <Esc>:call C_InsertTemplate("preprocessor.define")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>pu   <Esc>:call C_InsertTemplate("preprocessor.undefine")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>pif       :call C_InsertTemplate("preprocessor.if-endif")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>pie       :call C_InsertTemplate("preprocessor.if-else-endif")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>pid       :call C_InsertTemplate("preprocessor.ifdef-else-endif")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>pin       :call C_InsertTemplate("preprocessor.ifndef-else-endif")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>pind      :call C_InsertTemplate("preprocessor.ifndef-def-endif")<CR>

vnoremap    <buffer>  <silent>  <LocalLeader>pif  <Esc>:call C_InsertTemplate("preprocessor.if-endif", "v")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>pie  <Esc>:call C_InsertTemplate("preprocessor.if-else-endif", "v")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>pid  <Esc>:call C_InsertTemplate("preprocessor.ifdef-else-endif", "v")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>pin  <Esc>:call C_InsertTemplate("preprocessor.ifndef-else-endif", "v")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>pind <Esc>:call C_InsertTemplate("preprocessor.ifndef-def-endif", "v")<CR>
                                     
inoremap    <buffer>  <silent>  <LocalLeader>pif  <Esc>:call C_InsertTemplate("preprocessor.if-endif")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>pie  <Esc>:call C_InsertTemplate("preprocessor.if-else-endif")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>pid  <Esc>:call C_InsertTemplate("preprocessor.ifdef-else-endif")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>pin  <Esc>:call C_InsertTemplate("preprocessor.ifndef-else-endif")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>pind <Esc>:call C_InsertTemplate("preprocessor.ifndef-def-endif")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>pi0       :call C_PPIf0("a")<CR>2ji
inoremap    <buffer>  <silent>  <LocalLeader>pi0  <Esc>:call C_PPIf0("a")<CR>2ji
vnoremap    <buffer>  <silent>  <LocalLeader>pi0  <Esc>:call C_PPIf0("v")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>pr0       :call C_PPIf0Remove()<CR>
inoremap    <buffer>  <silent>  <LocalLeader>pr0  <Esc>:call C_PPIf0Remove()<CR>
"
 noremap    <buffer>  <silent>  <LocalLeader>pe        :call C_InsertTemplate("preprocessor.error")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>pl        :call C_InsertTemplate("preprocessor.line")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>pp        :call C_InsertTemplate("preprocessor.pragma")<CR>
"
inoremap    <buffer>  <silent>  <LocalLeader>pe   <Esc>:call C_InsertTemplate("preprocessor.error")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>pl   <Esc>:call C_InsertTemplate("preprocessor.line")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>pp   <Esc>:call C_InsertTemplate("preprocessor.pragma")<CR>
"
" ---------- idioms menu  ----------------------------------------------------
"
 noremap    <buffer>  <silent>  <LocalLeader>if         :call C_InsertTemplate("idioms.function")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>if    <Esc>:call C_InsertTemplate("idioms.function", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>if    <Esc>:call C_InsertTemplate("idioms.function")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>isf        :call C_InsertTemplate("idioms.function-static")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>isf   <Esc>:call C_InsertTemplate("idioms.function-static", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>isf   <Esc>:call C_InsertTemplate("idioms.function-static")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>im         :call C_InsertTemplate("idioms.main")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>im    <Esc>:call C_InsertTemplate("idioms.main", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>im    <Esc>:call C_InsertTemplate("idioms.main")<CR>
"
 noremap    <buffer>  <silent>  <LocalLeader>i0         :call C_CodeFor("up"  )<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>i0         :call C_CodeFor("up"  )<CR>
inoremap    <buffer>  <silent>  <LocalLeader>i0    <Esc>:call C_CodeFor("up"  )<CR>i
 noremap    <buffer>  <silent>  <LocalLeader>in         :call C_CodeFor("down")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>in         :call C_CodeFor("down")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>in    <Esc>:call C_CodeFor("down")<CR>i
"
 noremap    <buffer>  <silent>  <LocalLeader>ie         :call C_InsertTemplate("idioms.enum")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>ie    <Esc>:call C_InsertTemplate("idioms.enum"  , "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ie    <Esc>:call C_InsertTemplate("idioms.enum")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>is         :call C_InsertTemplate("idioms.struct")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>is    <Esc>:call C_InsertTemplate("idioms.struct", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>is    <Esc>:call C_InsertTemplate("idioms.struct")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>iu         :call C_InsertTemplate("idioms.union")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>iu    <Esc>:call C_InsertTemplate("idioms.union" , "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>iu    <Esc>:call C_InsertTemplate("idioms.union")<CR>
"
 noremap    <buffer>  <silent>  <LocalLeader>ip         :call C_InsertTemplate("idioms.printf")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ip    <Esc>:call C_InsertTemplate("idioms.printf")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>isc        :call C_InsertTemplate("idioms.scanf")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>isc   <Esc>:call C_InsertTemplate("idioms.scanf")<CR>
"
 noremap    <buffer>  <silent>  <LocalLeader>ica        :call C_InsertTemplate("idioms.calloc")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ica   <Esc>:call C_InsertTemplate("idioms.calloc")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>ima        :call C_InsertTemplate("idioms.malloc")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ima   <Esc>:call C_InsertTemplate("idioms.malloc")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>ire        :call C_InsertTemplate("idioms.realloc")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ire   <Esc>:call C_InsertTemplate("idioms.realloc")<CR>
"
 noremap    <buffer>  <silent>  <LocalLeader>isi        :call C_InsertTemplate("idioms.sizeof")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>isi   <Esc>:call C_InsertTemplate("idioms.sizeof")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>isi   <Esc>:call C_InsertTemplate("idioms.sizeof", "v")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>ias        :call C_InsertTemplate("idioms.assert")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>ias   <Esc>:call C_InsertTemplate("idioms.assert", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ias   <Esc>:call C_InsertTemplate("idioms.assert")<CR>
"
 noremap    <buffer>  <silent>  <LocalLeader>ii         :call C_InsertTemplate("idioms.open-input-file")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ii    <Esc>:call C_InsertTemplate("idioms.open-input-file")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>ii    <Esc>:call C_InsertTemplate("idioms.open-input-file", "v")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>io         :call C_InsertTemplate("idioms.open-output-file")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>io    <Esc>:call C_InsertTemplate("idioms.open-output-file")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>io    <Esc>:call C_InsertTemplate("idioms.open-output-file", "v")<CR>
"
 noremap    <buffer>  <silent>  <LocalLeader>ifs        :call C_InsertTemplate("idioms.fscanf")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ifs   <Esc>:call C_InsertTemplate("idioms.fscanf")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>ifp        :call C_InsertTemplate("idioms.fprintf")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ifp   <Esc>:call C_InsertTemplate("idioms.fprintf")<CR>
"
" ---------- snippet menu : snippets -----------------------------------------
"
 noremap    <buffer>  <silent>  <LocalLeader>nr         :call C_CodeSnippet("r")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>nw         :call C_CodeSnippet("w")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>nw    <Esc>:call C_CodeSnippet("wv")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>ne         :call C_CodeSnippet("e")<CR>
"
inoremap    <buffer>  <silent>  <LocalLeader>nr    <Esc>:call C_CodeSnippet("r")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>nw    <Esc>:call C_CodeSnippet("w")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ne    <Esc>:call C_CodeSnippet("e")<CR>
"
" ---------- snippet menu : prototypes ---------------------------------------
"
 noremap    <buffer>  <silent>  <LocalLeader>np        :call C_ProtoPick("function")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>np        :call C_ProtoPick("function")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>np   <Esc>:call C_ProtoPick("function")<CR>
"                                                                                 
 noremap    <buffer>  <silent>  <LocalLeader>nf        :call C_ProtoPick("function")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>nf        :call C_ProtoPick("function")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>nf   <Esc>:call C_ProtoPick("function")<CR>
"
 noremap    <buffer>  <silent>  <LocalLeader>nm        :call C_ProtoPick("method")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>nm        :call C_ProtoPick("method")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>nm   <Esc>:call C_ProtoPick("method")<CR>
"
 noremap    <buffer>  <silent>  <LocalLeader>ni         :call C_ProtoInsert()<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ni    <Esc>:call C_ProtoInsert()<CR>
"
 noremap    <buffer>  <silent>  <LocalLeader>nc         :call C_ProtoClear()<CR>
inoremap    <buffer>  <silent>  <LocalLeader>nc    <Esc>:call C_ProtoClear()<CR>
"
 noremap    <buffer>  <silent>  <LocalLeader>ns         :call C_ProtoShow()<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ns    <Esc>:call C_ProtoShow()<CR>
"
" ---------- snippet menu : templates ----------------------------------------
"
 noremap    <buffer>  <silent>  <LocalLeader>ntl        :call C_BrowseTemplateFiles("Local")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>ntl   <Esc>:call C_BrowseTemplateFiles("Local")<CR>
 if s:installation == 'system'
	 noremap    <buffer>  <silent>  <LocalLeader>ntg        :call C_BrowseTemplateFiles("Global")<CR>
	inoremap    <buffer>  <silent>  <LocalLeader>ntg   <Esc>:call C_BrowseTemplateFiles("Global")<CR>
 endif
 noremap    <buffer>  <silent>  <LocalLeader>ntr        :call C_RereadTemplates()<CR>
 noremap    <buffer>            <LocalLeader>nts        :CStyle<Space>
inoremap    <buffer>  <silent>  <LocalLeader>ntr   <Esc>:call C_RereadTemplates()<CR>
inoremap    <buffer>            <LocalLeader>nts   <Esc>:CStyle<Space>
"
" ---------- C++ menu ----------------------------------------------------
"
 noremap    <buffer>  <silent>  <LocalLeader>+"         :call C_InsertTemplate("cpp.cout-operator")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+"    <Esc>:call C_InsertTemplate("cpp.cout-operator")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>+co        :call C_InsertTemplate("cpp.cout")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+co   <Esc>:call C_InsertTemplate("cpp.cout")<CR>
"
 noremap    <buffer>  <silent>  <LocalLeader>+c         :call C_InsertTemplate("cpp.class-definition")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+c    <Esc>:call C_InsertTemplate("cpp.class-definition")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>+cn        :call C_InsertTemplate("cpp.class-using-new-definition")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+cn   <Esc>:call C_InsertTemplate("cpp.class-using-new-definition")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>+ci        :call C_InsertTemplate("cpp.class-implementation")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+ci   <Esc>:call C_InsertTemplate("cpp.class-implementation")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>+cni       :call C_InsertTemplate("cpp.class-using-new-implementation")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+cni  <Esc>:call C_InsertTemplate("cpp.class-using-new-implementation")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>+mi        :call C_InsertTemplate("cpp.method-implementation")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+mi   <Esc>:call C_InsertTemplate("cpp.method-implementation")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>+ai        :call C_InsertTemplate("cpp.accessor-implementation")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+ai   <Esc>:call C_InsertTemplate("cpp.accessor-implementation")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>+tc        :call C_InsertTemplate("cpp.template-class-definition")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+tc   <Esc>:call C_InsertTemplate("cpp.template-class-definition")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>+tcn       :call C_InsertTemplate("cpp.template-class-using-new-definition")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+tcn  <Esc>:call C_InsertTemplate("cpp.template-class-using-new-definition")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>+tci       :call C_InsertTemplate("cpp.template-class-implementation")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+tci  <Esc>:call C_InsertTemplate("cpp.template-class-implementation")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>+tcni      :call C_InsertTemplate("cpp.template-class-using-new-implementation")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+tcni <Esc>:call C_InsertTemplate("cpp.template-class-using-new-implementation")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>+tmi       :call C_InsertTemplate("cpp.template-method-implementation")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+tmi  <Esc>:call C_InsertTemplate("cpp.template-method-implementation")<CR>
 noremap    <buffer>  <silent>  <LocalLeader>+tai       :call C_InsertTemplate("cpp.template-accessor-implementation")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+tai  <Esc>:call C_InsertTemplate("cpp.template-accessor-implementation")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>+tf        :call C_InsertTemplate("cpp.template-function")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+tf   <Esc>:call C_InsertTemplate("cpp.template-function")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>+ec        :call C_InsertTemplate("cpp.error-class")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+ec   <Esc>:call C_InsertTemplate("cpp.error-class")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>+tr        :call C_InsertTemplate("cpp.try-catch")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>+tr   <Esc>:call C_InsertTemplate("cpp.try-catch", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+tr   <Esc>:call C_InsertTemplate("cpp.try-catch")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>+ca        :call C_InsertTemplate("cpp.catch")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>+ca   <Esc>:call C_InsertTemplate("cpp.catch", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+ca   <Esc>:call C_InsertTemplate("cpp.catch")<CR>

 noremap    <buffer>  <silent>  <LocalLeader>+c.        :call C_InsertTemplate("cpp.catch-points")<CR>
vnoremap    <buffer>  <silent>  <LocalLeader>+c.   <Esc>:call C_InsertTemplate("cpp.catch-points", "v")<CR>
inoremap    <buffer>  <silent>  <LocalLeader>+c.   <Esc>:call C_InsertTemplate("cpp.catch-points")<CR>
"
" ---------- run menu --------------------------------------------------------
"
 map    <buffer>  <silent>  <LocalLeader>rc         :call C_Compile()<CR>:call C_HlMessage()<CR>
 map    <buffer>  <silent>  <LocalLeader>rl         :call C_Link()<CR>:call C_HlMessage()<CR>
 map    <buffer>  <silent>  <LocalLeader>rr         :call C_Run()<CR>
 map    <buffer>  <silent>  <LocalLeader>ra         :call C_Arguments()<CR>
 map    <buffer>  <silent>  <LocalLeader>rm         :call C_Make()<CR>
 map    <buffer>  <silent>  <LocalLeader>rcm        :call C_ChooseMakefile()<CR>
 map    <buffer>  <silent>  <LocalLeader>rmc        :call C_MakeClean()<CR>
 map    <buffer>  <silent>  <LocalLeader>rme        :call C_MakeExeToRun()<CR>
 map    <buffer>  <silent>  <LocalLeader>rma        :call C_MakeArguments()<CR>
 map    <buffer>  <silent>  <LocalLeader>rp         :call C_SplintCheck()<CR>:call C_HlMessage()<CR>
 map    <buffer>  <silent>  <LocalLeader>rpa        :call C_SplintArguments()<CR>
 map    <buffer>  <silent>  <LocalLeader>rd         :call C_Indent()<CR>
 map    <buffer>  <silent>  <LocalLeader>rh         :call C_Hardcopy()<CR>
 map    <buffer>  <silent>  <LocalLeader>rs         :call C_Settings()<CR>
"
vmap    <buffer>  <silent>  <LocalLeader>rh         :call C_Hardcopy()<CR>
"
imap    <buffer>  <silent>  <LocalLeader>rc    <C-C>:call C_Compile()<CR>:call C_HlMessage()<CR>
imap    <buffer>  <silent>  <LocalLeader>rl    <C-C>:call C_Link()<CR>:call C_HlMessage()<CR>
imap    <buffer>  <silent>  <LocalLeader>rr    <C-C>:call C_Run()<CR>
imap    <buffer>  <silent>  <LocalLeader>ra    <C-C>:call C_Arguments()<CR>
imap    <buffer>  <silent>  <LocalLeader>rm    <C-C>:call C_Make()<CR>
imap    <buffer>  <silent>  <LocalLeader>rmc   <C-C>:call C_MakeClean()<CR>
imap    <buffer>  <silent>  <LocalLeader>rme   <C-C>:call C_MakeExeToRun()<CR>
imap    <buffer>  <silent>  <LocalLeader>rma   <C-C>:call C_MakeArguments()<CR>
imap    <buffer>  <silent>  <LocalLeader>rp    <C-C>:call C_SplintCheck()<CR>:call C_HlMessage()<CR>
imap    <buffer>  <silent>  <LocalLeader>rpa   <C-C>:call C_SplintArguments()<CR>
imap    <buffer>  <silent>  <LocalLeader>rd    <C-C>:call C_Indent()<CR>
imap    <buffer>  <silent>  <LocalLeader>rh    <C-C>:call C_Hardcopy()<CR>
imap    <buffer>  <silent>  <LocalLeader>rs    <C-C>:call C_Settings()<CR>
 if has("unix")
   map    <buffer>  <silent>  <LocalLeader>rx         :call C_XtermSize()<CR>
  imap    <buffer>  <silent>  <LocalLeader>rx    <C-C>:call C_XtermSize()<CR>
 endif
 map    <buffer>  <silent>  <LocalLeader>ro         :call C_Toggle_Gvim_Xterm()<CR>
imap    <buffer>  <silent>  <LocalLeader>ro    <C-C>:call C_Toggle_Gvim_Xterm()<CR>
"
" Abraxas CodeCheck (R)
"
if executable("check") 
  map    <buffer>  <silent>  <LocalLeader>rk         :call C_CodeCheck()<CR>:call C_HlMessage()<CR>
  map    <buffer>  <silent>  <LocalLeader>rka        :call C_CodeCheckArguments()<CR>
 imap    <buffer>  <silent>  <LocalLeader>rk    <C-C>:call C_CodeCheck()<CR>:call C_HlMessage()<CR>
 imap    <buffer>  <silent>  <LocalLeader>rka   <C-C>:call C_CodeCheckArguments()<CR>
endif
" ---------- plugin help -----------------------------------------------------
"
 map    <buffer>  <silent>  <LocalLeader>hp         :call C_HelpCsupport()<CR>
imap    <buffer>  <silent>  <LocalLeader>hp    <C-C>:call C_HelpCsupport()<CR>
 map    <buffer>  <silent>  <LocalLeader>hm         :call C_Help("m")<CR>
imap    <buffer>  <silent>  <LocalLeader>hm    <C-C>:call C_Help("m")<CR>
"
"-------------------------------------------------------------------------------
" additional mapping : complete a classical C comment: '/*' => '/* | */'
"-------------------------------------------------------------------------------
inoremap  <buffer>  /*       /*<Space><Space>*/<Left><Left><Left>
vnoremap  <buffer>  /*      s/*<Space><Space>*/<Left><Left><Left><Esc>p
"
"-------------------------------------------------------------------------------
" additional mapping : complete a classical C multi-line comment: 
"                      '/*<CR>' =>  /*
"                                    * |
"                                    */
"-------------------------------------------------------------------------------
inoremap  <buffer>  /*<CR>  /*<CR><CR>/<Esc>kA<Space>
"
"-------------------------------------------------------------------------------
" additional mapping : {<CR> always opens a block
"-------------------------------------------------------------------------------
inoremap  <buffer>  {<CR>    {<CR>}<Esc>O
vnoremap  <buffer>  {<CR>   S{<CR>}<Esc>Pk=iB
"
"
if !exists("g:C_Ctrl_j") || ( exists("g:C_Ctrl_j") && g:C_Ctrl_j != 'off' )
  nmap    <buffer>  <silent>  <C-j>   i<C-R>=C_JumpCtrlJ()<CR>
  imap    <buffer>  <silent>  <C-j>    <C-R>=C_JumpCtrlJ()<CR>
endif
"
