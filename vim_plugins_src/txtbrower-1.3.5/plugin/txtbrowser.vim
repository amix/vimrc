" txtbrowser.vim:	Utilities to browser plain text file.
" Release:		1.3.5
" Maintainer:		ypguo<guoyoooping@163.com>
" Last modified:	2011.08.07
" License:		GPL.

" ****************** Do not modify after this line ************************

" Line continuation used here
let s:cpo_save = &cpo
set cpo&vim

scriptencoding utf-8

if exists("g:txtbrowser_version")
    finish "stop loading the script
endif
let g:txtbrowser_version = "1.3.4"

"=Options===========================================================
" User defined web dictionary
if !exists('TxtBrowser_Dict_Url')
    let TxtBrowser_Dict_Url = 'http://www.google.com/dictionary?aq=f&langpair=en|zh-CN&q=text&hl=zh-CN'
endif

" User defined Search Engine.
if !exists('Txtbrowser_Search_Engine')
    let Txtbrowser_Search_Engine = 'http://www.google.com/search?hl=zh-CN&source=hp&q=text&btnG=Google+%E6%90%9C%E7%B4%A2&lr=&aq=f&oq='
endif

"===================================================================
"Default map:
if ("" == maparg("<Leader>s", "n"))
    nmap <script> <silent> <unique> <Leader>s <ESC>:TSearch <cword> <CR>
endif
if ("" == maparg("<Leader>s", "v"))
    vmap <script> <silent> <unique> <Leader>s y<ESC>:TSearch <c-r>" <CR>
endif
if ("" == maparg("<Leader>f", "n"))
    nmap <script> <silent> <unique> <Leader>f <ESC>:TFind <cword> <CR>
endif
if ("" == maparg("<Leader>f", "v"))
    vmap <script> <silent> <unique> <Leader>f y<ESC>:TFind <c-r>" <CR>
endif
if ("" == maparg("<Leader>g", "n"))
    nmap <script> <silent> <unique> <Leader>g <ESC>:TGoto <CR>
endif
if ("" == maparg("<Leader>g", "v"))
    vmap <script> <silent> <unique> <Leader>g y<ESC>:TGoto <c-r>" <CR>
endif
if ("" == maparg("<Leader>h", "n"))
    nmap <script> <silent> <unique> <Leader>h yaw<ESC>:TBMatch @\" <CR>
endif
if ("" == maparg("<Leader>h", "v"))
    vmap <script> <silent> <unique> <Leader>h y<ESC>:TBMatch @\" <CR>
endif
if ("" == maparg("*", "v"))
    vnoremap <silent> * y/<C-R>=substitute(escape(@", '.*\\/[]'), "\n", '\\n', 'g')<CR><CR>
endif
if ("" == maparg("#", "v"))
    vnoremap <silent> # y?<C-R>=substitute(escape(@", '.*\\/[]'), "\n", '\\n', 'g')<CR><CR>
endif

"Define the user commands:
command! -nargs=? -bar TSearch call s:TxtBrowserSearch(<f-args>)
command! -nargs=? -bar TFind call s:TxtBrowserWord(<f-args>)
command! -nargs=? -bar TGoto call s:TxtbrowserGoto(<f-args>)
command! -nargs=? -bar TBMatch call s:TBMatch(<args>)
command! -nargs=? -bar TBBold call s:TBBold(<args>)
command! -nargs=? -bar TBItalic call s:TBItalic(<args>)
command! -nargs=? -bar TBRed call s:TBRed(<args>)
command! -nargs=? -bar TBGreen call s:TBGreen(<args>)
command! -nargs=? -bar TBBlue call s:TBBlue(<args>)

" Add the popup menu.
amenu .50 PopUp.-Sep-    :
if v:lang =~ 'zh_CN'
    anoremenu <silent> .51 PopUp.文本浏览工具(&T).打开/关闭目录树(&T) :Tlist <CR>
    anoremenu <silent> .51 PopUp.文本浏览工具(&T).打开/关闭语法高亮(&E) :call <SID>TxtBrowserToggleFt() <CR>
    amenu .50 PopUp.文本浏览工具(&T).-Sep-    :
    nnoremenu <silent> .51 PopUp.文本浏览工具(&T).搜索光标下的单词(&S) :TSearch <cword> <CR>
    inoremenu <silent> .51 PopUp.文本浏览工具(&T).搜索光标下的单词(&S) <ESC>:TSearch <cword> <CR>
    vnoremenu <silent> .51 PopUp.文本浏览工具(&T).搜索选择的单词(&S) y<ESC>:TSearch <c-r>" <CR>
    nnoremenu <silent> .51 PopUp.文本浏览工具(&T).查找光标下的单词(&F) :TFind <cword> <CR>
    inoremenu <silent> .51 PopUp.文本浏览工具(&T).查找光标下的单词(&F) <ESC>:TFind <cword> <CR>
    vnoremenu <silent> .51 PopUp.文本浏览工具(&T).查找选择的单词(&F) y<ESC>:TFind <c-r>" <CR>
    nnoremenu <silent> .51 PopUp.文本浏览工具(&T).打开光标下的URL(&G) :TGoto <CR>
    inoremenu <silent> .51 PopUp.文本浏览工具(&T).打开光标下的URL(&G) <ESC>:TGoto <CR>
    vnoremenu <silent> .51 PopUp.文本浏览工具(&T).打开选择的URL(&G) y<ESC>:TGoto <c-r>" <CR>

    nnoremenu <silent> .51 PopUp.文本浏览工具(&T).高亮搜索光标下的单词(&H) yaw<ESC>:TBMatch @\" <CR>
    inoremenu <silent> .51 PopUp.文本浏览工具(&T).高亮搜索光标下的单词(&H) <ESC>yaw:TBMatch @\" <CR>
    vnoremenu <silent> .51 PopUp.文本浏览工具(&T).高亮选中的文本(&H) y<ESC>:TBMatch @\" <CR>
    nnoremenu <silent> .51 PopUp.文本浏览工具(&T).前向搜索光标下的单词(&N) *
    inoremenu <silent> .51 PopUp.文本浏览工具(&T).前向搜索光标下的单词(&N) <ESC>*
    vnoremenu <silent> .51 PopUp.文本浏览工具(&T).前向搜索选中的文本(&N) y/<C-R>=substitute(escape(@", '.*\\/[]'), "\n", '\\n', 'g')<CR><CR>
    nnoremenu <silent> .51 PopUp.文本浏览工具(&T).后向搜索光标下的单词(&P) #
    inoremenu <silent> .51 PopUp.文本浏览工具(&T).后向搜索光标下的单词(&P) <ESC>#
    vnoremenu <silent> .51 PopUp.文本浏览工具(&T).后向搜索选中的文本(&P) y?<C-R>=substitute(escape(@", '.*\\/[]'), "\n", '\\n', 'g')<CR><CR>

    nnoremenu <silent> .51 PopUp.文本浏览工具(&T).加粗(&B) yaw<ESC>:TBBold @\" <CR>
    inoremenu <silent> .51 PopUp.文本浏览工具(&T).加粗(&B) <ESC>yaw:TBBold @\" <CR>
    vnoremenu <silent> .51 PopUp.文本浏览工具(&T).加粗(&B) y<ESC>:TBBold @\" <CR>

    nnoremenu <silent> .51 PopUp.文本浏览工具(&T).变斜(&I) yaw<ESC>:TBItalic @\" <CR>
    inoremenu <silent> .51 PopUp.文本浏览工具(&T).变斜(&I) <ESC>yaw:TBItalic @\" <CR>
    vnoremenu <silent> .51 PopUp.文本浏览工具(&T).变斜(&I) y<ESC>:TBItalic @\" <CR>

    nnoremenu <silent> .51 PopUp.文本浏览工具(&T).加红(&R) yaw<ESC>:TBRed @\" <CR>
    inoremenu <silent> .51 PopUp.文本浏览工具(&T).加红(&R) <ESC>yaw:TBRed @\" <CR>
    vnoremenu <silent> .51 PopUp.文本浏览工具(&T).加红(&R) y<ESC>:TBRed @\" <CR>

    nnoremenu <silent> .51 PopUp.文本浏览工具(&T).加绿 yaw<ESC>:TBGreen @\" <CR>
    inoremenu <silent> .51 PopUp.文本浏览工具(&T).加绿 <ESC>yaw:TBGreen @\" <CR>
    vnoremenu <silent> .51 PopUp.文本浏览工具(&T).加绿 y<ESC>:TBGreen @\" <CR>

    nnoremenu <silent> .51 PopUp.文本浏览工具(&T).加蓝 yaw<ESC>:TBBlue @\" <CR>
    inoremenu <silent> .51 PopUp.文本浏览工具(&T).加蓝 <ESC>yaw:TBBlue @\" <CR>
    vnoremenu <silent> .51 PopUp.文本浏览工具(&T).加蓝 y<ESC>:TBBlue @\" <CR>

else
    anoremenu <silent> .51 PopUp.TxtBrowser(&T).Tlist(&T) :Tlist <CR>
    anoremenu <silent> .51 PopUp.TxtBrowser(&T).Toggle\ syntax\ highlight(&E) :call <SID>TxtBrowserToggleFt() <CR>
    amenu .50 PopUp.&TxtBrowser(&T).-Sep-    :
    nnoremenu <silent> .51 PopUp.TxtBrowser(&T).Search\ This\ Word(&S) :TSearch <cword> <CR>
    inoremenu <silent> .51 PopUp.TxtBrowser(&T).Search\ This\ Word(&S) <ESC>:TSearch <cword> <CR>
    vnoremenu <silent> .51 PopUp.TxtBrowser(&T).Search\ Word\ Selected(&S) y<ESC>:TSearch <c-r>" <CR>
    nnoremenu <silent> .51 PopUp.TxtBrowser(&T).Find\ This\ Word(&F) :TFind <cword> <CR>
    inoremenu <silent> .51 PopUp.TxtBrowser(&T).Find\ This\ Word(&F) <ESC>:TFind <cword> <CR>
    vnoremenu <silent> .51 PopUp.TxtBrowser(&T).Find\ Word\ Selected(&F) y<ESC>:TFind <c-r>" <CR>
    nnoremenu <silent> .51 PopUp.TxtBrowser(&T).Goto\ This\ Url(&G) :TGoto <CR>
    inoremenu <silent> .51 PopUp.TxtBrowser(&T).Goto\ This\ Url(&G) <ESC>:TGoto <CR>
    vnoremenu <silent> .51 PopUp.TxtBrowser(&T).Goto\ Url\ Selected(&G) y<ESC>:TGoto <c-r>" <CR>
    nnoremenu <silent> .51 PopUp.TxtBrowser(&T).Hightlight\ This\ Word(&H) yaw<ESC>:TBMatch @\" <CR>
    inoremenu <silent> .51 PopUp.TxtBrowser(&T).Hightlight\ This\ Word(&H) <ESC>yaw:TBMatch @\" <CR>
    vnoremenu <silent> .51 PopUp.TxtBrowser(&T).Hightlight\ Text\ Selected(&H) y<ESC>:TBMatch @\" <CR>
    nnoremenu <silent> .51 PopUp.TxtBrowser(&T).*\ Search\ Forward(&N) *
    inoremenu <silent> .51 PopUp.TxtBrowser(&T).*\ Search\ Forward(&N) <ESC>*
    vnoremenu <silent> .51 PopUp.TxtBrowser(&T).*\ Search\ Forward(&N) y/<C-R>=substitute(escape(@", '.*\\/[]'), "\n", '\\n', 'g')<CR><CR>
    nnoremenu <silent> .51 PopUp.TxtBrowser(&T).#\ Search\ Backward(&P) #
    inoremenu <silent> .51 PopUp.TxtBrowser(&T).#\ Search\ Backward(&P) <ESC>#
    vnoremenu <silent> .51 PopUp.TxtBrowser(&T).#\ Search\ Backward(&P) y?<C-R>=substitute(escape(@", '.*\\/[]'), "\n", '\\n', 'g')<CR><CR>

    nnoremenu <silent> .51 PopUp.TxtBrowser(&T).Make\ bold(&B) yaw<ESC>:TBBold @\" <CR>
    inoremenu <silent> .51 PopUp.TxtBrowser(&T).Make\ bold(&B) <ESC>yaw:TBBold @\" <CR>
    vnoremenu <silent> .51 PopUp.TxtBrowser(&T).Make\ bold(&B) y<ESC>:TBBold @\" <CR>
    nnoremenu <silent> .51 PopUp.TxtBrowser(&T).Make\ italic(&I) yaw<ESC>:TBItalic @\" <CR>
    inoremenu <silent> .51 PopUp.TxtBrowser(&T).Make\ italic(&I) <ESC>yaw:TBItalic @\" <CR>
    vnoremenu <silent> .51 PopUp.TxtBrowser(&T).Make\ italic(&I) y<ESC>:TBItalic @\" <CR>
    nnoremenu <silent> .51 PopUp.TxtBrowser(&T).Make\ red(&R) yaw<ESC>:TBRed @\" <CR>
    inoremenu <silent> .51 PopUp.TxtBrowser(&T).Make\ red(&R) <ESC>yaw:TBRed @\" <CR>
    vnoremenu <silent> .51 PopUp.TxtBrowser(&T).Make\ red(&R) y<ESC>:TBRed @\" <CR>
    nnoremenu <silent> .51 PopUp.TxtBrowser(&T).Make\ green yaw<ESC>:TBGreen @\" <CR>
    inoremenu <silent> .51 PopUp.TxtBrowser(&T).Make\ green <ESC>yaw:TBGreen @\" <CR>
    vnoremenu <silent> .51 PopUp.TxtBrowser(&T).Make\ green y<ESC>:TBGreen @\" <CR>
    nnoremenu <silent> .51 PopUp.TxtBrowser(&T).Make\ Blue yaw<ESC>:TBBlue @\" <CR>
    inoremenu <silent> .51 PopUp.TxtBrowser(&T).Make\ Blue <ESC>yaw:TBBlue @\" <CR>
    vnoremenu <silent> .51 PopUp.TxtBrowser(&T).Make\ Blue y<ESC>:TBBlue @\" <CR>

endif

"===================================================================
"The functions:
function! s:TBEscape (text)
	if a:text == ""
		echohl ErrorMsg | echo "No url found in the cursor."
		return -1
	endif
	let tmp = a:text
	let tmp = substitute(tmp, "\n", "\\\\n", 'g')
	let tmp = substitute(tmp, "/", "\\\\/", 'g')
	let tmp = escape(tmp, ".*?[]\"#;%")
	"exec ':match MatchParen ' . '/' . tmp . '/'
	"exec ':syntax match txtRed "' . s:TBEscape(a:text) . '"'
	exec 'hi txtRed term=standout term=standout ctermfg=Red gui=standout guifg=Red'
	return tmp
endfunction

function! s:TBMatch (text)
    exec ':match MatchParen ' . '/' . s:TBEscape(a:text) . '/'
    exec ':syntax match txtBold "' . s:TBEscape(a:text) . '"'
endfunction

function! s:TBBold (text)
    exec ':hi txtBold term=bold cterm=bold ctermfg=black gui=bold guifg=NONE'
    exec ':syntax match txtBold "' . s:TBEscape(a:text) . '"'
endfunction

function! s:TBItalic (text)
    exec ':hi txtItalic term=italic cterm=italic ctermfg=blue gui=italic guifg=NONE'
    exec ':syntax match txtItalic "' . s:TBEscape(a:text) . '"'
endfunction

function! s:TBRed (text)
    exec ':hi txtRed term=standout term=standout ctermfg=Red gui=standout guifg=Red'
    exec ':syntax match txtRed "' . s:TBEscape(a:text) . '"'
endfunction

function! s:TBGreen (text)
    exec ':hi txtGreen term=standout term=standout ctermfg=green gui=standout guifg=green'
    exec ':syntax match txtGreen "' . s:TBEscape(a:text) . '"'
endfunction

function! s:TBBlue (text)
    exec ':hi txtBlue term=standout term=standout ctermfg=Blue gui=standout guifg=Blue'
    exec ':syntax match txtBlue "' . s:TBEscape(a:text) . '"'
endfunction

" Function to parse and get the url in the line gvien.
" @line: input line that need to open.
" return: Url that prased, return "" if not found.
function! s:TxtbrowserGoto(...)
    if a:0 == 0
	let line = getline('.')
    else
	let line = a:1
    endif

    "let url = matchstr(getline("."), '[filehtp]*:\/\/[^>,;]*')
    let url = matchstr(line, "http:\/\/[^ (),:]*")
    if url==""
	let url = matchstr(line, "https:\/\/[^ (),:]*")
    endif
    if url==""
	let url = matchstr(line, "ftp:\/\/[^ ]*")
    endif
    if url==""
	let url = matchstr(line, "file:\/\/[^,;>]*")
    endif
    if url==""
	let url = matchstr(line, "mailto:[^ ]*")
    endif
    if url==""
	let url = matchstr(line, "www\.[^ (),:]*")
    endif
    if url==""
	let url = matchstr(line, "[^,:\> ]*@[^ ,:]*")
	if url!=""
	    let url = "mailto:" . url
	endif
    endif

    "parse local file
    if url==""
	"Get the <cfile>
	let url = expand("<cfile>:.")

	"If the <cfile> is not readable, let isfname += <SPACE> and have a
	"more try.
	if (!filereadable(url))
	    let old_isfname = &isfname
	    set isfname+=32
	    let url = expand("<cfile>:.")
	    " Restore the isfname option
	    let &isfname = old_isfname
	endif

	if (!filereadable(url))
	    let url = ""
	endif
    endif

    let url = escape (url, "\"#;%")
    if url == ""
	echohl ErrorMsg | echo "No url found in the cursor." | echohl Normal
	return -1
    else
	echo "Open url: " . url
    endif

    call s:TxtbrowserOpenUrl(url)

    return url
endfunction

" Function to open the url gvien.
" @url: url that need to open.
function! s:TxtbrowserOpenUrl (url)
    if a:url == ""
	echohl ErrorMsg | echo "No url found in the cursor."
	return -1
    endif

    if exists("g:default_web_browser")
	exec ":silent ! " . g:default_web_browser . " \"" . a:url . "\" &"
    else
	if (has("mac"))
	    exec "!open \"" . a:url . "\""
	elseif (has("win32") || has("win32unix"))
	    exec ':silent !cmd /q /c start "\""dummy title"\"" ' . "\"" . a:url . "\""
	elseif (has("unix"))
	    "exec ':silent !firefox ' . "\"" . a:url . "\" & "
	    exec ":silent !xdg-open \"" . a:url . "\""
	endif
    endif
    exec ":redraw!"
endfunction

" Function to open the url gvien.
" @url: url that need to open.
function! s:TxtBrowserWord (...) range
    if a:0 == 0
	let word = expand('<cword>')
    else
	let word = a:1
    endif

    if word == ""
	echohl ErrorMsg | echo "No text to lookup." | echohl Normal
	return -1
    else
	echo "Find word: " . word
    endif

    call s:TxtbrowserOpenUrl(substitute(g:TxtBrowser_Dict_Url, "text", word, "g"))
endfunction


" Function to open the url gvien.
" @url: url that need to open.
function! s:TxtBrowserSearch (...) range
    if a:0 == 0
	let word = expand('<cword>')
    else
	let word = a:1
    endif

    if word == ""
	echohl ErrorMsg | echo "No text to search." | echohl Normal
	return -1
    else
	echo "Searching: " . word
    endif

    call s:TxtbrowserOpenUrl(substitute(g:Txtbrowser_Search_Engine, "text", word, 'g'))
endfunction

" Function to open the url gvien.
" @url: url that need to open.
function! s:TxtBrowserToggleFt ()
    if &ft =~ "txt"
    set ft=
    else
    set ft=txt
    endif
endfunction

" restore 'cpo'
let &cpo = s:cpo_save
unlet s:cpo_save

" vim:ft=vim sw=4
