" Script Name: mark.vim
" Version:     1.1.8 (global version)
" Last Change: April 25, 2008
" Author:      Yuheng Xie <elephant@linux.net.cn>
" Contributor: Luc Hermitte
"
" Description: a little script to highlight several words in different colors
"              simultaneously
"
" Usage:       :Mark regexp   to mark a regular expression
"              :Mark regexp   with exactly the same regexp to unmark it
"              :Mark          to clear all marks
"
"              You may map keys for the call in your vimrc file for
"              convenience. The default keys is:
"              Highlighting:
"                Normal \m  mark or unmark the word under or before the cursor
"                       \r  manually input a regular expression
"                       \n  clear current mark (i.e. the mark under the cursor),
"                           or clear all marks
"                Visual \m  mark or unmark a visual selection
"                       \r  manually input a regular expression
"              Searching:
"                Normal \*  jump to the next occurrence of current mark
"                       \#  jump to the previous occurrence of current mark
"                       \/  jump to the next occurrence of ANY mark
"                       \?  jump to the previous occurrence of ANY mark
"                        *  behaviors vary, please refer to the table on
"                        #  line 123
"                combined with VIM's / and ? etc.
"
"              The default colors/groups setting is for marking six
"              different words in different colors. You may define your own
"              colors in your vimrc file. That is to define highlight group
"              names as "MarkWordN", where N is a number. An example could be
"              found below.
"
" Bugs:        some colored words could not be highlighted
"
" Changes:
"
" 10th Mar 2006, Yuheng Xie: jump to ANY mark
" (*) added \* \# \/ \? for the ability of jumping to ANY mark, even when the
"     cursor is not currently over any mark
"
" 20th Sep 2005, Yuheng Xie: minor modifications
" (*) merged MarkRegexVisual into MarkRegex
" (*) added GetVisualSelectionEscaped for multi-lines visual selection and
"     visual selection contains ^, $, etc.
" (*) changed the name ThisMark to CurrentMark
" (*) added SearchCurrentMark and re-used raw map (instead of VIM function) to
"     implement * and #
"
" 14th Sep 2005, Luc Hermitte: modifications done on v1.1.4
" (*) anti-reinclusion guards. They do not guard colors definitions in case
"     this script must be reloaded after .gvimrc
" (*) Protection against disabled |line-continuation|s.
" (*) Script-local functions
" (*) Default keybindings
" (*) \r for visual mode
" (*) uses <leader> instead of "\"
" (*) do not mess with global variable g:w
" (*) regex simplified -> double quotes changed into simple quotes.
" (*) strpart(str, idx, 1) -> str[idx]
" (*) command :Mark
"     -> e.g. :Mark Mark.\{-}\ze(

" default colors/groups
" you may define your own colors in you vimrc file, in the form as below:
hi MarkWord1   ctermbg=Cyan        ctermfg=Black  guibg=#ff7f50    guifg=Black
hi MarkWord2   ctermbg=Green       ctermfg=Black  guibg=#A4E57E    guifg=Black
hi MarkWord3   ctermbg=Yellow      ctermfg=Black  guibg=#FFDB72    guifg=Black
hi MarkWord4   ctermbg=Red         ctermfg=Black  guibg=#ff4500    guifg=Black
hi MarkWord5   ctermbg=Magenta     ctermfg=Black  guibg=#ff00ff    guifg=Black
hi MarkWord6   ctermbg=Blue        ctermfg=Black  guibg=#87ceeb    guifg=Black
hi MarkWord7   ctermbg=DarkBlue    ctermfg=Black  guibg=#0000FF    guifg=White
hi MarkWord8   ctermbg=DarkGreen   ctermfg=Black  guibg=#00FF00    guifg=Black
hi MarkWord9   ctermbg=DarkCyan    ctermfg=Black  guibg=#00FFFF    guifg=Black
hi MarkWord10  ctermbg=DarkRed     ctermfg=Black  guibg=#FF0000    guifg=White
hi MarkWord11  ctermbg=DarkMagenta ctermfg=Black  guibg=#FF00FF    guifg=White
hi MarkWord12  ctermbg=Brown       ctermfg=Black  guibg=#8b008b    guifg=White
hi MarkWord13  ctermbg=Grey        ctermfg=Black  guibg=#fafad2    guifg=Black
hi MarkWord14  ctermbg=Green       ctermfg=Black  guibg=#adff2f    guifg=Black

let g:mwCycleMax=14

" Anti reinclusion guards
if exists('g:loaded_mark') && !exists('g:force_reload_mark')
	finish
endif

" Support for |line-continuation|
let s:save_cpo = &cpo
set cpo&vim

" Default bindings

if !hasmapto('<Plug>MarkSet', 'n')
  if (mapcheck('<leader>m', 'n') != "")
    nunmap <leader>m
  endif
	nmap <unique> <silent> <leader>m <Plug>MarkSet
endif
if !hasmapto('<Plug>MarkSet', 'v')
  if (mapcheck('<leader>m', 'v') != "")
    vunmap <leader>m
  endif
	vmap <unique> <silent> <leader>m <Plug>MarkSet
endif
if !hasmapto('<Plug>MarkRegex', 'n')
  if (mapcheck('<leader>r', 'n') != "")
    nunmap <leader>r
  endif
	nmap <unique> <silent> <leader>r <Plug>MarkRegex
endif
if !hasmapto('<Plug>MarkRegex', 'v')
  if (mapcheck('<leader>r', 'v') != "")
    vunmap <leader>r
  endif
	vmap <unique> <silent> <leader>r <Plug>MarkRegex
endif
if !hasmapto('<Plug>MarkClear', 'n')
  if (mapcheck('<leader>n', 'n') != "")
    nunmap <leader>n
  endif
	nmap <unique> <silent> <leader>n <Plug>MarkClear
endif

nnoremap <silent> <Plug>MarkSet   :call
	\ <sid>MarkCurrentWord()<cr>
vnoremap <silent> <Plug>MarkSet   <c-\><c-n>:call
	\ <sid>DoMark(<sid>GetVisualSelectionEscaped("enV"))<cr>
nnoremap <silent> <Plug>MarkRegex :call
	\ <sid>MarkRegex()<cr>
vnoremap <silent> <Plug>MarkRegex <c-\><c-n>:call
	\ <sid>MarkRegex(<sid>GetVisualSelectionEscaped("N"))<cr>
nnoremap <silent> <Plug>MarkClear :call
	\ <sid>DoMark(<sid>CurrentMark())<cr>

" Here is a sumerization of the following keys' behaviors:
" 
" First of all, \#, \? and # behave just like \*, \/ and *, respectively,
" except that \#, \? and # search backward.
"
" \*, \/ and *'s behaviors differ base on whether the cursor is currently
" placed over an active mark:
"
"       Cursor over mark                  Cursor not over mark
" ---------------------------------------------------------------------------
"  \*   jump to the next occurrence of    jump to the next occurrence of
"       current mark, and remember it     "last mark".
"       as "last mark".
"
"  \/   jump to the next occurrence of    same as left
"       ANY mark.
"
"   *   if \* is the most recently used,  do VIM's original *
"       do a \*; otherwise (\/ is the
"       most recently used), do a \/.

nnoremap <silent> <leader>* :call <sid>SearchCurrentMark()<cr>
nnoremap <silent> <leader># :call <sid>SearchCurrentMark("b")<cr>
nnoremap <silent> <leader>/ :call <sid>SearchAnyMark()<cr>
nnoremap <silent> <leader>? :call <sid>SearchAnyMark("b")<cr>
nnoremap <silent> * :if !<sid>SearchNext()<bar>execute "norm! *"<bar>endif<cr>
nnoremap <silent> # :if !<sid>SearchNext("b")<bar>execute "norm! #"<bar>endif<cr>

command! -nargs=? Mark call s:DoMark(<f-args>)

autocmd! BufWinEnter * call s:UpdateMark()

" Functions

function! s:MarkCurrentWord()
	let w = s:PrevWord()
	if w != ""
		call s:DoMark('\<' . w . '\>')
	endif
endfunction

function! s:GetVisualSelection()
	let save_a = @a
	silent normal! gv"ay
	let res = @a
	let @a = save_a
	return res
endfunction

function! s:GetVisualSelectionEscaped(flags)
	" flags:
	"  "e" \  -> \\  
	"  "n" \n -> \\n  for multi-lines visual selection
	"  "N" \n removed
	"  "V" \V added   for marking plain ^, $, etc.
	let result = s:GetVisualSelection()
	let i = 0
	while i < strlen(a:flags)
		if a:flags[i] ==# "e"
			let result = escape(result, '\')
		elseif a:flags[i] ==# "n"
			let result = substitute(result, '\n', '\\n', 'g')
		elseif a:flags[i] ==# "N"
			let result = substitute(result, '\n', '', 'g')
		elseif a:flags[i] ==# "V"
			let result = '\V' . result
		endif
		let i = i + 1
	endwhile
	return result
endfunction

" manually input a regular expression
function! s:MarkRegex(...) " MarkRegex(regexp)
	let regexp = ""
	if a:0 > 0
		let regexp = a:1
	endif
	call inputsave()
	let r = input("@", regexp)
	call inputrestore()
	if r != ""
		call s:DoMark(r)
	endif
endfunction

" define variables if they don't exist
function! s:InitMarkVariables()
	if !exists("g:mwHistAdd")
		let g:mwHistAdd = "/@"
	endif
	if !exists("g:mwCycle")
		let g:mwCycle = 1
	endif
	let i = 1
	while i <= g:mwCycleMax
		if !exists("g:mwWord" . i)
			let g:mwWord{i} = ""
		endif
		let i = i + 1
	endwhile
	if !exists("g:mwLastSearched")
		let g:mwLastSearched = ""
	endif
endfunction

" return the word under or before the cursor
function! s:PrevWord()
	let line = getline(".")
	if line[col(".") - 1] =~ '\w'
		return expand("<cword>")
	else
		return substitute(strpart(line, 0, col(".") - 1), '^.\{-}\(\w\+\)\W*$', '\1', '')
	endif
endfunction

" mark or unmark a regular expression
function! s:DoMark(...) " DoMark(regexp)
	" define variables if they don't exist
	call s:InitMarkVariables()

	" clear all marks if regexp is null
	let regexp = ""
	if a:0 > 0
		let regexp = a:1
	endif
	if regexp == ""
		let i = 1
		while i <= g:mwCycleMax
			if g:mwWord{i} != ""
				let g:mwWord{i} = ""
				let lastwinnr = winnr()
				exe "windo syntax clear MarkWord" . i
				exe lastwinnr . "wincmd w"
			endif
			let i = i + 1
		endwhile
		let g:mwLastSearched = ""
		return 0
	endif

	" clear the mark if it has been marked
	let i = 1
	while i <= g:mwCycleMax
		if regexp == g:mwWord{i}
			if g:mwLastSearched == g:mwWord{i}
				let g:mwLastSearched = ""
			endif
			let g:mwWord{i} = ""
			let lastwinnr = winnr()
			exe "windo syntax clear MarkWord" . i
			exe lastwinnr . "wincmd w"
			return 0
		endif
		let i = i + 1
	endwhile

	" add to history
	if stridx(g:mwHistAdd, "/") >= 0
		call histadd("/", regexp)
	endif
	if stridx(g:mwHistAdd, "@") >= 0
		call histadd("@", regexp)
	endif

	" quote regexp with / etc. e.g. pattern => /pattern/
	let quote = "/?~!@#$%^&*+-=,.:"
	let i = 0
	while i < strlen(quote)
		if stridx(regexp, quote[i]) < 0
			let quoted_regexp = quote[i] . regexp . quote[i]
			break
		endif
		let i = i + 1
	endwhile
	if i >= strlen(quote)
		return -1
	endif

	" choose an unused mark group
	let i = 1
	while i <= g:mwCycleMax
		if g:mwWord{i} == ""
			let g:mwWord{i} = regexp
			if i < g:mwCycleMax
				let g:mwCycle = i + 1
			else
				let g:mwCycle = 1
			endif
			let lastwinnr = winnr()
			exe "windo syntax clear MarkWord" . i
			" suggested by Marc Weber
			" exe "windo syntax match MarkWord" . i . " " . quoted_regexp . " containedin=ALL"
			exe "windo syntax match MarkWord" . i . " " . quoted_regexp . " containedin=.*"
			exe lastwinnr . "wincmd w"
			return i
		endif
		let i = i + 1
	endwhile

	" choose a mark group by cycle
	let i = 1
	while i <= g:mwCycleMax
		if g:mwCycle == i
			if g:mwLastSearched == g:mwWord{i}
				let g:mwLastSearched = ""
			endif
			let g:mwWord{i} = regexp
			if i < g:mwCycleMax
				let g:mwCycle = i + 1
			else
				let g:mwCycle = 1
			endif
			let lastwinnr = winnr()
			exe "windo syntax clear MarkWord" . i
			" suggested by Marc Weber
			" exe "windo syntax match MarkWord" . i . " " . quoted_regexp . " containedin=ALL"
			exe "windo syntax match MarkWord" . i . " " . quoted_regexp . " containedin=.*"
			exe lastwinnr . "wincmd w"
			return i
		endif
		let i = i + 1
	endwhile
endfunction

" update mark colors
function! s:UpdateMark()
	" define variables if they don't exist
	call s:InitMarkVariables()

	let i = 1
	while i <= g:mwCycleMax
		exe "syntax clear MarkWord" . i
		if g:mwWord{i} != ""
			" quote regexp with / etc. e.g. pattern => /pattern/
			let quote = "/?~!@#$%^&*+-=,.:"
			let j = 0
			while j < strlen(quote)
				if stridx(g:mwWord{i}, quote[j]) < 0
					let quoted_regexp = quote[j] . g:mwWord{i} . quote[j]
					break
				endif
				let j = j + 1
			endwhile
			if j >= strlen(quote)
				continue
			endif

			" suggested by Marc Weber
			" exe "syntax match MarkWord" . i . " " . quoted_regexp . " containedin=ALL"
			exe "syntax match MarkWord" . i . " " . quoted_regexp . " containedin=.*"
		endif
		let i = i + 1
	endwhile
endfunction

" return the mark string under the cursor. multi-lines marks not supported
function! s:CurrentMark()
	" define variables if they don't exist
	call s:InitMarkVariables()

	let line = getline(".")
	let i = 1
	while i <= g:mwCycleMax
		if g:mwWord{i} != ""
			let start = 0
			while start >= 0 && start < strlen(line) && start < col(".")
				let b = match(line, g:mwWord{i}, start)
				let e = matchend(line, g:mwWord{i}, start)
				if b < col(".") && col(".") <= e
					let s:current_mark_position = line(".") . "_" . b
					return g:mwWord{i}
				endif
				let start = e
			endwhile
		endif
		let i = i + 1
	endwhile
	return ""
endfunction

" search current mark
function! s:SearchCurrentMark(...) " SearchCurrentMark(flags)
	let flags = ""
	if a:0 > 0
		let flags = a:1
	endif
	let w = s:CurrentMark()
	if w != ""
		let p = s:current_mark_position
		call search(w, flags)
		call s:CurrentMark()
		if p == s:current_mark_position
			call search(w, flags)
		endif
		let g:mwLastSearched = w
	else
		if g:mwLastSearched != ""
			call search(g:mwLastSearched, flags)
		else
			call s:SearchAnyMark(flags)
			let g:mwLastSearched = s:CurrentMark()
		endif
	endif
endfunction

" combine all marks into one regexp
function! s:AnyMark()
	" define variables if they don't exist
	call s:InitMarkVariables()

	let w = ""
	let i = 1
	while i <= g:mwCycleMax
		if g:mwWord{i} != ""
			if w != ""
				let w = w . '\|' . g:mwWord{i}
			else
				let w = g:mwWord{i}
			endif
		endif
		let i = i + 1
	endwhile
	return w
endfunction

" search any mark
function! s:SearchAnyMark(...) " SearchAnyMark(flags)
	let flags = ""
	if a:0 > 0
		let flags = a:1
	endif
	let w = s:CurrentMark()
	if w != ""
		let p = s:current_mark_position
	else
		let p = ""
	endif
	let w = s:AnyMark()
	call search(w, flags)
	call s:CurrentMark()
	if p == s:current_mark_position
		call search(w, flags)
	endif
	let g:mwLastSearched = ""
endfunction

" search last searched mark
function! s:SearchNext(...) " SearchNext(flags)
	let flags = ""
	if a:0 > 0
		let flags = a:1
	endif
	let w = s:CurrentMark()
	if w != ""
		if g:mwLastSearched != ""
			call s:SearchCurrentMark(flags)
		else
			call s:SearchAnyMark(flags)
		endif
		return 1
	else
		return 0
	endif
endfunction

" Restore previous 'cpo' value
let &cpo = s:save_cpo

" vim: ts=2 sw=2
