"        File: imaps.vim
"     Authors: Srinath Avadhanula <srinath AT fastmail.fm>
"              Benji Fisher <benji AT member.AMS.org>
"              
"         WWW: http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/vim-latex/vimfiles/plugin/imaps.vim?only_with_tag=MAIN
"
" Description: insert mode template expander with cursor placement
"              while preserving filetype indentation.
"
"     $Id: imaps.vim,v 1.33.2.1 2003/11/13 09:35:45 srinathava Exp $
"
" Documentation: {{{
"
" Motivation:
" this script provides a way to generate insert mode mappings which do not
" suffer from some of the problem of mappings and abbreviations while allowing
" cursor placement after the expansion. It can alternatively be thought of as
" a template expander. 
"
" Consider an example. If you do
"
" imap lhs something
"
" then a mapping is set up. However, there will be the following problems:
" 1. the 'ttimeout' option will generally limit how easily you can type the
"    lhs. if you type the left hand side too slowly, then the mapping will not
"    be activated.
" 2. if you mistype one of the letters of the lhs, then the mapping is
"    deactivated as soon as you backspace to correct the mistake.
"
" If, in order to take care of the above problems, you do instead
"
" iab lhs something
"
" then the timeout problem is solved and so is the problem of mistyping.
" however, abbreviations are only expanded after typing a non-word character.
" which causes problems of cursor placement after the expansion and invariably
" spurious spaces are inserted.
" 
" Usage Example:
" this script attempts to solve all these problems by providing an emulation
" of imaps wchich does not suffer from its attendant problems. Because maps
" are activated without having to press additional characters, therefore
" cursor placement is possible. furthermore, file-type specific indentation is
" preserved, because the rhs is expanded as if the rhs is typed in literally
" by the user.
"  
" The script already provides some default mappings. each "mapping" is of the
" form:
"
" call IMAP (lhs, rhs, ft)
" 
" Some characters in the RHS have special meaning which help in cursor
" placement.
"
" Example One:
"
" 	call IMAP ("bit`", "\\begin{itemize}\<cr>\\item <++>\<cr>\\end{itemize}<++>", "tex")
" 
" This effectively sets up the map for "bit`" whenever you edit a latex file.
" When you type in this sequence of letters, the following text is inserted:
" 
" \begin{itemize}
" \item *
" \end{itemize}<++>
"
" where * shows the cursor position. The cursor position after inserting the
" text is decided by the position of the first "place-holder". Place holders
" are special characters which decide cursor placement and movement. In the
" example above, the place holder characters are <+ and +>. After you have typed
" in the item, press <C-j> and you will be taken to the next set of <++>'s.
" Therefore by placing the <++> characters appropriately, you can minimize the
" use of movement keys.
"
" NOTE: Set g:Imap_UsePlaceHolders to 0 to disable placeholders altogether.
" Set 
" 	g:Imap_PlaceHolderStart and g:Imap_PlaceHolderEnd
" to something else if you want different place holder characters.
" Also, b:Imap_PlaceHolderStart and b:Imap_PlaceHolderEnd override the values
" of g:Imap_PlaceHolderStart and g:Imap_PlaceHolderEnd respectively. This is
" useful for setting buffer specific place hoders.
" 
" Example Two:
" You can use the <C-r> command to insert dynamic elements such as dates.
"	call IMAP ('date`', "\<c-r>=strftime('%b %d %Y')\<cr>", '')
"
" sets up the map for date` to insert the current date.
"
"--------------------------------------%<--------------------------------------
" Bonus: This script also provides a command Snip which puts tearoff strings,
" '----%<----' above and below the visually selected range of lines. The
" length of the string is chosen to be equal to the longest line in the range.
" Recommended Usage:
"   '<,'>Snip
"--------------------------------------%<--------------------------------------
" }}}

" ==============================================================================
" Script Options / Variables
" ============================================================================== 
" Options {{{
if !exists('g:Imap_StickyPlaceHolders')
	let g:Imap_StickyPlaceHolders = 1
endif
if !exists('g:Imap_DeleteEmptyPlaceHolders')
	let g:Imap_DeleteEmptyPlaceHolders = 1
endif
" }}}
" Variables {{{
" s:LHS_{ft}_{char} will be generated automatically.  It will look like
" s:LHS_tex_o = 'fo\|foo\|boo' and contain all mapped sequences ending in "o".
" s:Map_{ft}_{lhs} will be generated automatically.  It will look like
" s:Map_c_foo = 'for(<++>; <++>; <++>)', the mapping for "foo".
"
" }}}

" ==============================================================================
" functions for easy insert mode mappings.
" ==============================================================================
" IMAP: Adds a "fake" insert mode mapping. {{{
"       For example, doing
"           IMAP('abc', 'def' ft) 
"       will mean that if the letters abc are pressed in insert mode, then
"       they will be replaced by def. If ft != '', then the "mapping" will be
"       specific to the files of type ft. 
"
"       Using IMAP has a few advantages over simply doing:
"           imap abc def
"       1. with imap, if you begin typing abc, the cursor will not advance and
"          long as there is a possible completion, the letters a, b, c will be
"          displayed on on top of the other. using this function avoids that.
"       2. with imap, if a backspace or arrow key is pressed before completing
"          the word, then the mapping is lost. this function allows movement. 
"          (this ofcourse means that this function is only limited to
"          left-hand-sides which do not have movement keys or unprintable
"          characters)
"       It works by only mapping the last character of the left-hand side.
"       when this character is typed in, then a reverse lookup is done and if
"       the previous characters consititute the left hand side of the mapping,
"       the previously typed characters and erased and the right hand side is
"       inserted

" IMAP: set up a filetype specific mapping.
" Description:
"   "maps" the lhs to rhs in files of type 'ft'. If supplied with 2
"   additional arguments, then those are assumed to be the placeholder
"   characters in rhs. If unspecified, then the placeholder characters
"   are assumed to be '<+' and '+>' These placeholder characters in
"   a:rhs are replaced with the users setting of
"   [bg]:Imap_PlaceHolderStart and [bg]:Imap_PlaceHolderEnd settings.
"
function! IMAP(lhs, rhs, ft, ...)

	" Find the place holders to save for IMAP_PutTextWithMovement() .
	if a:0 < 2
		let phs = '<+'
		let phe = '+>'
	else
		let phs = a:1
		let phe = a:2
	endif

	let hash = s:Hash(a:lhs)
	let s:Map_{a:ft}_{hash} = a:rhs
	let s:phs_{a:ft}_{hash} = phs
	let s:phe_{a:ft}_{hash} = phe

	" Add a:lhs to the list of left-hand sides that end with lastLHSChar:
	let lastLHSChar = a:lhs[strlen(a:lhs)-1]
	let hash = s:Hash(lastLHSChar)
	if !exists("s:LHS_" . a:ft . "_" . hash)
		let s:LHS_{a:ft}_{hash} = escape(a:lhs, '\')
	else
		let s:LHS_{a:ft}_{hash} = escape(a:lhs, '\') .'\|'.  s:LHS_{a:ft}_{hash}
	endif

	" map only the last character of the left-hand side.
	if lastLHSChar == ' '
		let lastLHSChar = '<space>'
	end
	exe 'inoremap <silent>'
				\ escape(lastLHSChar, '|')
				\ '<C-r>=<SID>LookupCharacter("' .
				\ escape(lastLHSChar, '\|"') .
				\ '")<CR>'
endfunction

" }}}
" IMAP_list:  list the rhs and place holders corresponding to a:lhs {{{
"
" Added mainly for debugging purposes, but maybe worth keeping.
function! IMAP_list(lhs)
	let char = a:lhs[strlen(a:lhs)-1]
	let charHash = s:Hash(char)
	if exists("s:LHS_" . &ft ."_". charHash) && a:lhs =~ s:LHS_{&ft}_{charHash}
		let ft = &ft
	elseif exists("s:LHS__" . charHash) && a:lhs =~ s:LHS__{charHash}
		let ft = ""
	else
		return ""
	endif
	let hash = s:Hash(a:lhs)
	return "rhs = " . s:Map_{ft}_{hash} . " place holders = " .
				\ s:phs_{ft}_{hash} . " and " . s:phe_{ft}_{hash}
endfunction
" }}}
" LookupCharacter: inserts mapping corresponding to this character {{{
"
" This function extracts from s:LHS_{&ft}_{a:char} or s:LHS__{a:char}
" the longest lhs matching the current text.  Then it replaces lhs with the
" corresponding rhs saved in s:Map_{ft}_{lhs} .
" The place-holder variables are passed to IMAP_PutTextWithMovement() .
function! s:LookupCharacter(char)
	let charHash = s:Hash(a:char)

	" The line so far, including the character that triggered this function:
	let text = strpart(getline("."), 0, col(".")-1) . a:char
	" Prefer a local map to a global one, even if the local map is shorter.
	" Is this what we want?  Do we care?
	" Use '\V' (very no-magic) so that only '\' is special, and it was already
	" escaped when building up s:LHS_{&ft}_{charHash} .
	if exists("s:LHS_" . &ft . "_" . charHash)
				\ && text =~ '\V\(' . s:LHS_{&ft}_{charHash} . '\)\$'
		let ft = &ft
	elseif exists("s:LHS__" . charHash)
				\ && text =~ '\V\(' . s:LHS__{charHash} . '\)\$'
		let ft = ""
	else
		" If this is a character which could have been used to trigger an
		" abbreviation, check if an abbreviation exists.
		if a:char !~ '\k'
			let lastword = matchstr(getline('.'), '\k\+$', '')
			if lastword != ''
				" An extremeley wierd way to get around the fact that vim
				" doesn't have the equivalent of the :mapcheck() function for
				" abbreviations.
				let _a = @a
				exec "redir @a | silent! iab ".lastword." | redir END"
				let abbreviationRHS = matchstr(@a."\n", "\n".'i\s\+'.lastword.'\+\s\+@\?\zs.*\ze'."\n")

				if @a =~ "No abbreviation found" || abbreviationRHS == ""
					let @a = _a
					return a:char
				endif

				let @a = _a
				let abbreviationRHS = escape(abbreviationRHS, '\<"')
				exec 'let abbreviationRHS = "'.abbreviationRHS.'"'

				let lhs = lastword.a:char
				let rhs = abbreviationRHS.a:char
				let phs = IMAP_GetPlaceHolderStart()
				let phe = IMAP_GetPlaceHolderEnd()
			else
				return a:char
			endif
		else
			return a:char
		endif
	endif
	" Find the longest left-hand side that matches the line so far.
	" matchstr() returns the match that starts first. This automatically
	" ensures that the longest LHS is used for the mapping.
	if !exists('lhs') || !exists('rhs')
		let lhs = matchstr(text, '\V\(' . s:LHS_{ft}_{charHash} . '\)\$')
		let hash = s:Hash(lhs)
		let rhs = s:Map_{ft}_{hash}
		let phs = s:phs_{ft}_{hash} 
		let phe = s:phe_{ft}_{hash}
	endif

	if strlen(lhs) == 0
		return a:char
	endif
	" enough back-spaces to erase the left-hand side; -1 for the last
	" character typed:
	let bs = substitute(strpart(lhs, 1), ".", "\<bs>", "g")
	return bs . IMAP_PutTextWithMovement(rhs, phs, phe)
endfunction

" }}}
" IMAP_PutTextWithMovement: returns the string with movement appended {{{
" Description:
"   If a:str contains "placeholders", then appends movement commands to
"   str in a way that the user moves to the first placeholder and enters
"   insert or select mode. If supplied with 2 additional arguments, then
"   they are assumed to be the placeholder specs. Otherwise, they are
"   assumed to be '<+' and '+>'. These placeholder chars are replaced
"   with the users settings of [bg]:Imap_PlaceHolderStart and
"   [bg]:Imap_PlaceHolderEnd.
function! IMAP_PutTextWithMovement(str, ...)

	" The placeholders used in the particular input string. These can be
	" different from what the user wants to use.
	if a:0 < 2
		let phs = '<+'
		let phe = '+>'
	else
		let phs = escape(a:1, '\')
		let phe = escape(a:2, '\')
	endif

	let text = a:str

	" The user's placeholder settings.
	let phsUser = IMAP_GetPlaceHolderStart()
	let pheUser = IMAP_GetPlaceHolderEnd()

	" Problem:  depending on the setting of the 'encoding' option, a character
	" such as "\xab" may not match itself.  We try to get around this by
	" changing the encoding of all our strings.  At the end, we have to
	" convert text back.
	let phsEnc     = s:Iconv(phs, "encode")
	let pheEnc     = s:Iconv(phe, "encode")
	let phsUserEnc = s:Iconv(phsUser, "encode")
	let pheUserEnc = s:Iconv(pheUser, "encode")
	let textEnc    = s:Iconv(text, "encode")
	if textEnc != text
		let textEncoded = 1
	else
		let textEncoded = 0
	endif

	let pattern = '\V\(\.\{-}\)' .phs. '\(\.\{-}\)' .phe. '\(\.\*\)'
	" If there are no placeholders, just return the text.
	if textEnc !~ pattern
		call IMAP_Debug('Not getting '.phs.' and '.phe.' in '.textEnc, 'imap')
		return text
	endif
	" Break text up into "initial <+template+> final"; any piece may be empty.
	let initialEnc  = substitute(textEnc, pattern, '\1', '')
	let templateEnc = substitute(textEnc, pattern, '\2', '')
	let finalEnc    = substitute(textEnc, pattern, '\3', '')

	" If the user does not want to use placeholders, then remove all but the
	" first placeholder.
	" Otherwise, replace all occurences of the placeholders here with the
	" user's choice of placeholder settings.
	if exists('g:Imap_UsePlaceHolders') && !g:Imap_UsePlaceHolders
		let finalEnc = substitute(finalEnc, '\V'.phs.'\.\{-}'.phe, '', 'g')
	else
		let finalEnc = substitute(finalEnc, '\V'.phs.'\(\.\{-}\)'.phe,
					\ phsUserEnc.'\1'.pheUserEnc, 'g')
	endif

	" The substitutions are done, so convert back, if necessary.
	if textEncoded
		let initial = s:Iconv(initialEnc, "decode")
		let template = s:Iconv(templateEnc, "decode")
		let final = s:Iconv(finalEnc, "decode")
	else
		let initial = initialEnc
		let template = templateEnc
		let final = finalEnc
	endif

	" Build up the text to insert:
	" 1. the initial text plus an extra character;
	" 2. go to Normal mode with <C-\><C-N>, so it works even if 'insertmode'
	" is set, and mark the position;
	" 3. replace the extra character with tamplate and final;
	" 4. back to Normal mode and restore the cursor position;
	" 5. call IMAP_Jumpfunc().
	let template = phsUser . template . pheUser
	" Old trick:  insert and delete a character to get the same behavior at
	" start, middle, or end of line and on empty lines.
	let text = initial . "X\<C-\>\<C-N>:call IMAP_Mark('set')\<CR>\"_s"
	let text = text . template . final
	let text = text . "\<C-\>\<C-N>:call IMAP_Mark('go')\<CR>"
	let text = text . "i\<C-r>=IMAP_Jumpfunc('', 1)\<CR>"

	return text
endfunction

" }}}
" IMAP_Jumpfunc: takes user to next <+place-holder+> {{{
" Author: Luc Hermitte
" Arguments:
" direction: flag for the search() function. If set to '', search forwards,
"            if 'b', then search backwards. See the {flags} argument of the
"            |search()| function for valid values.
" inclusive: In vim, the search() function is 'exclusive', i.e we always goto
"            next cursor match even if there is a match starting from the
"            current cursor position. Setting this argument to 1 makes
"            IMAP_Jumpfunc() also respect a match at the current cursor
"            position. 'inclusive'ness is necessary for IMAP() because a
"            placeholder string can occur at the very beginning of a map which
"            we want to select.
"            We use a non-zero value only in special conditions. Most mappings
"            should use a zero value.
function! IMAP_Jumpfunc(direction, inclusive)

	" The user's placeholder settings.
	let phsUser = IMAP_GetPlaceHolderStart()
	let pheUser = IMAP_GetPlaceHolderEnd()

	let searchString = ''
	" If this is not an inclusive search or if it is inclusive, but the
	" current cursor position does not contain a placeholder character, then
	" search for the placeholder characters.
	if !a:inclusive || strpart(getline('.'), col('.')-1) !~ '\V\^'.phsUser
		let searchString = '\V'.phsUser.'\_.\{-}'.pheUser
	endif

	" If we didn't find any placeholders return quietly.
	if searchString != '' && !search(searchString, a:direction)
		return ''
	endif

	" Open any closed folds and make this part of the text visible.
	silent! foldopen!

	" Calculate if we have an empty placeholder or if it contains some
	" description.
	let template = 
		\ matchstr(strpart(getline('.'), col('.')-1),
		\          '\V\^'.phsUser.'\zs\.\{-}\ze\('.pheUser.'\|\$\)')
	let placeHolderEmpty = !strlen(template)

	" If we are selecting in exclusive mode, then we need to move one step to
	" the right
	let extramove = ''
	if &selection == 'exclusive'
		let extramove = 'l'
	endif

	" Select till the end placeholder character.
	let movement = "\<C-o>v/\\V".pheUser."/e\<CR>".extramove

	" First remember what the search pattern was. s:RemoveLastHistoryItem will
	" reset @/ to this pattern so we do not create new highlighting.
	let g:Tex_LastSearchPattern = @/

	" Now either goto insert mode or select mode.
	if placeHolderEmpty && g:Imap_DeleteEmptyPlaceHolders
		" delete the empty placeholder into the blackhole.
		return movement."\"_c\<C-o>:".s:RemoveLastHistoryItem."\<CR>"
	else
		return movement."\<C-\>\<C-N>:".s:RemoveLastHistoryItem."\<CR>gv\<C-g>"
	endif
	
endfunction

" }}}
" Maps for IMAP_Jumpfunc {{{
"
" These mappings use <Plug> and thus provide for easy user customization. When
" the user wants to map some other key to jump forward, he can do for
" instance:
"   nmap ,f   <plug>IMAP_JumpForward
" etc.

" jumping forward and back in insert mode.
imap <silent> <Plug>IMAP_JumpForward    <c-r>=IMAP_Jumpfunc('', 0)<CR>
imap <silent> <Plug>IMAP_JumpBack       <c-r>=IMAP_Jumpfunc('b', 0)<CR>

" jumping in normal mode
nmap <silent> <Plug>IMAP_JumpForward        i<c-r>=IMAP_Jumpfunc('', 0)<CR>
nmap <silent> <Plug>IMAP_JumpBack           i<c-r>=IMAP_Jumpfunc('b', 0)<CR>

" deleting the present selection and then jumping forward.
vmap <silent> <Plug>IMAP_DeleteAndJumpForward       "_<Del>i<c-r>=IMAP_Jumpfunc('', 0)<CR>
vmap <silent> <Plug>IMAP_DeleteAndJumpBack          "_<Del>i<c-r>=IMAP_Jumpfunc('b', 0)<CR>

" jumping forward without deleting present selection.
vmap <silent> <Plug>IMAP_JumpForward       <C-\><C-N>i<c-r>=IMAP_Jumpfunc('', 0)<CR>
vmap <silent> <Plug>IMAP_JumpBack          <C-\><C-N>`<i<c-r>=IMAP_Jumpfunc('b', 0)<CR>

" }}}
" Default maps for IMAP_Jumpfunc {{{
" map only if there is no mapping already. allows for user customization.
" NOTE: Default mappings for jumping to the previous placeholder are not
"       provided. It is assumed that if the user will create such mappings
"       hself if e so desires.
if !hasmapto('<Plug>IMAP_JumpForward', 'i')
    imap <C-J> <Plug>IMAP_JumpForward
endif
if !hasmapto('<Plug>IMAP_JumpForward', 'n')
    nmap <C-J> <Plug>IMAP_JumpForward
endif
if exists('g:Imap_StickyPlaceHolders') && g:Imap_StickyPlaceHolders
	if !hasmapto('<Plug>IMAP_JumpForward', 'v')
		vmap <C-J> <Plug>IMAP_JumpForward
	endif
else
	if !hasmapto('<Plug>IMAP_DeleteAndJumpForward', 'v')
		vmap <C-J> <Plug>IMAP_DeleteAndJumpForward
	endif
endif
" }}}

nmap <silent> <script> <plug><+SelectRegion+> `<v`>

" ============================================================================== 
" enclosing selected region.
" ============================================================================== 
" VEnclose: encloses the visually selected region with given arguments {{{
" Description: allows for differing action based on visual line wise
"              selection or visual characterwise selection. preserves the
"              marks and search history.
function! VEnclose(vstart, vend, VStart, VEnd)

	" its characterwise if
	" 1. characterwise selection and valid values for vstart and vend.
	" OR
	" 2. linewise selection and invalid values for VStart and VEnd
	if (visualmode() == 'v' && (a:vstart != '' || a:vend != '')) || (a:VStart == '' && a:VEnd == '')

		let newline = ""
		let _r = @r

		let normcmd = "normal! \<C-\>\<C-n>`<v`>\"_s"

		exe "normal! \<C-\>\<C-n>`<v`>\"ry"
		if @r =~ "\n$"
			let newline = "\n"
			let @r = substitute(@r, "\n$", '', '')
		endif

		" In exclusive selection, we need to select an extra character.
		if &selection == 'exclusive'
			let movement = 8
		else
			let movement = 7
		endif
		let normcmd = normcmd.
			\ a:vstart."!!mark!!".a:vend.newline.
			\ "\<C-\>\<C-N>?!!mark!!\<CR>v".movement."l\"_s\<C-r>r\<C-\>\<C-n>"

		" this little if statement is because till very recently, vim used to
		" report col("'>") > length of selected line when `> is $. on some
		" systems it reports a -ve number.
		if col("'>") < 0 || col("'>") > strlen(getline("'>"))
			let lastcol = strlen(getline("'>"))
		else
			let lastcol = col("'>")
		endif
		if lastcol - col("'<") != 0
			let len = lastcol - col("'<")
		else
			let len = ''
		endif

		" the next normal! is for restoring the marks.
		let normcmd = normcmd."`<v".len."l\<C-\>\<C-N>"

		" First remember what the search pattern was. s:RemoveLastHistoryItem
		" will reset @/ to this pattern so we do not create new highlighting.
		let g:Tex_LastSearchPattern = @/

		silent! exe normcmd
		" this is to restore the r register.
		let @r = _r
		" and finally, this is to restore the search history.
		execute s:RemoveLastHistoryItem

	else

		exec 'normal! `<O'.a:VStart."\<C-\>\<C-n>"
		exec 'normal! `>o'.a:VEnd."\<C-\>\<C-n>"
		if &indentexpr != ''
			silent! normal! `<kV`>j=
		endif
		silent! normal! `>
	endif
endfunction 

" }}}
" ExecMap: adds the ability to correct an normal/visual mode mapping.  {{{
" Author: Hari Krishna Dara <hari_vim@yahoo.com>
" Reads a normal mode mapping at the command line and executes it with the
" given prefix. Press <BS> to correct and <Esc> to cancel.
function! ExecMap(prefix, mode)
	" Temporarily remove the mapping, otherwise it will interfere with the
	" mapcheck call below:
	let myMap = maparg(a:prefix, a:mode)
	exec a:mode."unmap ".a:prefix

	" Generate a line with spaces to clear the previous message.
	let i = 1
	let clearLine = "\r"
	while i < &columns
		let clearLine = clearLine . ' '
		let i = i + 1
	endwhile

	let mapCmd = a:prefix
	let foundMap = 0
	let breakLoop = 0
	echon "\rEnter Map: " . mapCmd
	while !breakLoop
		let char = getchar()
		if char !~ '^\d\+$'
			if char == "\<BS>"
				let mapCmd = strpart(mapCmd, 0, strlen(mapCmd) - 1)
			endif
		else " It is the ascii code.
			let char = nr2char(char)
			if char == "\<Esc>"
				let breakLoop = 1
			else
				let mapCmd = mapCmd . char
				if maparg(mapCmd, a:mode) != ""
					let foundMap = 1
					let breakLoop = 1
				elseif mapcheck(mapCmd, a:mode) == ""
					let mapCmd = strpart(mapCmd, 0, strlen(mapCmd) - 1)
				endif
			endif
		endif
		echon clearLine
		echon "\rEnter Map: " . mapCmd
	endwhile
	if foundMap
		if a:mode == 'v'
			" use a plug to select the region instead of using something like
			" `<v`> to avoid problems caused by some of the characters in
			" '`<v`>' being mapped.
			let gotoc = "\<plug><+SelectRegion+>"
		else
			let gotoc = ''
		endif
		exec "normal ".gotoc.mapCmd
	endif
	exec a:mode.'noremap '.a:prefix.' '.myMap
endfunction

" }}}

" ============================================================================== 
" helper functions
" ============================================================================== 
" Strntok: extract the n^th token from a list {{{
" example: Strntok('1,23,3', ',', 2) = 23
fun! <SID>Strntok(s, tok, n)
	return matchstr( a:s.a:tok[0], '\v(\zs([^'.a:tok.']*)\ze['.a:tok.']){'.a:n.'}')
endfun

" }}}
" s:RemoveLastHistoryItem: removes last search item from search history {{{
" Description: Execute this string to clean up the search history.
let s:RemoveLastHistoryItem = ':call histdel("/", -1)|let @/=g:Tex_LastSearchPattern'

" }}}
" s:Hash: Return a version of a string that can be used as part of a variable" {{{
" name.
" 	Converts every non alphanumeric character into _{ascii}_ where {ascii} is
" 	the ASCII code for that character...
fun! s:Hash(text)
	return substitute(a:text, '\([^[:alnum:]]\)',
				\ '\="_".char2nr(submatch(1))."_"', 'g')
endfun
"" }}}
" IMAP_GetPlaceHolderStart and IMAP_GetPlaceHolderEnd:  "{{{
" return the buffer local placeholder variables, or the global one, or the default.
function! IMAP_GetPlaceHolderStart()
	if exists("b:Imap_PlaceHolderStart") && strlen(b:Imap_PlaceHolderEnd)
		return b:Imap_PlaceHolderStart
	elseif exists("g:Imap_PlaceHolderStart") && strlen(g:Imap_PlaceHolderEnd)
		return g:Imap_PlaceHolderStart
	else
		return "<+"
endfun
function! IMAP_GetPlaceHolderEnd()
	if exists("b:Imap_PlaceHolderEnd") && strlen(b:Imap_PlaceHolderEnd)
		return b:Imap_PlaceHolderEnd
	elseif exists("g:Imap_PlaceHolderEnd") && strlen(g:Imap_PlaceHolderEnd)
		return g:Imap_PlaceHolderEnd
	else
		return "+>"
endfun
" }}}
" s:Iconv:  a wrapper for iconv()" {{{
" Problem:  after
" 	let text = "\xab"
" (or using the raw 8-bit ASCII character in a file with 'fenc' set to
" "latin1") if 'encoding' is set to utf-8, then text does not match itself:
" 	echo text =~ text
" returns 0.
" Solution:  When this happens, a re-encoded version of text does match text:
" 	echo iconv(text, "latin1", "utf8") =~ text
" returns 1.  In this case, convert text to utf-8 with iconv().
" TODO:  Is it better to use &encoding instead of "utf8"?  Internally, vim
" uses utf-8, and can convert between latin1 and utf-8 even when compiled with
" -iconv, so let's try using utf-8.
" Arguments:
" 	a:text = text to be encoded or decoded
" 	a:mode = "encode" (latin1 to utf8) or "decode" (utf8 to latin1)
" Caution:  do not encode and then decode without checking whether the text
" has changed, becuase of the :if clause in encoding!
function! s:Iconv(text, mode)
	if a:mode == "decode"
		return iconv(a:text, "utf8", "latin1")
	endif
	if a:text =~ '\V\^' . escape(a:text, '\') . '\$'
		return a:text
	endif
	let textEnc = iconv(a:text, "latin1", "utf8")
	if textEnc !~ '\V\^' . escape(a:text, '\') . '\$''
		call IMAP_Debug('Encoding problems with text '.a:text.' ', 'imap')
	endif
	return textEnc
endfun
"" }}}
" IMAP_Debug: interface to Tex_Debug if available, otherwise emulate it {{{
" Description: 
" Do not want a memory leak! Set this to zero so that imaps always
" starts out in a non-debugging mode.
if !exists('g:Imap_Debug')
	let g:Imap_Debug = 0
endif
function! IMAP_Debug(string, pattern)
	if !g:Imap_Debug
		return
	endif
	if exists('*Tex_Debug')
		call Tex_Debug(a:string, a:pattern)
	else
		if !exists('s:debug_'.a:pattern)
			let s:debug_{a:pattern} = a:string
		else
			let s:debug_{a:pattern} = s:debug_{a:pattern}.a:string
		endif
	endif
endfunction " }}}
" IMAP_DebugClear: interface to Tex_DebugClear if avaialable, otherwise emulate it {{{
" Description: 
function! IMAP_DebugClear(pattern)
	if exists('*Tex_DebugClear')
		call Tex_DebugClear(a:pattern)
	else	
		let s:debug_{a:pattern} = ''
	endif
endfunction " }}}
" IMAP_DebugPrint: interface to Tex_DebugPrint if avaialable, otherwise emulate it {{{
" Description: 
function! IMAP_DebugPrint(pattern)
	if exists('*Tex_DebugPrint')
		call Tex_DebugPrint(a:pattern)
	else
		if exists('s:debug_'.a:pattern)
			let s:debug_{a:pattern} = ''
		else
			echo s:debug_{a:pattern}
		endif
	endif
endfunction " }}}
" IMAP_Mark:  Save the cursor position (if a:action == 'set') in a" {{{
" script-local variable; restore this position if a:action == 'go'.
let s:Mark = "(0,0)"
function! IMAP_Mark(action)
	if a:action == 'set'
		let s:Mark = "(" . line(".") . "," . col(".") . ")"
	elseif a:action == 'go'
		execute "call cursor" s:Mark
	endif
endfunction	"" }}}

" ============================================================================== 
" A bonus function: Snip()
" ============================================================================== 
" Snip: puts a scissor string above and below block of text {{{
" Desciption:
"-------------------------------------%<-------------------------------------
"   this puts a the string "--------%<---------" above and below the visually
"   selected block of lines. the length of the 'tearoff' string depends on the
"   maximum string length in the selected range. this is an aesthetically more
"   pleasing alternative instead of hardcoding a length.
"-------------------------------------%<-------------------------------------
function! <SID>Snip() range
	let i = a:firstline
	let maxlen = -2
	" find out the maximum virtual length of each line.
	while i <= a:lastline
		exe i
		let length = virtcol('$')
		let maxlen = (length > maxlen ? length : maxlen)
		let i = i + 1
	endwhile
	let maxlen = (maxlen > &tw && &tw != 0 ? &tw : maxlen)
	let half = maxlen/2
	exe a:lastline
	" put a string below
	exe "norm! o\<esc>".(half - 1)."a-\<esc>A%<\<esc>".(half - 1)."a-"
	" and above. its necessary to put the string below the block of lines
	" first because that way the first line number doesnt change...
	exe a:firstline
	exe "norm! O\<esc>".(half - 1)."a-\<esc>A%<\<esc>".(half - 1)."a-"
endfunction

com! -nargs=0 -range Snip :<line1>,<line2>call <SID>Snip()
" }}}

" vim:ft=vim:ts=4:sw=4:noet:fdm=marker:commentstring=\"\ %s:nowrap
