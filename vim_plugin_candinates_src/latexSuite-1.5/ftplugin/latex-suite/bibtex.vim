"=============================================================================
" 	       File: bibtex.vim
"      Function: BibT
"        Author: Alan G Isaac <aisaac@american.edu>
"                modified by Srinath Avadhanula <srinath AT fastmail DOT fm>
"                for latex-suite.
"       License: Vim Charityware license.
"           CVS: $Id: bibtex.vim,v 1.5 2003/06/15 08:23:14 srinathava Exp $
"=============================================================================

" Fields:
" Define what field type each letter denotes {{{
" 
let s:w_standsfor = 'address'
let s:a_standsfor = 'author'
let s:b_standsfor = 'booktitle'
let s:c_standsfor = 'chapter'
let s:d_standsfor = 'edition'
let s:e_standsfor = 'editor'
let s:h_standsfor = 'howpublished'
let s:i_standsfor = 'institution'
let s:k_standsfor = 'isbn'
let s:j_standsfor = 'journal'
let s:m_standsfor = 'month'
let s:n_standsfor = 'number'
let s:o_standsfor = 'organization'
let s:p_standsfor = 'pages'
let s:q_standsfor = 'publisher'
let s:r_standsfor = 'school'
let s:s_standsfor = 'series'
let s:t_standsfor = 'title'
let s:u_standsfor = 'type'
let s:v_standsfor = 'volume'
let s:y_standsfor = 'year'
let s:z_standsfor = 'note'

" }}}
" Define the fields required for the various entry types {{{
" 
" s:{type}_required defines the required fields
" s:{type}_optional1 defines common optional fields
" s:{type}_optional2 defines uncommmon optional fields
" s:{type}_retval defines the first line of the formatted bib entry.
"
let s:key='<+key+>'

let s:{'article'}_required="atjy"
let s:{'article'}_optional1="vnpm"
let s:{'article'}_optional2="z" " z is note
let s:{'article'}_retval = '@ARTICLE{' . s:key . ','."\n"

let s:{'book'}_required="aetqy" " requires author *or* editor
let s:{'book'}_optional1="wd"
let s:{'book'}_optional2="vnsmz" " w is address, d is edition
let s:{'book'}_extras="k" " isbn
let s:{'book'}_retval = '@BOOK{' . s:key . ','."\n"

let s:{'booklet'}_required="t"
let s:{'booklet'}_optional1="ahy"
let s:{'booklet'}_optional2="wmz" " w is address
let s:{'booklet'}_retval = '@BOOKLET{' . s:key . ','."\n"

let s:{'inbook'}_required="aetcpqy"
let s:{'inbook'}_optional1="w" " w is address
let s:{'inbook'}_optional2="vnsudmz" " d is edition
let s:{'inbook'}_extras="k" " isbn
let s:{'inbook'}_retval = '@INBOOK{' . s:key . ','."\n"

let s:{'incollection'}_required="atbqy" " b is booktitle
let s:{'incollection'}_optional1="cpw" " w is address, c is chapter
let s:{'incollection'}_optional2="evnsudmz" " d is edition
let s:{'incollection'}_extras="k" " isbn
let s:{'incollection'}_retval = '@INCOLLECTION{' . s:key . ','."\n"

let s:{'inproceedings'}_required="atby" " b is booktitle
let s:{'inproceedings'}_optional1="epwoq" " w is address, q is publisher
let s:{'inproceedings'}_optional2="vnsmz"
let s:{'inproceedings'}_extras="k" " isbn
let s:{'inproceedings'}_retval = '@INPROCEEDINGS{' . s:key . ','."\n"

let s:{'conference'}_required="atby" " b is booktitle
let s:{'conference'}_optional1="epwoq" " w is address, q is publisher
let s:{'conference'}_optional2="vnsmz"
let s:{'conference'}_extras="k" " isbn
let s:{'conference'}_retval = '@CONFERENCE{' . s:key . ','."\n"

let s:{'manual'}_required="t"
let s:{'manual'}_optional1="ow"
let s:{'manual'}_optional2="admyz" " w is address
let s:{'manual'}_retval = '@MANUAL{' . s:key . ','."\n"

let s:{'msthesis'}_required="atry" " r is school
let s:{'msthesis'}_optional1="w" " w is address
let s:{'msthesis'}_optional2="umz" " u is type, w is address
let s:{'msthesis'}_retval = '@MASTERSTHESIS{' . s:key . ','."\n"

let s:{'misc'}_required=""
let s:{'misc'}_optional1="ath"
let s:{'misc'}_optional2="myz"
let s:{'misc'}_retval = '@MISC{' . s:key . ','."\n"

let s:{'phdthesis'}_required="atry" " r is school
let s:{'phdthesis'}_optional1="w" " w is address
let s:{'phdthesis'}_optional2="umz" " u is type
let s:{'phdthesis'}_retval = '@PHDTHESIS{' . s:key . ','."\n"

let s:{'proceedings'}_required="ty"
let s:{'proceedings'}_optional1="ewo" " w is address
let s:{'proceedings'}_optional2="vnsmqz" " q is publisher
let s:{'proceedings'}_retval = '@PROCEEDINGS{' . s:key . ','."\n"

let s:{'techreport'}_required="atiy"
let s:{'techreport'}_optional1="unw" " u is type, w is address
let s:{'techreport'}_optional2="mz"
let s:{'techreport'}_retval = '@TECHREPORT{' . s:key . ','."\n"

let s:{'unpublished'}_required="atz"
let s:{'unpublished'}_optional1="y"
let s:{'unpublished'}_optional2="m"
let s:{'unpublished'}_retval = '@UNPUBLISHED{' . s:key . ','."\n"

" }}}

if exists('s:done')
	finish
endif
let s:done = 1

call IMAP ('BBB', "\<C-r>=BibT('', '', 0)\<CR>", 'bib')
call IMAP ('BBL', "\<C-r>=BibT('', 'o', 0)\<CR>", 'bib')
call IMAP ('BBH', "\<C-r>=BibT('', 'O', 0)\<CR>", 'bib')
call IMAP ('BBX', "\<C-r>=BibT('', 'Ox', 0)\<CR>", 'bib')

" BibT: function to generate a formatted bibtex entry {{{
" three sample usages:
"   :call BibT()                    will request type choice
"   :call BibT("article")           preferred, provides most common fields
"   :call BibT("article","ox")      more optional fields (o) and extras (x)
"
" Input Arguments:
" type: is one of the types listed above. (this should be a complete name, not
"       the acronym).
" options: a string containing 0 or more of the letters 'oOx'
"          where
"          o: include a bib entry with first set of options
"          O: include a bib entry with extended options
"          x: incude bib entry with extra options
" prompt: whether the fields are asked to be filled on the command prompt or
"         whether place-holders are used. when prompt == 1, then comman line
"         questions are used.
"
" Returns:
" a string containing a formatted bib entry
function BibT(type, options, prompt)
	if a:type != ''
		let choosetype = a:type
	else
		let types = 
			\ 'article'."\n".
			\ 'booklet'."\n".
			\ 'book'."\n".
			\ 'conference'."\n".
			\ 'inbook'."\n".
			\ 'incollection'."\n".
			\ 'inproceedings'."\n".
			\ 'manual'."\n".
			\ 'msthesis'."\n".
			\ 'misc'."\n".
			\ 'phdthesis'."\n".
			\ 'proceedings'."\n".
			\ 'techreport'."\n".
			\ 'unpublished'
		let choosetype = Tex_ChooseFromPrompt(
					\ "Choose the type of bibliographic entry: \n" . 
					\ Tex_CreatePrompt(types, 3, "\n") .
					\ "\nEnter number or filename :", 
					\ types, "\n")
		if choosetype == ''
			let choosetype = 'article'
		endif
		if types !~ '^\|\n'.choosetype.'$\|\n'
			echomsg 'Please choose only one of the given types'
			return
		endif
	endif
	if a:options != ''
		let options = a:options
	else
		let options = ""
	endif

	let fields = ''
	let extras=""
	let retval = ""

	" define fields
	let fields = s:{choosetype}_required
	if options =~ 'o' && exists('s:'.choosetype.'_optional1')
		let fields = fields . s:{choosetype}_optional1
	endif
	if options =~ "O" && exists('s:'.choosetype.'_optional2')
		if options !~ 'o'&& exists('s:'.choosetype.'_optional1') 
			let fields = fields . s:{choosetype}_optional1
		endif
		let fields = fields . s:{choosetype}_optional2
	endif
	if options =~ "x" && exists('s:'.choosetype.'_extras')
		let fields = fields . extras
	endif
	if exists('g:Bib_'.choosetype.'_options')
		let fields = fields . g:Bib_{choosetype}_options
	endif

	let retval = s:{choosetype}_retval
	
	let i = 0
	while i < strlen(fields)
		let field = strpart(fields, i, 1)
		if exists('s:'.field.'_standsfor')
			let field_name = s:{field}_standsfor
			let retval = retval.field_name." = {<++>},\n"
		endif

		let i = i + 1
	endwhile
	let retval = retval.'otherinfo = {<++>}'."\n"
	let retval = retval."}<++>"."\n"

	return IMAP_PutTextWithMovement(retval)
endfunction

" }}}
function! s:Input(prompt, ask) " {{{
	if a:ask == 1
		let retval = input(a:prompt)
		if retval == ''
			return "<++>"
		endif
	else
		return "<++>"
	endif
endfunction 

" }}}

" vim:fdm=marker:ff=unix:noet:ts=4:sw=4
