" Vim indent file
" Language: Mako
" Author: Scott Torborg <storborg@mit.edu>
" Version: 0.4
" License: Do What The Fuck You Want To Public License (WTFPL)
"
" ---------------------------------------------------------------------------
"
"            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
"                    Version 2, December 2004
"
" Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>
"
" Everyone is permitted to copy and distribute verbatim or modified
" copies of this license document, and changing it is allowed as long
" as the name is changed.
"
"            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
"   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
"
"  0. You just DO WHAT THE FUCK YOU WANT TO.
"
" ---------------------------------------------------------------------------
"
" This script does more useful indenting for Mako HTML templates. It indents
" inside of control blocks, defs, etc. Note that this indenting style will
" sacrifice readability of the output text for the sake of readability of the
" template.
"
" We'll use HTML indenting globally, python inside <% %> blocks. Inspired by
" the excellent PHP + HTML indentation files such as php.vim by Pim Snel.
"
" Changelog:
"       0.4 - 5 March 2010
"       - Added license information
"       0.3 - 15 September 2009
"       - Added explicit indenting for ## comments, fixed unindenting count,
"       thanks to Mike Lewis (@MikeRLewis) for this
"       0.2 - 15 June 2009
"       - Fixed issue where opening and closing mako tags on the same line
"       would cause incorrect indenting
"       0.1 - 06 June 2009
"       - Initial public release of mako indent file

let sw=2    " default shiftwidth of 2 spaces

if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

setlocal nosmartindent
setlocal noautoindent
setlocal nocindent
setlocal nolisp

setlocal indentexpr=GetMakoIndent()
setlocal indentkeys+=*<Return>,<>>,<bs>,end,:

" Only define the function once.
if exists("*GetMakoIndent")
    finish
endif

if exists('g:html_indent_tags')
    unlet g:html_indent_tags
endif

function IsInsidePythonBlock(startline)
    " Loop until we get a line that's either <% or %>
    let lnum = a:startline
    while getline(lnum) !~ '\(%>\|<%\)$' && lnum > 0
        let lnum = lnum - 1
    endwhile

    " lnum points to the last control. If it's a <% then we're inside an
    " embedded python block, otherwise we're not.
    return getline(lnum) =~ '<%$'
endfunction

function GetMakoIndent()
    " Find a non-empty line above the current line
    let lnum = prevnonblank(v:lnum - 1)
    
    " Hit the start of the file, use zero indent.
    if lnum == 0
        return 0
    endif
    
    let line = getline(lnum)        " last line
    let cline = getline(v:lnum)     " current line
    let pline = getline(lnum - 1)   " previous to last line
    let ind = indent(lnum)
	if line =~ '^\s*##'
		return indent(lnum)
	end
    
    let restore_ic=&ic
    let &ic=1 " ignore case
    
    let ind = <SID>HtmlIndentSum(lnum, -1)
	let ind = <SID>HtmlIndentSum(lnum, -1)
	let ind = ind + <SID>HtmlIndentSum(v:lnum, 0)
    
	let &ic=restore_ic
	
	let ind = indent(lnum) + (&sw * ind)
    
    " Indent after %anything: or <%anything NOT ending in />
    if line =~ '^\s*%.*:\s*$'
        let ind = ind + &sw
    endif
    
    " Unindent before %end* or </%anything
    if cline =~ '^\s*%\s*end'
        let ind = ind - &sw
    endif
    "
    " Unindent before %else, %except, and %elif
    if cline =~ '^\s*%\s*else' || cline =~ '^\s*%\s*except' || cline =~ '^\s*%\s*elif'
        let ind = ind - &sw
    endif

    " Indent at the beginning of a python control block
    if line =~ '<%$'
        let ind = ind + &sw
    endif
    "
    " Unindent at the end of the python block.
    if cline =~ '^\s*%>$'
        let scanlnum = lnum
        " Scan backwards until we find the beginning of this python block.
        while getline(scanlnum) !~ '<%$' && scanlnum > 0
            let scanlnum = scanlnum - 1
        endwhile
        let ind = indent(scanlnum)
    endif

    " If we're inside a python block and the previous line ends in a colon,
    " indent.
    if IsInsidePythonBlock(lnum - 1)
        " Indent after :
        if line =~ '\:$'
            let ind = ind + &sw
        endif
    endif
    
    return ind
endfunction


" [-- helper function to assemble tag list --]
fun! <SID>HtmlIndentPush(tag)
    if exists('g:html_indent_tags')
        let g:html_indent_tags = g:html_indent_tags.'\|'.a:tag
    else
        let g:html_indent_tags = a:tag
    endif
endfun

fun! <SID>MakoIndentPush(tag)
    if exists('g:mako_indent_tags')
        let g:mako_indent_tags = g:mako_indent_tags.'\|'.a:tag
    else
        let g:mako_indent_tags = a:tag
    endif
endfun

" [-- <ELEMENT ? - - ...> --]
call <SID>HtmlIndentPush('a')
call <SID>HtmlIndentPush('abbr')
call <SID>HtmlIndentPush('acronym')
call <SID>HtmlIndentPush('address')
call <SID>HtmlIndentPush('b')
call <SID>HtmlIndentPush('bdo')
call <SID>HtmlIndentPush('big')
call <SID>HtmlIndentPush('blockquote')
call <SID>HtmlIndentPush('button')
call <SID>HtmlIndentPush('caption')
call <SID>HtmlIndentPush('center')
call <SID>HtmlIndentPush('cite')
call <SID>HtmlIndentPush('code')
call <SID>HtmlIndentPush('colgroup')
call <SID>HtmlIndentPush('del')
call <SID>HtmlIndentPush('dfn')
call <SID>HtmlIndentPush('dir')
call <SID>HtmlIndentPush('div')
call <SID>HtmlIndentPush('dl')
call <SID>HtmlIndentPush('em')
call <SID>HtmlIndentPush('fieldset')
call <SID>HtmlIndentPush('font')
call <SID>HtmlIndentPush('form')
call <SID>HtmlIndentPush('frameset')
call <SID>HtmlIndentPush('h1')
call <SID>HtmlIndentPush('h2')
call <SID>HtmlIndentPush('h3')
call <SID>HtmlIndentPush('h4')
call <SID>HtmlIndentPush('h5')
call <SID>HtmlIndentPush('h6')
call <SID>HtmlIndentPush('i')
call <SID>HtmlIndentPush('iframe')
call <SID>HtmlIndentPush('ins')
call <SID>HtmlIndentPush('kbd')
call <SID>HtmlIndentPush('label')
call <SID>HtmlIndentPush('legend')
call <SID>HtmlIndentPush('map')
call <SID>HtmlIndentPush('menu')
call <SID>HtmlIndentPush('noframes')
call <SID>HtmlIndentPush('noscript')
call <SID>HtmlIndentPush('object')
call <SID>HtmlIndentPush('ol')
call <SID>HtmlIndentPush('optgroup')
call <SID>HtmlIndentPush('pre')
call <SID>HtmlIndentPush('q')
call <SID>HtmlIndentPush('s')
call <SID>HtmlIndentPush('samp')
call <SID>HtmlIndentPush('script')
call <SID>HtmlIndentPush('select')
call <SID>HtmlIndentPush('small')
call <SID>HtmlIndentPush('span')
call <SID>HtmlIndentPush('strong')
call <SID>HtmlIndentPush('style')
call <SID>HtmlIndentPush('sub')
call <SID>HtmlIndentPush('sup')
call <SID>HtmlIndentPush('table')
call <SID>HtmlIndentPush('textarea')
call <SID>HtmlIndentPush('title')
call <SID>HtmlIndentPush('tt')
call <SID>HtmlIndentPush('u')
call <SID>HtmlIndentPush('ul')
call <SID>HtmlIndentPush('var')

" For some reason the default HTML indentation script doesn't consider these
" elements to be worthy of indentation.
call <SID>HtmlIndentPush('p')
call <SID>HtmlIndentPush('dt')
call <SID>HtmlIndentPush('dd')


" [-- <ELEMENT ? O O ...> --]
if !exists('g:html_indent_strict')
    call <SID>HtmlIndentPush('body')
    call <SID>HtmlIndentPush('head')
    call <SID>HtmlIndentPush('html')
    call <SID>HtmlIndentPush('tbody')
endif


" [-- <ELEMENT ? O - ...> --]
if !exists('g:html_indent_strict_table')
    call <SID>HtmlIndentPush('th')
    call <SID>HtmlIndentPush('td')
    call <SID>HtmlIndentPush('tr')
    call <SID>HtmlIndentPush('tfoot')
    call <SID>HtmlIndentPush('thead')
endif

" [-- <Mako Elements> --]
call <SID>MakoIndentPush('%def')
call <SID>MakoIndentPush('%call')
call <SID>MakoIndentPush('%doc')
call <SID>MakoIndentPush('%text')
call <SID>MakoIndentPush('%.\+:.\+')

delfun <SID>HtmlIndentPush
delfun <SID>MakoIndentPush

set cpo-=C

" [-- get number of regex matches in a string --]
fun! <SID>MatchCount(expr, pat)
    let mpos = 0
    let mcount = 0
    let expr = a:expr
    while (mpos > -1)
        let mend = matchend(expr, a:pat)
        if mend > -1
            let mcount = mcount + 1
        endif
        if mend == mpos
            let mpos = mpos + 1
        else
            let mpos = mend
        endif
        let expr = strpart(expr, mpos)
    endwhile
    return mcount
endfun

" [-- count indent-increasing tags of line a:lnum --]
fun! <SID>HtmlIndentOpen(lnum)
    let s = substitute('x'.getline(a:lnum),
    \ '.\{-}\(\(<\)\('.g:html_indent_tags.'\)\>\)', "\1", 'g')
    let s = substitute(s, "[^\1].*$", '', '')
    return strlen(s)
endfun

" [-- count indent-decreasing tags of line a:lnum --]
fun! <SID>HtmlIndentClose(lnum)
    let s = substitute('x'.getline(a:lnum),
    \ '.\{-}\(\(<\)/\('.g:html_indent_tags.'\)\>>\)', "\1", 'g')
    let s = substitute(s, "[^\1].*$", '', '')
    return strlen(s)
endfun

" [-- count indent-increasing mako tags of line a:lnum --]
fun! <SID>MakoIndentOpen(lnum)
    let s = substitute('x'.getline(a:lnum),
    \ '.\{-}\(\(<\)\('.g:mako_indent_tags.'\)\>\)', "\1", 'g')
    let s = substitute(s, "[^\1].*$", '', '')
    return strlen(s)
endfun

" [-- count indent-decreasing mako tags of line a:lnum --]
fun! <SID>MakoIndentClose(lnum)
    let mcount = <SID>MatchCount(getline(a:lnum), '</\('.g:mako_indent_tags.'\)>')
    let mcount = mcount + <SID>MatchCount(getline(a:lnum), '<\('.g:mako_indent_tags.'\)[^>]*/>')
    return mcount
endfun

" [-- count indent-increasing '{' of (java|css) line a:lnum --]
fun! <SID>HtmlIndentOpenAlt(lnum)
    return strlen(substitute(getline(a:lnum), '[^{]\+', '', 'g'))
endfun

" [-- count indent-decreasing '}' of (java|css) line a:lnum --]
fun! <SID>HtmlIndentCloseAlt(lnum)
    return strlen(substitute(getline(a:lnum), '[^}]\+', '', 'g'))
endfun

" [-- return the sum of indents respecting the syntax of a:lnum --]
fun! <SID>HtmlIndentSum(lnum, style)
    let open = <SID>HtmlIndentOpen(a:lnum) + <SID>MakoIndentOpen(a:lnum)
    let close = <SID>HtmlIndentClose(a:lnum) + <SID>MakoIndentClose(a:lnum)
    if a:style == match(getline(a:lnum), '^\s*</')
        if a:style == match(getline(a:lnum), '^\s*</\('.g:html_indent_tags.'\|'.g:mako_indent_tags.'\)')
            if 0 != open || 0 != close
                return open - close
            endif
        endif
    endif
    if '' != &syntax &&
	\ synIDattr(synID(a:lnum, 1, 1), 'name') =~ '\(css\|java\).*' &&
	\ synIDattr(synID(a:lnum, strlen(getline(a:lnum)) - 1, 1), 'name')
	\ =~ '\(css\|java\).*'
	if a:style == match(getline(a:lnum), '^\s*}')
	    return <SID>HtmlIndentOpenAlt(a:lnum) - <SID>HtmlIndentCloseAlt(a:lnum)
	endif
    endif
    return 0
endfun

" vim: set ts=4 sw=4:
