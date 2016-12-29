" Tabular:     Align columnar data using regex-designated column boundaries
" Maintainer:  Matthew Wozniski (godlygeek@gmail.com)
" Date:        Thu, 03 May 2012 20:49:32 -0400
" Version:     1.0
"
" Long Description:
" Sometimes, it's useful to line up text.  Naturally, it's nicer to have the
" computer do this for you, since aligning things by hand quickly becomes
" unpleasant.  While there are other plugins for aligning text, the ones I've
" tried are either impossibly difficult to understand and use, or too simplistic
" to handle complicated tasks.  This plugin aims to make the easy things easy
" and the hard things possible, without providing an unnecessarily obtuse
" interface.  It's still a work in progress, and criticisms are welcome.
"
" License:
" Copyright (c) 2012, Matthew J. Wozniski
" All rights reserved.
"
" Redistribution and use in source and binary forms, with or without
" modification, are permitted provided that the following conditions are met:
"     * Redistributions of source code must retain the above copyright notice,
"       this list of conditions and the following disclaimer.
"     * Redistributions in binary form must reproduce the above copyright
"       notice, this list of conditions and the following disclaimer in the
"       documentation and/or other materials provided with the distribution.
"     * The names of the contributors may not be used to endorse or promote
"       products derived from this software without specific prior written
"       permission.
"
" THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ``AS IS'' AND ANY EXPRESS
" OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
" OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
" NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY DIRECT, INDIRECT,
" INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
" LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
" OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
" LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
" NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
" EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

" Stupid vimscript crap                                                   {{{1
let s:savecpo = &cpo
set cpo&vim

" Private Functions                                                       {{{1

" Return the number of bytes in a string after expanding tabs to spaces.  {{{2
" This expansion is done based on the current value of 'tabstop'
if exists('*strdisplaywidth')
  " Needs vim 7.3
  let s:Strlen = function("strdisplaywidth")
else
  function! s:Strlen(string)
    " Implement the tab handling part of strdisplaywidth for vim 7.2 and
    " earlier - not much that can be done about handling doublewidth
    " characters.
    let rv = 0
    let i = 0

    for char in split(a:string, '\zs')
      if char == "\t"
        let rv += &ts - i
        let i = 0
      else
        let rv += 1
        let i = (i + 1) % &ts
      endif
    endfor

    return rv
  endfunction
endif

" Align a string within a field                                           {{{2
" These functions do not trim leading and trailing spaces.

" Right align 'string' in a field of size 'fieldwidth'
function! s:Right(string, fieldwidth)
  let spaces = a:fieldwidth - s:Strlen(a:string)
  return matchstr(a:string, '^\s*') . repeat(" ", spaces) . substitute(a:string, '^\s*', '', '')
endfunction

" Left align 'string' in a field of size 'fieldwidth'
function! s:Left(string, fieldwidth)
  let spaces = a:fieldwidth - s:Strlen(a:string)
  return a:string . repeat(" ", spaces)
endfunction

" Center align 'string' in a field of size 'fieldwidth'
function! s:Center(string, fieldwidth)
  let spaces = a:fieldwidth - s:Strlen(a:string)
  let right = spaces / 2
  let left = right + (right * 2 != spaces)
  return repeat(" ", left) . a:string . repeat(" ", right)
endfunction

" Remove spaces around a string                                           {{{2

" Remove all trailing spaces from a string.
function! s:StripTrailingSpaces(string)
  return matchstr(a:string, '^.\{-}\ze\s*$')
endfunction

" Remove all leading spaces from a string.
function! s:StripLeadingSpaces(string)
  return matchstr(a:string, '^\s*\zs.*$')
endfunction

" Split a string into fields and delimiters                               {{{2
" Like split(), but include the delimiters as elements
" All odd numbered elements are delimiters
" All even numbered elements are non-delimiters (including zero)
function! s:SplitDelim(string, delim)
  let rv = []
  let beg = 0

  let len = len(a:string)
  let searchoff = 0

  while 1
    let mid = match(a:string, a:delim, beg + searchoff, 1)
    if mid == -1 || mid == len
      break
    endif

    let matchstr = matchstr(a:string, a:delim, beg + searchoff, 1)
    let length = strlen(matchstr)

    if length == 0 && beg == mid
      " Zero-length match for a zero-length delimiter - advance past it
      let searchoff += 1
      continue
    endif

    if beg == mid
      let rv += [ "" ]
    else
      let rv += [ a:string[beg : mid-1] ]
    endif

    let rv += [ matchstr ]

    let beg = mid + length
    let searchoff = 0
  endwhile

  let rv += [ strpart(a:string, beg) ]

  return rv
endfunction

" Replace lines from `start' to `start + len - 1' with the given strings. {{{2
" If more lines are needed to show all strings, they will be added.
" If there are too few strings to fill all lines, lines will be removed.
function! s:SetLines(start, len, strings)
  if a:start > line('$') + 1 || a:start < 1
    throw "Invalid start line!"
  endif

  if len(a:strings) > a:len
    let fensave = &fen
    let view = winsaveview()
    call append(a:start + a:len - 1, repeat([''], len(a:strings) - a:len))
    call winrestview(view)
    let &fen = fensave
  elseif len(a:strings) < a:len
    let fensave = &fen
    let view = winsaveview()
    sil exe (a:start + len(a:strings)) . ',' .  (a:start + a:len - 1) . 'd_'
    call winrestview(view)
    let &fen = fensave
  endif

  call setline(a:start, a:strings)
endfunction

" Runs the given commandstring argument as an expression.                 {{{2
" The commandstring expression is expected to reference the a:lines argument.
" If the commandstring expression returns a list the items of that list will
" replace the items in a:lines, otherwise the expression is assumed to have
" modified a:lines itself.
function! s:FilterString(lines, commandstring)
  exe 'let rv = ' . a:commandstring

  if type(rv) == type(a:lines) && rv isnot a:lines
    call filter(a:lines, 0)
    call extend(a:lines, rv)
  endif
endfunction

" Public API                                                              {{{1

if !exists("g:tabular_default_format")
  let g:tabular_default_format = "l1"
endif

let s:formatelempat = '\%([lrc]\d\+\)'

function! tabular#ElementFormatPattern()
  return s:formatelempat
endfunction

" Given a list of strings and a delimiter, split each string on every
" occurrence of the delimiter pattern, format each element according to either
" the provided format (optional) or the default format, and join them back
" together with enough space padding to guarantee that the nth delimiter of
" each string is aligned.
function! tabular#TabularizeStrings(strings, delim, ...)
  if a:0 > 1
    echoerr "TabularizeStrings accepts only 2 or 3 arguments (got ".(a:0+2).")"
    return 1
  endif

  let formatstr = (a:0 ? a:1 : g:tabular_default_format)

  if formatstr !~? s:formatelempat . '\+'
    echoerr "Tabular: Invalid format \"" . formatstr . "\" specified!"
    return 1
  endif

  let format = split(formatstr, s:formatelempat . '\zs')

  let lines = map(a:strings, 's:SplitDelim(v:val, a:delim)')

  " Strip spaces
  "   - Only from non-delimiters; spaces in delimiters must have been matched
  "     intentionally
  "   - Don't strip leading spaces from the first element; we like indenting.
  for line in lines
    if len(line) == 1 && s:do_gtabularize
      continue " Leave non-matching lines unchanged for GTabularize
    endif

    if line[0] !~ '^\s*$'
      let line[0] = s:StripTrailingSpaces(line[0])
    endif
    if len(line) >= 3
      for i in range(2, len(line)-1, 2)
        let line[i] = s:StripLeadingSpaces(s:StripTrailingSpaces(line[i]))
      endfor
    endif
  endfor

  " Find the max length of each field
  let maxes = []
  for line in lines
    if len(line) == 1 && s:do_gtabularize
      continue " non-matching lines don't affect field widths for GTabularize
    endif

    for i in range(len(line))
      if i == len(maxes)
        let maxes += [ s:Strlen(line[i]) ]
      else
        let maxes[i] = max( [ maxes[i], s:Strlen(line[i]) ] )
      endif
    endfor
  endfor

  let lead_blank = empty(filter(copy(lines), 'v:val[0] =~ "\\S"'))

  " Concatenate the fields, according to the format pattern.
  for idx in range(len(lines))
    let line = lines[idx]

    if len(line) == 1 && s:do_gtabularize
      let lines[idx] = line[0] " GTabularize doesn't change non-matching lines
      continue
    endif

    for i in range(len(line))
      let how = format[i % len(format)][0]
      let pad = format[i % len(format)][1:-1]

      if how =~? 'l'
        let field = s:Left(line[i], maxes[i])
      elseif how =~? 'r'
        let field = s:Right(line[i], maxes[i])
      elseif how =~? 'c'
        let field = s:Center(line[i], maxes[i])
      endif

      let line[i] = field . (lead_blank && i == 0 ? '' : repeat(" ", pad))
    endfor

    let lines[idx] = s:StripTrailingSpaces(join(line, ''))
  endfor
endfunction

" Apply 0 or more filters, in sequence, to selected text in the buffer    {{{2
" The lines to be filtered are determined as follows:
"   If the function is called with a range containing multiple lines, then
"     those lines will be used as the range.
"   If the function is called with no range or with a range of 1 line, then
"     if GTabularize mode is being used,
"       the range will not be adjusted
"     if "includepat" is not specified,
"       that 1 line will be filtered,
"     if "includepat" is specified and that line does not match it,
"       no lines will be filtered
"     if "includepat" is specified and that line does match it,
"       all contiguous lines above and below the specified line matching the
"       pattern will be filtered.
"
" The remaining arguments must each be a filter to apply to the text.
" Each filter must either be a String evaluating to a function to be called.
function! tabular#PipeRange(includepat, ...) range
  exe a:firstline . ',' . a:lastline
      \ . 'call tabular#PipeRangeWithOptions(a:includepat, a:000, {})'
endfunction

" Extended version of tabular#PipeRange, which
" 1) Takes the list of filters as an explicit list rather than as varargs
" 2) Supports passing a dictionary of options to control the routine.
"    Currently, the only supported option is 'mode', which determines whether
"    to behave as :Tabularize or as :GTabularize
" This allows me to add new features here without breaking API compatibility
" in the future.
function! tabular#PipeRangeWithOptions(includepat, filterlist, options) range
  let top = a:firstline
  let bot = a:lastline

  let s:do_gtabularize = (get(a:options, 'mode', '') ==# 'GTabularize')

  if !s:do_gtabularize
    " In the default mode, apply range extension logic
    if a:includepat != '' && top == bot
      if top < 0 || top > line('$') || getline(top) !~ a:includepat
        return
      endif
      while top > 1 && getline(top-1) =~ a:includepat
        let top -= 1
      endwhile
      while bot < line('$') && getline(bot+1) =~ a:includepat
        let bot += 1
      endwhile
    endif
  endif

  let lines = map(range(top, bot), 'getline(v:val)')

  for filter in a:filterlist
    if type(filter) != type("")
      echoerr "PipeRange: Bad filter: " . string(filter)
    endif

    call s:FilterString(lines, filter)

    unlet filter
  endfor

  call s:SetLines(top, bot - top + 1, lines)
endfunction

" Part of the public interface so interested pipelines can query this and
" adjust their behavior appropriately.
function! tabular#DoGTabularize()
  return s:do_gtabularize
endfunction

function! s:SplitDelimTest(string, delim, expected)
  let result = s:SplitDelim(a:string, a:delim)

  if result !=# a:expected
    echomsg 'Test failed!'
    echomsg '  string=' . string(a:string) . '  delim=' . string(a:delim)
    echomsg '  Returned=' . string(result)
    echomsg '  Expected=' . string(a:expected)
  endif
endfunction

function! tabular#SplitDelimUnitTest()
  let assignment = '[|&+*/%<>=!~-]\@<!\([<>!=]=\|=\~\)\@![|&+*/%<>=!~-]*='
  let two_spaces = '  '
  let ternary_operator = '^.\{-}\zs?\|:'
  let cpp_io = '<<\|>>'
  let pascal_assign = ':='
  let trailing_c_comments = '\/\*\|\*\/\|\/\/'

  call s:SplitDelimTest('a+=b',    assignment, ['a', '+=', 'b'])
  call s:SplitDelimTest('a-=b',    assignment, ['a', '-=', 'b'])
  call s:SplitDelimTest('a!=b',    assignment, ['a!=b'])
  call s:SplitDelimTest('a==b',    assignment, ['a==b'])
  call s:SplitDelimTest('a&=b',    assignment, ['a', '&=', 'b'])
  call s:SplitDelimTest('a|=b',    assignment, ['a', '|=', 'b'])
  call s:SplitDelimTest('a=b=c',   assignment, ['a', '=', 'b', '=', 'c'])

  call s:SplitDelimTest('a  b  c', two_spaces, ['a', '  ', 'b', '  ', 'c'])
  call s:SplitDelimTest('a b   c', two_spaces, ['a b', '  ', ' c'])
  call s:SplitDelimTest('ab    c', two_spaces, ['ab', '  ', '', '  ', 'c'])

  call s:SplitDelimTest('a?b:c',   ternary_operator, ['a', '?', 'b', ':', 'c'])

  call s:SplitDelimTest('a<<b<<c', cpp_io, ['a', '<<', 'b', '<<', 'c'])

  call s:SplitDelimTest('a:=b=c',  pascal_assign, ['a', ':=', 'b=c'])

  call s:SplitDelimTest('x//foo',  trailing_c_comments, ['x', '//', 'foo'])
  call s:SplitDelimTest('x/*foo*/',trailing_c_comments, ['x', '/*', 'foo', '*/', ''])

  call s:SplitDelimTest('#ab#cd#ef', '[^#]*', ['#', 'ab', '#', 'cd', '#', 'ef', ''])
  call s:SplitDelimTest('#ab#cd#ef', '#\zs',  ['#', '', 'ab#', '', 'cd#', '', 'ef'])
endfunction

" Stupid vimscript crap, part 2                                           {{{1
let &cpo = s:savecpo
unlet s:savecpo

" vim:set sw=2 sts=2 fdm=marker:
