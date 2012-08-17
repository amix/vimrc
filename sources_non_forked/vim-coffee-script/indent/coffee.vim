" Language:    CoffeeScript
" Maintainer:  Mick Koch <kchmck@gmail.com>
" URL:         http://github.com/kchmck/vim-coffee-script
" License:     WTFPL

if exists("b:did_indent")
  finish
endif

let b:did_indent = 1

setlocal autoindent
setlocal indentexpr=GetCoffeeIndent(v:lnum)
" Make sure GetCoffeeIndent is run when these are typed so they can be
" indented or outdented.
setlocal indentkeys+=0],0),0.,=else,=when,=catch,=finally

" Only define the function once.
if exists("*GetCoffeeIndent")
  finish
endif

" Keywords to indent after
let s:INDENT_AFTER_KEYWORD = '^\%(if\|unless\|else\|for\|while\|until\|'
\                          . 'loop\|switch\|when\|try\|catch\|finally\|'
\                          . 'class\)\>'

" Operators to indent after
let s:INDENT_AFTER_OPERATOR = '\%([([{:=]\|[-=]>\)$'

" Keywords and operators that continue a line
let s:CONTINUATION = '\<\%(is\|isnt\|and\|or\)\>$'
\                  . '\|'
\                  . '\%(-\@<!-\|+\@<!+\|<\|[-=]\@<!>\|\*\|/\@<!/\|%\||\|'
\                  . '&\|,\|\.\@<!\.\)$'

" Operators that block continuation indenting
let s:CONTINUATION_BLOCK = '[([{:=]$'

" A continuation dot access
let s:DOT_ACCESS = '^\.'

" Keywords to outdent after
let s:OUTDENT_AFTER = '^\%(return\|break\|continue\|throw\)\>'

" A compound assignment like `... = if ...`
let s:COMPOUND_ASSIGNMENT = '[:=]\s*\%(if\|unless\|for\|while\|until\|'
\                         . 'switch\|try\|class\)\>'

" A postfix condition like `return ... if ...`.
let s:POSTFIX_CONDITION = '\S\s\+\zs\<\%(if\|unless\)\>'

" A single line else statement like `else ...` but not `else if ...`
let s:SINGLE_LINE_ELSE = '^else\s\+\%(\<\%(if\|unless\)\>\)\@!'

" Max lines to look back for a match
let s:MAX_LOOKBACK = 50

" Syntax names for strings
let s:SYNTAX_STRING = 'coffee\%(String\|AssignString\|Embed\|Regex\|Heregex\|'
\                   . 'Heredoc\)'

" Syntax names for comments
let s:SYNTAX_COMMENT = 'coffee\%(Comment\|BlockComment\|HeregexComment\)'

" Syntax names for strings and comments
let s:SYNTAX_STRING_COMMENT = s:SYNTAX_STRING . '\|' . s:SYNTAX_COMMENT

" Get the linked syntax name of a character.
function! s:SyntaxName(linenum, col)
  return synIDattr(synID(a:linenum, a:col, 1), 'name')
endfunction

" Check if a character is in a comment.
function! s:IsComment(linenum, col)
  return s:SyntaxName(a:linenum, a:col) =~ s:SYNTAX_COMMENT
endfunction

" Check if a character is in a string.
function! s:IsString(linenum, col)
  return s:SyntaxName(a:linenum, a:col) =~ s:SYNTAX_STRING
endfunction

" Check if a character is in a comment or string.
function! s:IsCommentOrString(linenum, col)
  return s:SyntaxName(a:linenum, a:col) =~ s:SYNTAX_STRING_COMMENT
endfunction

" Check if a whole line is a comment.
function! s:IsCommentLine(linenum)
  " Check the first non-whitespace character.
  return s:IsComment(a:linenum, indent(a:linenum) + 1)
endfunction

" Search a line for a regex until one is found outside a string or comment.
function! s:SmartSearch(linenum, regex)
  " Start at the first column.
  let col = 0

  " Search until there are no more matches, unless a good match is found.
  while 1
    call cursor(a:linenum, col + 1)
    let [_, col] = searchpos(a:regex, 'cn', a:linenum)

    " No more matches.
    if !col
      break
    endif

    if !s:IsCommentOrString(a:linenum, col)
      return 1
    endif
  endwhile

  " No good match found.
  return 0
endfunction

" Check if a match should be skipped.
function! s:ShouldSkip(startlinenum, linenum, col)
  " Skip if in a comment or string.
  if s:IsCommentOrString(a:linenum, a:col)
    return 1
  endif

  " Skip if a single line statement that isn't adjacent.
  if s:SmartSearch(a:linenum, '\<then\>') && a:startlinenum - a:linenum > 1
    return 1
  endif

  " Skip if a postfix condition.
  if s:SmartSearch(a:linenum, s:POSTFIX_CONDITION) &&
  \ !s:SmartSearch(a:linenum, s:COMPOUND_ASSIGNMENT)
    return 1
  endif

  return 0
endfunction

" Find the farthest line to look back to, capped to line 1 (zero and negative
" numbers cause bad things).
function! s:MaxLookback(startlinenum)
  return max([1, a:startlinenum - s:MAX_LOOKBACK])
endfunction

" Get the skip expression for searchpair().
function! s:SkipExpr(startlinenum)
  return "s:ShouldSkip(" . a:startlinenum . ", line('.'), col('.'))"
endfunction

" Search for pairs of text.
function! s:SearchPair(start, end)
  " The cursor must be in the first column for regexes to match.
  call cursor(0, 1)

  let startlinenum = line('.')

  " Don't need the W flag since MaxLookback caps the search to line 1.
  return searchpair(a:start, '', a:end, 'bcn',
  \                 s:SkipExpr(startlinenum),
  \                 s:MaxLookback(startlinenum))
endfunction

" Try to find a previous matching line.
function! s:GetMatch(curline)
  let firstchar = a:curline[0]

  if firstchar == '}'
    return s:SearchPair('{', '}')
  elseif firstchar == ')'
    return s:SearchPair('(', ')')
  elseif firstchar == ']'
    return s:SearchPair('\[', '\]')
  elseif a:curline =~ '^else\>'
    return s:SearchPair('\<\%(if\|unless\|when\)\>', '\<else\>')
  elseif a:curline =~ '^catch\>'
    return s:SearchPair('\<try\>', '\<catch\>')
  elseif a:curline =~ '^finally\>'
    return s:SearchPair('\<try\>', '\<finally\>')
  endif

  return 0
endfunction

" Get the nearest previous line that isn't a comment.
function! s:GetPrevNormalLine(startlinenum)
  let curlinenum = a:startlinenum

  while curlinenum
    let curlinenum = prevnonblank(curlinenum - 1)

    if !s:IsCommentLine(curlinenum)
      return curlinenum
    endif
  endwhile

  return 0
endfunction

" Try to find a comment in a line.
function! s:FindComment(linenum)
  call cursor(a:linenum, 0)

  " Current column
  let cur = 0
  " Last column in the line
  let end = col('$') - 1

  while cur != end
    call cursor(0, cur + 1)
    let [_, cur] = searchpos('#', 'cn', a:linenum)

    if !cur
      break
    endif

    if s:IsComment(a:linenum, cur)
      return cur
    endif
  endwhile

  return 0
endfunction

" Get a line without comments or surrounding whitespace.
function! s:GetTrimmedLine(linenum)
  let comment = s:FindComment(a:linenum)
  let line = getline(a:linenum)

  if comment
    " Subtract 1 to get to the column before the comment and another 1 for
    " zero-based indexing.
    let line = line[:comment - 2]
  endif

  return substitute(substitute(line, '^\s\+', '', ''),
  \                                  '\s\+$', '', '')
endfunction

function! GetCoffeeIndent(curlinenum)
  " Don't do anything if on the first line.
  if a:curlinenum == 1
    return -1
  endif

  let prevlinenum = a:curlinenum - 1

  " If continuing a comment, keep the indent level.
  if s:IsCommentLine(prevlinenum)
    return indent(prevlinenum)
  endif

  let prevlinenum = s:GetPrevNormalLine(a:curlinenum)

  " Don't do anything if there's no code before.
  if !prevlinenum
    return -1
  endif

  " Indent based on the current line.
  let curline = s:GetTrimmedLine(a:curlinenum)

  " Try to find a matching statement. This handles outdenting.
  let matchlinenum = s:GetMatch(curline)

  if matchlinenum
    return indent(matchlinenum)
  endif

  " Try to find a matching when.
  if curline =~ '^when\>' && !s:SmartSearch(prevlinenum, '\<switch\>')
    let linenum = a:curlinenum

    while linenum
      let linenum = s:GetPrevNormalLine(linenum)

      if getline(linenum) =~ '^\s*when\>'
        return indent(linenum)
      endif
    endwhile

    return -1
  endif

  " Indent based on the previous line.
  let prevline = s:GetTrimmedLine(prevlinenum)
  let previndent = indent(prevlinenum)

  " Always indent after these operators.
  if prevline =~ s:INDENT_AFTER_OPERATOR
    return previndent + &shiftwidth
  endif

  " Indent after a continuation if it's the first.
  if prevline =~ s:CONTINUATION
    " If the line ends in a slash, make sure it isn't a regex.
    if prevline =~ '/$'
      " Move to the line so we can get the last column.
      call cursor(prevlinenum)

      if s:IsString(prevlinenum, col('$') - 1)
        return -1
      endif
    endif

    let prevprevlinenum = s:GetPrevNormalLine(prevlinenum)

    " If the continuation is the first in the file, there can't be others before
    " it.
    if !prevprevlinenum
      return previndent + &shiftwidth
    endif

    let prevprevline = s:GetTrimmedLine(prevprevlinenum)

    " Only indent after the first continuation.
    if prevprevline !~ s:CONTINUATION && prevprevline !~ s:CONTINUATION_BLOCK
      return previndent + &shiftwidth
    endif

    return -1
  endif

  " Indent after these keywords and compound assignments if they aren't a
  " single line statement.
  if prevline =~ s:INDENT_AFTER_KEYWORD || prevline =~ s:COMPOUND_ASSIGNMENT
    if !s:SmartSearch(prevlinenum, '\<then\>') && prevline !~ s:SINGLE_LINE_ELSE
      return previndent + &shiftwidth
    endif

    return -1
  endif

  " Indent a dot access if it's the first.
  if curline =~ s:DOT_ACCESS && prevline !~ s:DOT_ACCESS
    return previndent + &shiftwidth
  endif

  " Outdent after these keywords if they don't have a postfix condition or are
  " a single-line statement.
  if prevline =~ s:OUTDENT_AFTER
    if !s:SmartSearch(prevlinenum, s:POSTFIX_CONDITION) ||
    \   s:SmartSearch(prevlinenum, '\<then\>')
      return previndent - &shiftwidth
    endif
  endif

  " If no indent or outdent is needed, keep the indent level of the previous
  " line.
  return previndent
endfunction
