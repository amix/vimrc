" This is the minimum number of characters required between regions of change
" in a line.  It's somewhat arbitrary: higher values mean less visual busyness;
" lower values mean more detail.
let s:gap_between_regions = 5


" Calculates the changed portions of lines.
"
" Based on:
"
" - diff-highlight (included with git)
"   https://github.com/git/git/blob/master/contrib/diff-highlight/DiffHighlight.pm
"
" - Diff Strategies, Neil Fraser
"   https://neil.fraser.name/writing/diff/


" Returns a list of intra-line changed regions.
" Each element is a list:
"
"   [
"     line number (1-based),
"     type ('+' or '-'),
"     start column (1-based, inclusive),
"     stop column (1-based, inclusive),
"   ]
"
" Args:
"   hunk_body - list of lines
function! gitgutter#diff_highlight#process(hunk_body)
  " Check whether we have the same number of lines added as removed.
  let [removed, added] = [0, 0]
  for line in a:hunk_body
    if line[0] == '-'
      let removed += 1
    elseif line[0] == '+'
      let added += 1
    endif
  endfor
  if removed != added
    return []
  endif

  let regions = []

  for i in range(removed)
    " pair lines by position
    let rline = a:hunk_body[i]
    let aline = a:hunk_body[i + removed]

    call s:diff(rline, aline, i, i+removed, 0, 0, regions, 1)
  endfor

  return regions
endfunction


function! s:diff(rline, aline, rlinenr, alinenr, rprefix, aprefix, regions, whole_line)
  " diff marker does not count as a difference in prefix
  let start = a:whole_line ? 1 : 0
  let prefix = s:common_prefix(a:rline[start:], a:aline[start:])
  if a:whole_line
    let prefix += 1
  endif
  let [rsuffix, asuffix] = s:common_suffix(a:rline, a:aline, prefix+1)

  " region of change (common prefix and suffix removed)
  let rtext = a:rline[prefix+1:rsuffix-1]
  let atext = a:aline[prefix+1:asuffix-1]

  " singular insertion
  if empty(rtext)
    if !a:whole_line || len(atext) != len(a:aline)  " not whole line
      call add(a:regions, [a:alinenr+1, '+', a:aprefix+prefix+1+1, a:aprefix+asuffix+1-1])
    endif
    return
  endif

  " singular deletion
  if empty(atext)
    if !a:whole_line || len(rtext) != len(a:rline)  " not whole line
      call add(a:regions, [a:rlinenr+1, '-', a:rprefix+prefix+1+1, a:rprefix+rsuffix+1-1])
    endif
    return
  endif

  " two insertions
  let j = stridx(atext, rtext)
  if j != -1
    call add(a:regions, [a:alinenr+1, '+', a:aprefix+prefix+1+1, a:aprefix+prefix+j+1])
    call add(a:regions, [a:alinenr+1, '+', a:aprefix+prefix+1+1+j+len(rtext), a:aprefix+asuffix+1-1])
    return
  endif

  " two deletions
  let j = stridx(rtext, atext)
  if j != -1
    call add(a:regions, [a:rlinenr+1, '-', a:rprefix+prefix+1+1, a:rprefix+prefix+j+1])
    call add(a:regions, [a:rlinenr+1, '-', a:rprefix+prefix+1+1+j+len(atext), a:rprefix+rsuffix+1-1])
    return
  endif

  " two edits
  let lcs = s:lcs(rtext, atext)
  " TODO do we need to ensure we don't get more than 2 elements when splitting?
  if len(lcs) > s:gap_between_regions
    let redits = s:split(rtext, lcs)
    let aedits = s:split(atext, lcs)
    call s:diff(redits[0], aedits[0], a:rlinenr, a:alinenr, a:rprefix+prefix+1,                         a:aprefix+prefix+1,                         a:regions, 0)
    call s:diff(redits[1], aedits[1], a:rlinenr, a:alinenr, a:rprefix+prefix+1+len(redits[0])+len(lcs), a:aprefix+prefix+1+len(aedits[0])+len(lcs), a:regions, 0)
    return
  endif

  " fall back to highlighting entire changed area

  " if a change (but not the whole line)
  if !a:whole_line || ((prefix != 0 || rsuffix != len(a:rline)) && prefix+1 < rsuffix)
    call add(a:regions, [a:rlinenr+1, '-', a:rprefix+prefix+1+1, a:rprefix+rsuffix+1-1])
  endif

  " if a change (but not the whole line)
  if !a:whole_line || ((prefix != 0 || asuffix != len(a:aline)) && prefix+1 < asuffix)
    call add(a:regions, [a:alinenr+1, '+', a:aprefix+prefix+1+1, a:aprefix+asuffix+1-1])
  endif
endfunction


function! s:lcs(s1, s2)
  if empty(a:s1) || empty(a:s2)
    return ''
  endif

  let matrix = map(repeat([repeat([0], len(a:s2)+1)], len(a:s1)+1), 'copy(v:val)')

  let maxlength = 0
  let endindex = len(a:s1)

  for i in range(1, len(a:s1))
    for j in range(1, len(a:s2))
      if a:s1[i-1] ==# a:s2[j-1]
        let matrix[i][j] = 1 + matrix[i-1][j-1]
        if matrix[i][j] > maxlength
          let maxlength = matrix[i][j]
          let endindex = i - 1
        endif
      endif
    endfor
  endfor

  return a:s1[endindex - maxlength + 1 : endindex]
endfunction


" Returns 0-based index of last character of common prefix
" If there is no common prefix, returns -1.
"
" a, b - strings
"
function! s:common_prefix(a, b)
  let len = min([len(a:a), len(a:b)])
  if len == 0
    return -1
  endif
  for i in range(len)
    if a:a[i:i] != a:b[i:i]
      return i - 1
    endif
  endfor
  return i
endfunction


" Returns 0-based indices of start of common suffix
"
" a, b - strings
" start - 0-based index to start from
function! s:common_suffix(a, b, start)
  let [sa, sb] = [len(a:a), len(a:b)]
  while sa >= a:start && sb >= a:start
    if a:a[sa] ==# a:b[sb]
      let sa -= 1
      let sb -= 1
    else
      break
    endif
  endwhile
  return [sa+1, sb+1]
endfunction


" Split a string on another string.
" Assumes 1 occurrence of the delimiter.
function! s:split(str, delimiter)
  let i = stridx(a:str, a:delimiter)

  if i == 0
    return ['', a:str[len(a:delimiter):]]
  endif

  return [a:str[:i-1], a:str[i+len(a:delimiter):]]
endfunction
