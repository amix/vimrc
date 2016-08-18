"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim indent file
"
" Language: JSX (JavaScript)
" Maintainer: Max Wang <mxawng@gmail.com>
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Save the current JavaScript indentexpr.
let b:jsx_js_indentexpr = &indentexpr

" Prologue; load in XML indentation.
if exists('b:did_indent')
  let s:did_indent=b:did_indent
  unlet b:did_indent
endif
exe 'runtime! indent/xml.vim'
if exists('s:did_indent')
  let b:did_indent=s:did_indent
endif

setlocal indentexpr=GetJsxIndent()

" JS indentkeys
setlocal indentkeys=0{,0},0),0],0\,,!^F,o,O,e
" XML indentkeys
setlocal indentkeys+=*<Return>,<>>,<<>,/

" Multiline end tag regex (line beginning with '>' or '/>')
let s:endtag = '^\s*\/\?>\s*;\='

" Get all syntax types at the beginning of a given line.
fu! SynSOL(lnum)
  return map(synstack(a:lnum, 1), 'synIDattr(v:val, "name")')
endfu

" Get all syntax types at the end of a given line.
fu! SynEOL(lnum)
  let lnum = prevnonblank(a:lnum)
  let col = strlen(getline(lnum))
  return map(synstack(lnum, col), 'synIDattr(v:val, "name")')
endfu

" Check if a syntax attribute is XMLish.
fu! SynAttrXMLish(synattr)
  return a:synattr =~ "^xml" || a:synattr =~ "^jsx"
endfu

" Check if a synstack is XMLish (i.e., has an XMLish last attribute).
fu! SynXMLish(syns)
  return SynAttrXMLish(get(a:syns, -1))
endfu

" Check if a synstack denotes the end of a JSX block.
fu! SynJSXBlockEnd(syns)
  return get(a:syns, -1) =~ '\%(js\|javascript\)Braces' &&
       \ SynAttrXMLish(get(a:syns, -2))
endfu

" Determine how many jsxRegions deep a synstack is.
fu! SynJSXDepth(syns)
  return len(filter(copy(a:syns), 'v:val ==# "jsxRegion"'))
endfu

" Check whether `cursyn' continues the same jsxRegion as `prevsyn'.
fu! SynJSXContinues(cursyn, prevsyn)
  let curdepth = SynJSXDepth(a:cursyn)
  let prevdepth = SynJSXDepth(a:prevsyn)

  " In most places, we expect the nesting depths to be the same between any
  " two consecutive positions within a jsxRegion (e.g., between a parent and
  " child node, between two JSX attributes, etc.).  The exception is between
  " sibling nodes, where after a completed element (with depth N), we return
  " to the parent's nesting (depth N - 1).  This case is easily detected,
  " since it is the only time when the top syntax element in the synstack is
  " jsxRegion---specifically, the jsxRegion corresponding to the parent.
  return prevdepth == curdepth ||
      \ (prevdepth == curdepth + 1 && get(a:cursyn, -1) ==# 'jsxRegion')
endfu

" Cleverly mix JS and XML indentation.
fu! GetJsxIndent()
  let cursyn  = SynSOL(v:lnum)
  let prevsyn = SynEOL(v:lnum - 1)

  " Use XML indenting iff:
  "   - the syntax at the end of the previous line was either JSX or was the
  "     closing brace of a jsBlock whose parent syntax was JSX; and
  "   - the current line continues the same jsxRegion as the previous line.
  if (SynXMLish(prevsyn) || SynJSXBlockEnd(prevsyn)) &&
        \ SynJSXContinues(cursyn, prevsyn)
    let ind = XmlIndentGet(v:lnum, 0)

    " Align '/>' and '>' with '<' for multiline tags.
    if getline(v:lnum) =~? s:endtag
      let ind = ind - &sw
    endif

    " Then correct the indentation of any JSX following '/>' or '>'.
    if getline(v:lnum - 1) =~? s:endtag
      let ind = ind + &sw
    endif
  else
    if len(b:jsx_js_indentexpr)
      " Invoke the base JS package's custom indenter.  (For vim-javascript,
      " e.g., this will be GetJavascriptIndent().)
      let ind = eval(b:jsx_js_indentexpr)
    else
      let ind = cindent(v:lnum)
    endif
  endif

  return ind
endfu
