if exists("b:did_indent")
  finish
endif

runtime! indent/html.vim

" Indent Golang HTML templates
setlocal indentexpr=GetGoHTMLTmplIndent(v:lnum)
setlocal indentkeys+==else,=end

" Only define the function once.
if exists("*GetGoHTMLTmplIndent")
  finish
endif

function! GetGoHTMLTmplIndent(lnum)
  " Get HTML indent
  if exists('*HtmlIndent')
    let ind = HtmlIndent()
  else
    let ind = HtmlIndentGet(a:lnum)
  endif

  " The value of a single shift-width
  if exists('*shiftwidth')
    let sw = shiftwidth()
  else
    let sw = &sw
  endif

  " If need to indent based on last line
  let last_line = getline(a:lnum-1)
  if last_line =~ '^\s*{{\s*\%(if\|else\|range\|with\|define\|block\).*}}'
    let ind += sw
  endif

  " End of FuncMap block
  let current_line = getline(a:lnum)
  if current_line =~ '^\s*{{\s*\%(else\|end\).*}}'
    let ind -= sw
  endif

  return ind
endfunction
