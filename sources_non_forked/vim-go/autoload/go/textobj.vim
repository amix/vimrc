if !exists("g:go_textobj_enabled")
  let g:go_textobj_enabled = 1
endif

if !exists("g:go_textobj_include_function_doc")
  let g:go_textobj_include_function_doc = 1
endif

" ( ) motions
" { } motions
" s for sentence
" p for parapgrah
" < >
" t for tag

function! go#textobj#Function(mode) abort
  let offset = go#util#OffsetCursor()

  let fname = shellescape(expand("%:p"))
  if &modified
    " Write current unsaved buffer to a temp file and use the modified content
    let l:tmpname = tempname()
    call writefile(getline(1, '$'), l:tmpname)
    let fname = l:tmpname
  endif

  let bin_path = go#path#CheckBinPath('motion')
  if empty(bin_path)
    return
  endif

  let command = printf("%s -format vim -file %s -offset %s", bin_path, fname, offset)
  let command .= " -mode enclosing"

  if g:go_textobj_include_function_doc
    let command .= " -parse-comments"
  endif

  let out = go#util#System(command)
  if go#util#ShellError() != 0
    call go#util#EchoError(out)
    return
  endif

  " if exists, delete it as we don't need it anymore
  if exists("l:tmpname")
    call delete(l:tmpname)
  endif

  " convert our string dict representation into native Vim dictionary type
  let result = eval(out)
  if type(result) != 4 || !has_key(result, 'fn')
    return
  endif

  let info = result.fn

  if a:mode == 'a'
    " anonymous functions doesn't have associated doc. Also check if the user
    " want's to include doc comments for function declarations
    if has_key(info, 'doc') && g:go_textobj_include_function_doc
      call cursor(info.doc.line, info.doc.col)
    else
      call cursor(info.func.line, info.func.col)
    endif

    normal! v
    call cursor(info.rbrace.line, info.rbrace.col)
    return
  endif 

  " rest is inner mode, a:mode == 'i'

  " if the function is a one liner we need to select only that portion
  if info.lbrace.line == info.rbrace.line
    call cursor(info.lbrace.line, info.lbrace.col+1)
    normal! v
    call cursor(info.rbrace.line, info.rbrace.col-1)
    return
  endif

  call cursor(info.lbrace.line+1, 1)
  normal! V
  call cursor(info.rbrace.line-1, 1)
endfunction

function! go#textobj#FunctionJump(mode, direction) abort
  " get count of the motion. This should be done before all the normal
  " expressions below as those reset this value(because they have zero
  " count!). We abstract -1 because the index starts from 0 in motion.
  let l:cnt = v:count1 - 1

  " set context mark so we can jump back with  '' or ``
  normal! m'

  " select already previously selected visual content and continue from there.
  " If it's the first time starts with the visual mode. This is needed so
  " after selecting something in visual mode, every consecutive motion
  " continues.
  if a:mode == 'v'
    normal! gv
  endif

  let offset = go#util#OffsetCursor()

  let fname = shellescape(expand("%:p"))
  if &modified
    " Write current unsaved buffer to a temp file and use the modified content
    let l:tmpname = tempname()
    call writefile(getline(1, '$'), l:tmpname)
    let fname = l:tmpname
  endif

  let bin_path = go#path#CheckBinPath('motion')
  if empty(bin_path)
    return
  endif

  let command = printf("%s -format vim -file %s -offset %s", bin_path, fname, offset)
  let command .= ' -shift ' . l:cnt

  if a:direction == 'next'
    let command .= ' -mode next'
  else " 'prev'
    let command .= ' -mode prev'
  endif

  if g:go_textobj_include_function_doc
    let command .= " -parse-comments"
  endif

  let out = go#util#System(command)
  if go#util#ShellError() != 0
    call go#util#EchoError(out)
    return
  endif

  " if exists, delete it as we don't need it anymore
  if exists("l:tmpname")
    call delete(l:tmpname)
  endif

  " convert our string dict representation into native Vim dictionary type
  let result = eval(out)
  if type(result) != 4 || !has_key(result, 'fn')
    return
  endif

  " we reached the end and there are no functions. The usual [[ or ]] jumps to
  " the top or bottom, we'll do the same.
  if type(result) == 4 && has_key(result, 'err') && result.err == "no functions found"
    if a:direction == 'next'
      keepjumps normal! G
    else " 'prev'
      keepjumps normal! gg
    endif
    return
  endif

  let info = result.fn

  " if we select something ,select all function
  if a:mode == 'v' && a:direction == 'next'
    keepjumps call cursor(info.rbrace.line, 1)
    return
  endif

  if a:mode == 'v' && a:direction == 'prev'
    if has_key(info, 'doc') && g:go_textobj_include_function_doc
      keepjumps call cursor(info.doc.line, 1)
    else
      keepjumps call cursor(info.func.line, 1)
    endif
    return
  endif

  keepjumps call cursor(info.func.line, 1)
endfunction

" vim: sw=2 ts=2 et
