let s:deferred = []

function! copilot#util#Nop(...) abort
  return v:null
endfunction

function! copilot#util#Defer(fn, ...) abort
  call add(s:deferred, function(a:fn, a:000))
  return timer_start(0, function('s:RunDeferred'))
endfunction

function! s:RunDeferred(...) abort
  if empty(s:deferred)
    return
  endif
  let Fn = remove(s:deferred, 0)
  call timer_start(0, function('s:RunDeferred'))
  call call(Fn, [])
endfunction

function! copilot#util#UTF16Width(str) abort
  return strchars(substitute(a:str, "\\%#=2[^\u0001-\uffff]", "  ", 'g'))
endfunction

if exists('*utf16idx')

  function! copilot#util#UTF16ToByteIdx(str, utf16_idx) abort
    return byteidx(a:str, a:utf16_idx, 1)
  endfunction

elseif has('nvim')

  function! copilot#util#UTF16ToByteIdx(str, utf16_idx) abort
    try
      return v:lua.vim.str_byteindex(a:str, a:utf16_idx, 1)
    catch /^Vim(return):E5108:/
      return -1
    endtry
  endfunction

else

  function! copilot#util#UTF16ToByteIdx(str, utf16_idx) abort
    if copilot#util#UTF16Width(a:str) < a:utf16_idx
      return -1
    endif
    let end_offset = len(a:str)
    while copilot#util#UTF16Width(strpart(a:str, 0, end_offset)) > a:utf16_idx && end_offset > 0
      let end_offset -= 1
    endwhile
    return end_offset
  endfunction

endif

function! copilot#util#AppendPosition() abort
  let line = getline('.')
  let col_byte = col('.') - (mode() =~# '^[iR]' || empty(line))
  let col_utf16 = copilot#util#UTF16Width(strpart(line, 0, col_byte))
  return {'line': line('.') - 1, 'character': col_utf16}
endfunction
