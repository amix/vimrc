" buf_identifier is either a buf_nr or a filename
" If any window shows the buffer move to the buffer
" If not show it in current window (by c-w s c^ you can always
" reshow the last buffer
"
" Example: buf_utils#GotoBuf("/tmp/tfile.txt", {'create': 1})
" returns: The command which was used to switch to the buffer
fun! buf_utils#GotoBuf(buf_identifier, opts)
  let buf_nr = bufnr(a:buf_identifier)
  if buf_nr == -1 && ( get(a:opts, 'create', 0) || has_key(a:opts, 'create_cmd'))
    exec get(a:opts,'create_cmd','e').' '.fnameescape(a:buf_identifier)
    return "e"
  else
    let win_nr = bufwinnr(buf_nr)
    if win_nr == -1
      exec 'b '.buf_nr
      return "b"
    else
      exec win_nr.'wincmd w'
      return "w"
    endif
    wincmd w"
  endif
endf
