" asmfmt.vim: Vim command to format Go asm files with asmfmt
" (github.com/klauspost/asmfmt).
"
" This filetype plugin adds new commands for asm buffers:
"
"   :Fmt
"
"       Filter the current asm buffer through asmfmt.
"       It tries to preserve cursor position and avoids
"       replacing the buffer with stderr output.
"
" Options:
"
"   g:go_asmfmt_autosave [default=0]
"
"       Flag to automatically call :Fmt when file is saved.

" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

let s:got_fmt_error = 0

" This is a trimmed-down version of the logic in fmt.vim.

function! go#asmfmt#Format() abort
  " Save state.
  let l:curw = winsaveview()

  " Write the current buffer to a tempfile.
  let l:tmpname = tempname()
  call writefile(go#util#GetLines(), l:tmpname)

  " Run asmfmt.
  let [l:out, l:err] = go#util#Exec(['asmfmt', '-w', l:tmpname])
  if l:err
    call go#util#EchoError(l:out)
    return
  endif

  " Remove undo point caused by BufWritePre.
  try | silent undojoin | catch | endtry

  " Replace the current file with the temp file; then reload the buffer.
  let old_fileformat = &fileformat

  " save old file permissions
  let original_fperm = getfperm(expand('%'))
  call rename(l:tmpname, expand('%'))

  " restore old file permissions
  call setfperm(expand('%'), original_fperm)
  silent edit!
  let &fileformat = old_fileformat
  let &syntax = &syntax

  " Restore the cursor/window positions.
  call winrestview(l:curw)
endfunction

function! go#asmfmt#ToggleAsmFmtAutoSave() abort
  if go#config#AsmfmtAutosave()
    call go#config#SetAsmfmtAutosave(1)
    call go#util#EchoProgress("auto asmfmt enabled")
    return
  end

  call go#config#SetAsmfmtAutosave(0)
  call go#util#EchoProgress("auto asmfmt disabled")
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
