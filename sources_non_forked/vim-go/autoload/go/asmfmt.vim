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
"   g:go_asmfmt_autosave [default=1]
"
"       Flag to automatically call :Fmt when file is saved.

let s:got_fmt_error = 0

" This is a trimmed-down version of the logic in fmt.vim.

function! go#asmfmt#Format()
  " Save state.
  let l:curw = winsaveview()

  " Write the current buffer to a tempfile.
  let l:tmpname = tempname()
  call writefile(getline(1, '$'), l:tmpname)

  " Run asmfmt.
  let path = go#path#CheckBinPath("asmfmt")
  if empty(path)
    return
  endif
  let out = go#util#System(path . ' -w ' . l:tmpname)

  " If there's no error, replace the current file with the output.
  if go#util#ShellError() == 0
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
  endif

  " Restore the cursor/window positions.
  call winrestview(l:curw)
endfunction

function! go#asmfmt#ToggleAsmFmtAutoSave()
  if get(g:, "go_asmfmt_autosave", 1)
    let g:go_asmfmt_autosave = 0
    call go#util#EchoProgress("auto asmfmt disabled")
    return
  end

  let g:go_asmfmt_autosave = 1
  call go#util#EchoProgress("auto asmfmt enabled")
endfunction

" vim: sw=2 ts=2 et
