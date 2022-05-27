" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

function! go#play#Share(count, line1, line2) abort
  if !executable('curl')
    call go#util#EchoError('cannot share: curl cannot be found')
    return
  endif

  let content = join(getline(a:line1, a:line2), "\n")
  let share_file = tempname()
  call writefile(split(content, "\n"), share_file, "b")

  let l:cmd = ['curl', '-s',
        \ '-H', 'Content-Type: text/plain; charset=utf-8',
        \ '-X', 'POST', 'https://go.dev/_/share',
        \ '--data-binary', '@' . l:share_file]
  let [l:snippet_id, l:err] = go#util#Exec(l:cmd)

  " we can remove the temp file because it's now posted.
  call delete(share_file)

  if l:err != 0
    call go#util#EchoError(['A error has occurred. Run this command to see what the problem is:', go#util#Shelljoin(l:cmd)])
    return
  endif

  let url = printf("https://go.dev/play/p/%s", snippet_id)

  " copy to clipboard
  if has('unix') && !has('xterm_clipboard') && !has('clipboard')
    let @" = url
  else
    let @+ = url
  endif

  if go#config#PlayOpenBrowser()
    call go#util#OpenBrowser(url)
  endif

  call go#util#EchoInfo('snippet uploaded: ' . url)
endfunction


function! s:get_visual_content() abort
  let save_regcont = @"
  let save_regtype = getregtype('"')
  silent! normal! gvy
  let content = @"
  call setreg('"', save_regcont, save_regtype)
  return content
endfunction

" modified version of
" http://stackoverflow.com/questions/1533565/how-to-get-visually-selected-text-in-vimscript
" another function that returns the content of visual selection, it's not used
" but might be useful in the future
function! s:get_visual_selection() abort
  let [lnum1, col1] = getpos("'<")[1:2]
  let [lnum2, col2] = getpos("'>")[1:2]

  " check if the the visual mode is used before
  if lnum1 == 0  || lnum2 == 0  || col1 == 0  || col2 == 0
    return
  endif

  let lines = getline(lnum1, lnum2)
  let lines[-1] = lines[-1][: col2 - (&selection == 'inclusive' ? 1 : 2)]
  let lines[0] = lines[0][col1 - 1:]
  return join(lines, "\n")
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
