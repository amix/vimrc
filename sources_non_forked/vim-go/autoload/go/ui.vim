let s:buf_nr = -1

"OpenWindow opens a new scratch window and put's the content into the window
function! go#ui#OpenWindow(title, content, filetype) abort
  " Ensure there's only one return window in this session/tabpage
  call go#util#Windo("unlet! w:vim_go_return_window")
  " Mark the window we're leaving as such
  let w:vim_go_return_window = 1

  " reuse existing buffer window if it exists otherwise create a new one
  if !bufexists(s:buf_nr)
    execute 'botright new'
    file `="[" . a:title . "]"`
    let s:buf_nr = bufnr('%')
  elseif bufwinnr(s:buf_nr) == -1
    execute 'botright new'
    execute s:buf_nr . 'buffer'
  elseif bufwinnr(s:buf_nr) != bufwinnr('%')
    execute bufwinnr(s:buf_nr) . 'wincmd w'
  endif

  " Resize window to content length
  exe 'resize' . len(a:content)

  execute "setlocal filetype=".a:filetype

  " some sane default values for a readonly buffer
  setlocal bufhidden=delete
  setlocal buftype=nofile
  setlocal noswapfile
  setlocal nobuflisted
  setlocal winfixheight
  setlocal cursorline " make it easy to distinguish
  setlocal nonumber
  setlocal norelativenumber
  setlocal showbreak=""

  " we need this to purge the buffer content
  setlocal modifiable

  "delete everything first from the buffer
  %delete _

  " add the content
  call append(0, a:content)

  " delete last line that comes from the append call
  $delete _

  " set it back to non modifiable
  setlocal nomodifiable

  " Remove the '... [New File]' message line from the command line
  echon
endfunction

function! go#ui#GetReturnWindow() abort
  for l:wn in range(1, winnr("$"))
    if !empty(getwinvar(l:wn, "vim_go_return_window"))
      return l:wn
    endif
  endfor
endfunction

" CloseWindow closes the current window
function! go#ui#CloseWindow() abort
  " Close any window associated with the ui buffer, if it's there
  if bufexists(s:buf_nr)
    let ui_window_number = bufwinnr(s:buf_nr)
    if ui_window_number != -1
      execute ui_window_number . 'close'
    endif
  endif

  "return to original window, if it's there
  let l:rw = go#ui#GetReturnWindow()
  if !empty(l:rw)
    execute l:rw . 'wincmd w'
    unlet! w:vim_go_return_window
  endif
endfunction

" OpenDefinition parses the current line and jumps to it by openening a new
" tab
function! go#ui#OpenDefinition(filter) abort
  let curline = getline('.')

  " don't touch our first line or any blank line
  if curline =~ a:filter || curline =~ "^$"
    " suppress information about calling this function
    echo ""
    return
  endif

  " format: 'interface file:lnum:coln'
  let mx = '^\(^\S*\)\s*\(.\{-}\):\(\d\+\):\(\d\+\)'

  " parse it now into the list
  let tokens = matchlist(curline, mx)

  " convert to: 'file:lnum:coln'
  let expr = tokens[2] . ":" . tokens[3] . ":" .  tokens[4]

  " jump to it in a new tab, we use explicit lgetexpr so we can later change
  " the behaviour via settings (like opening in vsplit instead of tab)
  lgetexpr expr
  tab split
  ll 1

  " center the word
  norm! zz
endfunction

" vim: sw=2 ts=2 et
