function! ack#Ack(cmd, args)
  redraw
  echo "Searching ..."

  " If no pattern is provided, search for the word under the cursor
  if empty(a:args)
    let l:grepargs = expand("<cword>")
  else
    let l:grepargs = a:args . join(a:000, ' ')
  end
  echom l:grepargs
  let l:ackprg_run = g:ackprg

  " Format, used to manage column jump
  if a:cmd =~# '-g$'
    let g:ackformat="%f"
    let l:ackprg_run = substitute(l:ackprg_run, '-H\|--column', '', 'g')
  else
    let g:ackformat="%f:%l:%c:%m,%f:%l:%m"
  endif

  let grepprg_bak = &grepprg
  let grepformat_bak = &grepformat
  let &grepprg=l:ackprg_run
  let &grepformat=g:ackformat

  try
    " NOTE: we escape special chars, but not everything using shellescape to
    "       allow for passing arguments etc
    if g:ack_use_dispatch
      let &l:errorformat = g:ackformat
      let &l:makeprg=g:ackprg." " . escape(l:grepargs, '|#%')
      Make
    else
      silent execute a:cmd . " " . escape(l:grepargs, '|#%')
    endif

  finally
    let &grepprg=grepprg_bak
    let &grepformat=grepformat_bak
  endtry

  if a:cmd =~# '^l'
    let s:handler = g:ack_lhandler
    let s:apply_mappings = g:ack_apply_lmappings
    let s:close_cmd = ':lclose<CR>'
  else
    let s:handler = g:ack_qhandler
    let s:apply_mappings = g:ack_apply_qmappings
    let s:close_cmd = ':cclose<CR>'
  endif

  if !g:ack_use_dispatch
    call ack#show_results()
  else
    copen
  endif
  call <SID>apply_maps()
  call <SID>highlight(l:grepargs)

  redraw!
endfunction

function! ack#show_results()
  execute s:handler
endfunction

function! s:apply_maps()
  let g:ack_mappings.q = s:close_cmd

  execute "nnoremap <buffer> <silent> ? :call ack#quick_help()<CR>"

  if s:apply_mappings && &ft == "qf"
    if g:ack_autoclose
      for key_map in items(g:ack_mappings)
        execute printf("nnoremap <buffer> <silent> %s %s", get(key_map, 0), get(key_map, 1) . s:close_cmd)
      endfor
      execute "nnoremap <buffer> <silent> <CR> <CR>" . s:close_cmd
    else
      for key_map in items(g:ack_mappings)
        execute printf("nnoremap <buffer> <silent> %s %s", get(key_map, 0), get(key_map, 1))
      endfor
    endif

    if exists("g:ackpreview") " if auto preview in on, remap j and k keys
      execute "nnoremap <buffer> <silent> j j<CR><C-W><C-W>"
      execute "nnoremap <buffer> <silent> k k<CR><C-W><C-W>"
    endif
  endif
endfunction

function! ack#quick_help()
  execute "edit " . globpath(&rtp, "doc/ack_quick_help.txt")

  silent normal gg
  setlocal buftype=nofile
  setlocal bufhidden=hide
  setlocal noswapfile
  setlocal nobuflisted
  setlocal nomodifiable
  setlocal filetype=help
  setlocal nonumber
  setlocal norelativenumber
  setlocal nowrap
  setlocal foldlevel=20
  setlocal foldmethod=diff
  nnoremap <buffer> <silent> ? :q!<CR>:call ack#show_results()<CR>
endfunction

function! s:highlight(args)
  if !g:ackhighlight
    return
  endif

  let @/ = matchstr(a:args, "\\v(-)\@<!(\<)\@<=\\w+|['\"]\\zs.{-}\\ze['\"]")
  call feedkeys(":let &l:hlsearch=1 \| echo \<CR>", "n")
endfunction

function! ack#AckFromSearch(cmd, args)
  let search = getreg('/')
  " translate vim regular expression to perl regular expression.
  let search = substitute(search, '\(\\<\|\\>\)', '\\b', 'g')
  call ack#Ack(a:cmd, '"' . search . '" ' . a:args)
endfunction

function! s:GetDocLocations()
  let dp = ''
  for p in split(&rtp, ',')
    let p = p . '/doc/'
    if isdirectory(p)
      let dp = p . '*.txt ' . dp
    endif
  endfor

  return dp
endfunction

function! ack#AckHelp(cmd, args)
  let args = a:args . ' ' . s:GetDocLocations()
  call ack#Ack(a:cmd, args)
endfunction

function! ack#AckWindow(cmd, args)
  let files = tabpagebuflist()
  " remove duplicated filenames (files appearing in more than one window)
  let files = filter(copy(sort(files)), 'index(files,v:val,v:key+1)==-1')
  call map(files, "bufname(v:val)")
  " remove unnamed buffers as quickfix (empty strings before shellescape)
  call filter(files, 'v:val != ""')
  " expand to full path (avoid problems with cd/lcd in au QuickFixCmdPre)
  let files = map(files, "shellescape(fnamemodify(v:val, ':p'))")
  let args = a:args . ' ' . join(files)
  call ack#Ack(a:cmd, args)
endfunction
