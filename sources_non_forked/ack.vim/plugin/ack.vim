if !exists("g:ack_default_options")
  let g:ack_default_options = " -s -H --nocolor --nogroup --column"
endif

" Location of the ack utility
if !exists("g:ackprg")
  if executable('ack')
    let g:ackprg = "ack"
  elseif executable('ack-grep')
    let g:ackprg = "ack-grep"
  else
    finish
  endif
  let g:ackprg .= g:ack_default_options
endif

if !exists("g:ack_apply_qmappings")
  let g:ack_apply_qmappings = !exists("g:ack_qhandler")
endif

if !exists("g:ack_apply_lmappings")
  let g:ack_apply_lmappings = !exists("g:ack_lhandler")
endif

if !exists("g:ack_use_dispatch")
  let g:ack_use_dispatch = 0
end

let s:ack_mappings = {
      \ "t": "<C-W><CR><C-W>T",
      \ "T": "<C-W><CR><C-W>TgT<C-W>j",
      \ "o": "<CR>",
      \ "O": "<CR><C-W>p<C-W>c",
      \ "go": "<CR><C-W>p",
      \ "h": "<C-W><CR><C-W>K",
      \ "H": "<C-W><CR><C-W>K<C-W>b",
      \ "v": "<C-W><CR><C-W>H<C-W>b<C-W>J<C-W>t",
      \ "gv": "<C-W><CR><C-W>H<C-W>b<C-W>J" }

if exists("g:ack_mappings")
  let g:ack_mappings = extend(s:ack_mappings, g:ack_mappings)
else
  let g:ack_mappings = s:ack_mappings
endif

if !exists("g:ack_qhandler")
  let g:ack_qhandler = "botright copen"
endif

if !exists("g:ack_lhandler")
  let g:ack_lhandler = "botright lopen"
endif

if !exists("g:ackhighlight")
  let g:ackhighlight = 0
endif

if !exists("g:ack_autoclose")
  let g:ack_autoclose = 0
endif

if !exists("g:ack_autofold_results")
  let g:ack_autofold_results = 0
endif

command! -bang -nargs=* -complete=file Ack           call ack#Ack('grep<bang>', <q-args>)
command! -bang -nargs=* -complete=file AckAdd        call ack#Ack('grepadd<bang>', <q-args>)
command! -bang -nargs=* -complete=file AckFromSearch call ack#AckFromSearch('grep<bang>', <q-args>)
command! -bang -nargs=* -complete=file LAck          call ack#Ack('lgrep<bang>', <q-args>)
command! -bang -nargs=* -complete=file LAckAdd       call ack#Ack('lgrepadd<bang>', <q-args>)
command! -bang -nargs=* -complete=file AckFile       call ack#Ack('grep<bang> -g', <q-args>)
command! -bang -nargs=* -complete=help AckHelp       call ack#AckHelp('grep<bang>', <q-args>)
command! -bang -nargs=* -complete=help LAckHelp      call ack#AckHelp('lgrep<bang>', <q-args>)
command! -bang -nargs=* -complete=help AckWindow     call ack#AckWindow('grep<bang>', <q-args>)
command! -bang -nargs=* -complete=help LAckWindow    call ack#AckWindow('lgrep<bang>', <q-args>)
