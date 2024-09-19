if exists('g:loaded_copilot')
  finish
endif
let g:loaded_copilot = 1

scriptencoding utf-8

command! -bang -nargs=? -range=-1 -complete=customlist,copilot#CommandComplete Copilot exe copilot#Command(<line1>, <count>, +"<range>", <bang>0, "<mods>", <q-args>)

if v:version < 800 || !exists('##InsertLeavePre')
  finish
endif

function! s:ColorScheme() abort
  if &t_Co == 256
    hi def CopilotSuggestion guifg=#808080 ctermfg=244
  else
    hi def CopilotSuggestion guifg=#808080 ctermfg=12
  endif
  hi def link CopilotAnnotation MoreMsg
endfunction

function! s:MapTab() abort
  if get(g:, 'copilot_no_tab_map') || get(g:, 'copilot_no_maps')
    return
  endif
  let tab_map = maparg('<Tab>', 'i', 0, 1)
  if !has_key(tab_map, 'rhs')
    imap <script><silent><nowait><expr> <Tab> empty(get(g:, 'copilot_no_tab_map')) ? copilot#Accept() : "\t"
  elseif tab_map.rhs !~# 'copilot'
    if tab_map.expr
      let tab_fallback = '{ -> ' . tab_map.rhs . ' }'
    else
      let tab_fallback = substitute(json_encode(tab_map.rhs), '<', '\\<', 'g')
    endif
    let tab_fallback = substitute(tab_fallback, '<SID>', '<SNR>' . get(tab_map, 'sid') . '_', 'g')
    if get(tab_map, 'noremap') || get(tab_map, 'script') || mapcheck('<Left>', 'i') || mapcheck('<Del>', 'i')
      exe 'imap <script><silent><nowait><expr> <Tab> copilot#Accept(' . tab_fallback . ')'
    else
      exe 'imap <silent><nowait><expr>         <Tab> copilot#Accept(' . tab_fallback . ')'
    endif
  endif
endfunction

function! s:Event(type) abort
  try
    call call('copilot#On' . a:type, [])
  catch
    call copilot#logger#Exception('autocmd.' . a:type)
  endtry
endfunction

augroup github_copilot
  autocmd!
  autocmd FileType             * call s:Event('FileType')
  autocmd InsertLeavePre       * call s:Event('InsertLeavePre')
  autocmd BufLeave             * if mode() =~# '^[iR]'|call s:Event('InsertLeavePre')|endif
  autocmd InsertEnter          * call s:Event('InsertEnter')
  autocmd BufEnter             * if mode() =~# '^[iR]'|call s:Event('InsertEnter')|endif
  autocmd BufEnter             * call s:Event('BufEnter')
  autocmd CursorMovedI         * call s:Event('CursorMovedI')
  autocmd CompleteChanged      * call s:Event('CompleteChanged')
  autocmd ColorScheme,VimEnter * call s:ColorScheme()
  autocmd VimEnter             * call s:MapTab() | call copilot#Init()
  autocmd BufUnload            * call s:Event('BufUnload')
  autocmd VimLeavePre          * call s:Event('VimLeavePre')
  autocmd BufReadCmd copilot://* setlocal buftype=nofile bufhidden=wipe nobuflisted nomodifiable
  autocmd BufReadCmd copilot:///log call copilot#logger#BufReadCmd() | setfiletype copilotlog
augroup END

call s:ColorScheme()
call s:MapTab()
if !get(g:, 'copilot_no_maps')
  imap <Plug>(copilot-dismiss)     <Cmd>call copilot#Dismiss()<CR>
  if empty(mapcheck('<C-]>', 'i'))
    imap <silent><script><nowait><expr> <C-]> copilot#Dismiss() . "\<C-]>"
  endif
  imap <Plug>(copilot-next)     <Cmd>call copilot#Next()<CR>
  imap <Plug>(copilot-previous) <Cmd>call copilot#Previous()<CR>
  imap <Plug>(copilot-suggest)  <Cmd>call copilot#Suggest()<CR>
  imap <script><silent><nowait><expr> <Plug>(copilot-accept-word) copilot#AcceptWord()
  imap <script><silent><nowait><expr> <Plug>(copilot-accept-line) copilot#AcceptLine()
  try
    if !has('nvim') && &encoding ==# 'utf-8'
      " avoid 8-bit meta collision with UTF-8 characters
      let s:restore_encoding = 1
      silent noautocmd set encoding=cp949
    endif
    if empty(mapcheck('<M-]>', 'i'))
      imap <M-]> <Plug>(copilot-next)
    endif
    if empty(mapcheck('<M-[>', 'i'))
      imap <M-[> <Plug>(copilot-previous)
    endif
    if empty(mapcheck('<M-Bslash>', 'i'))
      imap <M-Bslash> <Plug>(copilot-suggest)
    endif
    if empty(mapcheck('<M-Right>', 'i'))
      imap <M-Right> <Plug>(copilot-accept-word)
    endif
    if empty(mapcheck('<M-C-Right>', 'i'))
      imap <M-C-Right> <Plug>(copilot-accept-line)
    endif
  finally
    if exists('s:restore_encoding')
      silent noautocmd set encoding=utf-8
    endif
  endtry
endif

let s:dir = expand('<sfile>:h:h')
if getftime(s:dir . '/doc/copilot.txt') > getftime(s:dir . '/doc/tags')
  silent! execute 'helptags' fnameescape(s:dir . '/doc')
endif
