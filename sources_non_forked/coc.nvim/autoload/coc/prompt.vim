scriptencoding utf-8
let s:is_vim = !has('nvim')
let s:activated = 0
let s:session_names = []
let s:saved_ve = &t_ve
let s:saved_cursor = &guicursor
let s:gui = has('gui_running') || has('nvim')

let s:char_map = {
      \ "\<Plug>": '<plug>',
      \ "\<Esc>": '<esc>',
      \ "\<Tab>": '<tab>',
      \ "\<S-Tab>": '<s-tab>',
      \ "\<bs>": '<bs>',
      \ "\<right>": '<right>',
      \ "\<left>": '<left>',
      \ "\<up>": '<up>',
      \ "\<down>": '<down>',
      \ "\<home>": '<home>',
      \ "\<end>": '<end>',
      \ "\<cr>": '<cr>',
      \ "\<PageUp>":'<PageUp>' ,
      \ "\<PageDown>":'<PageDown>' ,
      \ "\<FocusGained>":'<FocusGained>',
      \ "\<FocusLost>":'<FocusLost>',
      \ "\<ScrollWheelUp>": '<ScrollWheelUp>',
      \ "\<ScrollWheelDown>": '<ScrollWheelDown>',
      \ "\<LeftMouse>": '<LeftMouse>',
      \ "\<LeftDrag>": '<LeftDrag>',
      \ "\<LeftRelease>": '<LeftRelease>',
      \ "\<2-LeftMouse>": '<2-LeftMouse>',
      \ "\<C-a>": '<C-a>',
      \ "\<C-b>": '<C-b>',
      \ "\<C-c>": '<C-c>',
      \ "\<C-d>": '<C-d>',
      \ "\<C-e>": '<C-e>',
      \ "\<C-f>": '<C-f>',
      \ "\<C-g>": '<C-g>',
      \ "\<C-h>": '<C-h>',
      \ "\<C-j>": '<C-j>',
      \ "\<C-k>": '<C-k>',
      \ "\<C-l>": '<C-l>',
      \ "\<C-n>": '<C-n>',
      \ "\<C-o>": '<C-o>',
      \ "\<C-p>": '<C-p>',
      \ "\<C-q>": '<C-q>',
      \ "\<C-r>": '<C-r>',
      \ "\<C-s>": '<C-s>',
      \ "\<C-t>": '<C-t>',
      \ "\<C-u>": '<C-u>',
      \ "\<C-v>": '<C-v>',
      \ "\<C-w>": '<C-w>',
      \ "\<C-x>": '<C-x>',
      \ "\<C-y>": '<C-y>',
      \ "\<C-z>": '<C-z>',
      \ "\<A-a>": '<A-a>',
      \ "\<A-b>": '<A-b>',
      \ "\<A-c>": '<A-c>',
      \ "\<A-d>": '<A-d>',
      \ "\<A-e>": '<A-e>',
      \ "\<A-f>": '<A-f>',
      \ "\<A-g>": '<A-g>',
      \ "\<A-h>": '<A-h>',
      \ "\<A-i>": '<A-i>',
      \ "\<A-j>": '<A-j>',
      \ "\<A-k>": '<A-k>',
      \ "\<A-l>": '<A-l>',
      \ "\<A-m>": '<A-m>',
      \ "\<A-n>": '<A-n>',
      \ "\<A-o>": '<A-o>',
      \ "\<A-p>": '<A-p>',
      \ "\<A-q>": '<A-q>',
      \ "\<A-r>": '<A-r>',
      \ "\<A-s>": '<A-s>',
      \ "\<A-t>": '<A-t>',
      \ "\<A-u>": '<A-u>',
      \ "\<A-v>": '<A-v>',
      \ "\<A-w>": '<A-w>',
      \ "\<A-x>": '<A-x>',
      \ "\<A-y>": '<A-y>',
      \ "\<A-z>": '<A-z>',
      \ }

function! coc#prompt#getc() abort
  let c = getchar()
  return type(c) is 0 ? nr2char(c) : c
endfunction

function! coc#prompt#getchar() abort
  let input = coc#prompt#getc()
  if 1 != &iminsert
    return input
  endif
  "a language keymap is activated, so input must be resolved to the mapped values.
  let partial_keymap = mapcheck(input, 'l')
  while partial_keymap !=# ''
    let dict = maparg(input, 'l', 0, 1)
    if empty(dict) || get(dict, 'expr', 0)
      return input
    endif
    let full_keymap = get(dict, 'rhs', '')
    if full_keymap ==# "" && len(input) >= 3 "HACK: assume there are no keymaps longer than 3.
      return input
    elseif full_keymap ==# partial_keymap
      return full_keymap
    endif
    let c = coc#prompt#getc()
    if c ==# "\<Esc>" || c ==# "\<CR>"
      "if the short sequence has a valid mapping, return that.
      if !empty(full_keymap)
        return full_keymap
      endif
      return input
    endif
    let input .= c
    let partial_keymap = mapcheck(input, 'l')
  endwhile
  return input
endfunction

function! coc#prompt#start_prompt(session) abort
  let s:session_names = s:filter(s:session_names, a:session)
  call add(s:session_names, a:session)
  if s:activated | return | endif
  if s:is_vim
    call s:start_prompt_vim()
  else
    call s:start_prompt()
  endif
endfunction

function! s:start_prompt_vim() abort
  call timer_start(10, {-> s:start_prompt()})
endfunction

function! s:start_prompt()
  if s:activated | return | endif
  if !get(g:, 'coc_disable_transparent_cursor', 0)
    if s:gui
      if has('nvim-0.5.0') && !empty(s:saved_cursor)
        set guicursor+=a:ver1-CocCursorTransparent/lCursor
      endif
    elseif s:is_vim
      set t_ve=
    endif
  endif
  let s:activated = 1
  try
    while s:activated
      let ch = coc#prompt#getchar()
      if ch ==# "\<FocusLost>" || ch ==# "\<FocusGained>" || ch ==# "\<CursorHold>"
        continue
      else
        let curr = s:current_session()
        let mapped = get(s:char_map, ch, ch)
        if !empty(curr)
          call coc#rpc#notify('InputChar', [curr, mapped, getcharmod()])
        endif
        if mapped == '<esc>'
          let s:session_names = []
          call s:reset()
          break
        endif
      endif
    endwhile
  catch /^Vim:Interrupt$/
    let s:activated = 0
    call coc#rpc#notify('InputChar', [s:current_session(), '<esc>'])
    return
  endtry
  let s:activated = 0
endfunction

function! coc#prompt#stop_prompt(session)
  let s:session_names = s:filter(s:session_names, a:session)
  if len(s:session_names)
    return
  endif
  if s:activated
    let s:activated = 0
    call s:reset()
    call feedkeys("\<esc>", 'int')
  endif
endfunction

function! coc#prompt#activated() abort
  return s:activated
endfunction

function! s:reset() abort
  if !get(g:, 'coc_disable_transparent_cursor',0)
    " neovim has bug with revert empty &guicursor
    if s:gui && !empty(s:saved_cursor)
      if has('nvim-0.5.0')
        set guicursor+=a:ver1-Cursor/lCursor
        let &guicursor = s:saved_cursor
      endif
    elseif s:is_vim
      let &t_ve = s:saved_ve
    endif
  endif
  echo ""
endfunction

function! s:current_session() abort
  if empty(s:session_names)
    return v:null
  endif
  return s:session_names[len(s:session_names) - 1]
endfunction

function! s:filter(list, id) abort
  return filter(copy(a:list), 'v:val !=# a:id')
endfunction
