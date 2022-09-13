let s:is_vim = !has('nvim')
let s:virtual_text_support = has('nvim-0.5.0') || has('patch-9.0.0067')
let s:text_options = has('patch-9.0.0121')

" opts.hl_mode default to 'combine'.
" opts.col not used on neovim.
" opts.virt_text_win_col neovim only.
" opts.text_align could be 'after' 'right' 'below', vim9 only.
" opts.text_wrap could be 'wrap' and 'truncate', vim9 only.
function! coc#vtext#add(bufnr, src_id, line, blocks, opts) abort
  if !s:virtual_text_support
    return
  endif
  if s:is_vim
    for [text, hl] in a:blocks
      let type = coc#api#create_type(a:src_id, hl, a:opts)
      let column = get(a:opts, 'col', 0)
      let opts = { 'text': text, 'type': type }
      if s:text_options && column == 0
        let opts['text_align'] = get(a:opts, 'text_align', 'after')
        let opts['text_wrap'] = get(a:opts, 'text_wrap', 'truncate')
      endif
      call prop_add(a:line + 1, column, opts)
    endfor
  else
    let opts = {
          \ 'virt_text': a:blocks,
          \ 'hl_mode': get(a:opts, 'hl_mode', 'combine'),
          \ }
    if has('nvim-0.5.1') && has_key(a:opts, 'virt_text_win_col')
      let opts['virt_text_win_col'] = a:opts['virt_text_win_col']
      let opts['virt_text_pos'] = 'overlay'
    endif
    call nvim_buf_set_extmark(a:bufnr, a:src_id, a:line, 0, opts)
  endif
endfunction
