" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

scriptencoding utf-8

let s:skip_symbol = '…'

function! airline#extensions#tabline#formatters#unique_tail_improved#format(bufnr, buffers)
  if len(a:buffers) <= 1 " don't need to compare bufnames if has less than one buffer opened
    return airline#extensions#tabline#formatters#default#format(a:bufnr, a:buffers)
  endif

  let curbuf_tail = fnamemodify(bufname(a:bufnr), ':t')
  let do_deduplicate = 0
  let path_tokens = {}

  for nr in a:buffers
    let name = bufname(nr)
    if !empty(name) && nr != a:bufnr && fnamemodify(name, ':t') == curbuf_tail " only perform actions if curbuf_tail isn't unique
      let do_deduplicate = 1
      let tokens = reverse(split(substitute(fnamemodify(name, ':p:h'), '\\', '/', 'g'), '/'))
      let token_index = 0
      for token in tokens
        if token == '' | continue | endif
        if token == '.' | break | endif
        if !has_key(path_tokens, token_index)
          let path_tokens[token_index] = {}
        endif
        let path_tokens[token_index][token] = 1
        let token_index += 1
      endfor
    endif
  endfor

  if do_deduplicate == 1
    let path = []
    let token_index = 0
    for token in reverse(split(substitute(fnamemodify(bufname(a:bufnr), ':p:h'), '\\', '/', 'g'), '/'))
      if token == '.' | break | endif
      let duplicated = 0
      let uniq = 1
      let single = 1
      if has_key(path_tokens, token_index)
        let duplicated = 1
        if len(keys(path_tokens[token_index])) > 1 | let single = 0 | endif
        if has_key(path_tokens[token_index], token) | let uniq = 0 | endif
      endif
      call insert(path, {'token': token, 'duplicated': duplicated, 'uniq': uniq, 'single': single})
      let token_index += 1
    endfor

    let buf_name = [curbuf_tail]
    let has_uniq = 0
    let has_skipped = 0
    for token1 in reverse(path)
      if !token1['duplicated'] && len(buf_name) > 1
        call insert(buf_name, s:skip_symbol)
        let has_skipped = 0
        break
      endif

      if has_uniq == 1
        call insert(buf_name, s:skip_symbol)
        let has_skipped = 0
        break
      endif

      if token1['uniq'] == 0 && token1['single'] == 1
        let has_skipped = 1
      else
        if has_skipped == 1
          call insert(buf_name, s:skip_symbol)
          let has_skipped = 0
        endif
        call insert(buf_name, token1['token'])
      endif

      if token1['uniq'] == 1
        let has_uniq = 1
      endif
    endfor

    if has_skipped == 1
      call insert(buf_name, s:skip_symbol)
    endif

    return airline#extensions#tabline#formatters#default#wrap_name(a:bufnr, join(buf_name, '/'))
  else
    return airline#extensions#tabline#formatters#default#format(a:bufnr, a:buffers)
  endif
endfunction
