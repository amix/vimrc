" MIT License. Copyright (c) 2013-2016 Bailey Ling.
" vim: et ts=2 sts=2 sw=2

function! airline#extensions#wordcount#formatters#default#format()
  let words = string(s:wordcount())
  if empty(words)
    return
  endif
  let separator = s:get_decimal_group()
  if words > 999 && !empty(separator)
    " Format number according to locale, e.g. German: 1.245 or English: 1,245
    let a = join(reverse(split(words, '.\zs')),'')
    let a = substitute(a, '...', '&'.separator, 'g')
    let words = join(reverse(split(a, '.\zs')),'')
  endif
  return  words . " words" . g:airline_symbols.space . g:airline_right_alt_sep . g:airline_symbols.space
endfunction

function! s:wordcount()
  if exists("*wordcount")
    let l:mode = mode()
    if l:mode ==# 'v' || l:mode ==# 'V' || l:mode ==# 's' || l:mode ==# 'S'
      let l:visual_words = wordcount()['visual_words']
      if l:visual_words != ''
        return l:visual_words
      else
        return 0
      endif
    else
      return wordcount()['words']
    endif
  elseif mode() =~? 's'
    return
  else
    let old_status = v:statusmsg
    let position = getpos(".")
    exe "silent normal! g\<c-g>"
    let stat = v:statusmsg
    call setpos('.', position)
    let v:statusmsg = old_status

    let parts = split(stat)
    if len(parts) > 11
      return str2nr(parts[11])
    else
      return
    endif
  endif
endfunction

function! s:get_decimal_group()
  if match(v:lang, '\v\cC|en') > -1
    return ','
  elseif match(v:lang, '\v\cde|dk|fr|pt') > -1
    return '.'
  endif
  return ''
endfunction
