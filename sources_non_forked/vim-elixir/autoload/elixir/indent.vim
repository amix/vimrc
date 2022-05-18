if !exists("g:elixir_indent_max_lookbehind")
  let g:elixir_indent_max_lookbehind = 30
endif

" Return the effective value of 'shiftwidth'
function! s:sw()
  return &shiftwidth == 0 ? &tabstop : &shiftwidth
endfunction

function! elixir#indent#indent(lnum)
  let lnum = a:lnum
  let text = getline(lnum)
  let prev_nb_lnum = prevnonblank(lnum-1)
  let prev_nb_text = getline(prev_nb_lnum)

  call s:debug("==> Indenting line " . lnum)
  call s:debug("text = '" . text . "'")

  let [_, curs_lnum, curs_col, _] = getpos('.')
  call cursor(lnum, 0)

  let handlers = [
        \'inside_embedded_view',
        \'top_of_file',
        \'starts_with_string_continuation',
        \'following_trailing_binary_operator',
        \'starts_with_pipe',
        \'starts_with_binary_operator',
        \'inside_block',
        \'starts_with_end',
        \'inside_generic_block',
        \'follow_prev_nb'
        \]
  for handler in handlers
    call s:debug('testing handler elixir#indent#handle_'.handler)
    let context = {'lnum': lnum, 'text': text, 'first_nb_char_idx': match(text, '\w'), 'prev_nb_lnum': prev_nb_lnum, 'prev_nb_text': prev_nb_text}
    let indent = function('elixir#indent#handle_'.handler)(context)
    if indent == -2
      " Keep indent the same
      call s:debug('line '.lnum.': elixir#indent#handle_'.handler.' returned -2; returning indent of -1')
      call cursor(curs_lnum, curs_col)
      return -1
    elseif indent != -1
      call s:debug('line '.lnum.': elixir#indent#handle_'.handler.' returned '.indent)
      call cursor(curs_lnum, curs_col)
      return indent
    endif
  endfor

  call s:debug("defaulting")
  call cursor(curs_lnum, curs_col)
  return 0
endfunction

function! s:debug(str)
  if exists("g:elixir_indent_debug") && g:elixir_indent_debug
    echom a:str
  endif
endfunction

function! s:starts_with(context, expr)
  return s:_starts_with(a:context.text, a:expr, a:context.lnum)
endfunction

function! s:prev_starts_with(context, expr)
  return s:_starts_with(a:context.prev_nb_text, a:expr, a:context.prev_nb_lnum)
endfunction

function! s:in_embedded_view()
  let groups = map(synstack(line('.'), col('.')), "synIDattr(v:val, 'name')")
  for group in ['elixirPhoenixESigil', 'elixirLiveViewSigil', 'elixirSurfaceSigil']
    if index(groups, group) >= 0
      return 1
    endif
  endfor

  return 0
endfunction

" Returns 0 or 1 based on whether or not the text starts with the given
" expression and is not a string or comment
function! s:_starts_with(text, expr, lnum)
  let pos = match(a:text, '^\s*'.a:expr)
  if pos == -1
    return 0
  else
    " NOTE: @jbodah 2017-02-24: pos is the index of the match which is
    " zero-indexed. Add one to make it the column number
    if s:is_string_or_comment(a:lnum, pos + 1)
      return 0
    else
      return 1
    end
  end
endfunction

function! s:prev_ends_with(context, expr)
  return s:_ends_with(a:context.prev_nb_text, a:expr, a:context.prev_nb_lnum)
endfunction

" Returns 0 or 1 based on whether or not the text ends with the given
" expression and is not a string or comment
function! s:_ends_with(text, expr, lnum)
  let pos = match(a:text, a:expr.'\s*$')
  if pos == -1
    return 0
  else
    if s:is_string_or_comment(a:lnum, pos)
      return 0
    else
      return 1
    end
  end
endfunction

" Returns 0 or 1 based on whether or not the given line number and column
" number pair is a string or comment
function! s:is_string_or_comment(line, col)
  return s:syntax_name(a:line, a:col) =~ '\%(String\|Comment\|CharList\)'
endfunction

function! s:syntax_name(line, col)
  return synIDattr(synID(a:line, a:col, 1), "name")
endfunction

" Skip expression for searchpair. Returns 0 or 1 based on whether the value
" under the cursor is a string or comment
function! elixir#indent#searchpair_back_skip()
  " NOTE: @jbodah 2017-02-27: for some reason this function gets called with
  " and index that doesn't exist in the line sometimes. Detect and account for
  " that situation
  let curr_col = col('.')
  if getline('.')[curr_col-1] == ''
    let curr_col = curr_col-1
  endif
  return s:is_string_or_comment(line('.'), curr_col)
endfunction

" DRY up regex for keywords that 1) makes sure we only look at complete words
" and 2) ignores atoms
function! s:keyword(expr)
  return ':\@<!\<\C\%('.a:expr.'\)\>:\@!'
endfunction

" Start at the end of text and search backwards looking for a match. Also peek
" ahead if we get a match to make sure we get a complete match. This means
" that the result should be the position of the start of the right-most match
function! s:find_last_pos(lnum, text, match)
  let last = len(a:text) - 1
  let c = last

  while c >= 0
    let substr = strpart(a:text, c, last)
    let peek = strpart(a:text, c - 1, last)
    let ss_match = match(substr, a:match)
    if ss_match != -1
      let peek_match = match(peek, a:match)
      if peek_match == ss_match + 1
        let syng = synIDattr(synID(a:lnum, c + ss_match, 1), 'name')
        if syng !~ '\%(String\|Comment\|CharList\)'
          return c + ss_match
        end
      end
    end
    let c -= 1
  endwhile

  return -1
endfunction

function! elixir#indent#handle_inside_embedded_view(context)
  if !s:in_embedded_view()
    return -1
  endif

  " Multi-line Surface data delimiters
  let pair_lnum = searchpair('{{', '', '}}', 'bW', "line('.') == ".a:context.lnum." || s:is_string_or_comment(line('.'), col('.'))", max([0, a:context.lnum - g:elixir_indent_max_lookbehind]))
  if pair_lnum
    if a:context.text =~ '}}$'
      return indent(pair_lnum)
    elseif a:context.text =~ '}}*>$'
      return -1
    elseif s:prev_ends_with(a:context, '[\|%{')
      return indent(a:context.prev_nb_lnum) + s:sw()
    elseif a:context.prev_nb_text =~ ',$'
      return indent(a:context.prev_nb_lnum)
    else
      return indent(pair_lnum) + s:sw()
    endif
  endif

  " Multi-line opening tag -- >, />, or %> are on a different line that their opening <
  let pair_lnum = searchpair('^\s\+<.*[^>]$', '', '^[^<]*[/%}]\?>$', 'bW', "line('.') == ".a:context.lnum." || s:is_string_or_comment(line('.'), col('.'))", max([0, a:context.lnum - g:elixir_indent_max_lookbehind]))
  if pair_lnum
    if a:context.text =~ '^\s\+\%\(>\|\/>\|%>\|}}>\)$'
      call s:debug("current line is a lone >, />, or %>")
      return indent(pair_lnum)
    elseif a:context.text =~ '\%\(>\|\/>\|%>\|}}>\)$'
      call s:debug("current line ends in >, />, or %>")
      if s:prev_ends_with(a:context, ',')
        return indent(a:context.prev_nb_lnum)
      else
        return -1
      endif
    else
      call s:debug("in the body of a multi-line opening tag")
      return indent(pair_lnum) + s:sw()
    endif
  endif

  " Special cases
  if s:prev_ends_with(a:context, '^[^<]*do\s%>')
    call s:debug("prev line closes a multi-line do block")
    return indent(a:context.prev_nb_lnum)
  elseif a:context.prev_nb_text =~ 'do\s*%>$'
    call s:debug("prev line opens a do block")
    return indent(a:context.prev_nb_lnum) + s:sw()
  elseif a:context.text =~ '^\s\+<\/[a-zA-Z0-9\.\-_]\+>\|<% end %>'
    call s:debug("a single closing tag")
    if a:context.prev_nb_text =~ '^\s\+<[^%\/]*[^/]>.*<\/[a-zA-Z0-9\.\-_]\+>$'
      call s:debug("opening and closing tags are on the same line")
      return indent(a:context.prev_nb_lnum) - s:sw()
    elseif a:context.prev_nb_text =~ '^\s\+<[^%\/]*[^/]>\|\s\+>' 
      call s:debug("prev line is opening html tag or single >")
      return indent(a:context.prev_nb_lnum)
    elseif s:prev_ends_with(a:context, '^[^<]*\%\(do\s\)\@<!%>')
      call s:debug("prev line closes a multi-line eex tag")
      return indent(a:context.prev_nb_lnum) - 2 * s:sw()
    else
      return indent(a:context.prev_nb_lnum) - s:sw()
    endif
  elseif a:context.text =~ '^\s*<%\s*\%(end\|else\|catch\|rescue\)\>.*%>'
    call s:debug("eex middle or closing eex tag")
    return indent(a:context.prev_nb_lnum) - s:sw()
  elseif a:context.prev_nb_text =~ '\s*<\/\|<% end %>$'
    call s:debug("prev is closing tag")
    return indent(a:context.prev_nb_lnum)
  elseif a:context.prev_nb_text =~ '^\s\+<[^%\/]*[^/]>.*<\/[a-zA-Z0-9\.\-_]\+>$'
    call s:debug("opening and closing tags are on the same line")
    return indent(a:context.prev_nb_lnum)
  elseif s:prev_ends_with(a:context, '\s\+\/>')
    call s:debug("prev ends with a single \>")
    return indent(a:context.prev_nb_lnum)
  elseif s:prev_ends_with(a:context, '^[^<]*\/>')
    call s:debug("prev line is closing a multi-line self-closing tag")
    return indent(a:context.prev_nb_lnum) - s:sw()
  elseif s:prev_ends_with(a:context, '^\s\+<.*\/>')
    call s:debug("prev line is closing self-closing tag")
    return indent(a:context.prev_nb_lnum)
  elseif a:context.prev_nb_text =~ '^\s\+%\?>$'
    call s:debug("prev line is a single > or %>")
    return indent(a:context.prev_nb_lnum) + s:sw()
  endif

  " Simple HTML (ie, opening tag is not split across lines)
  let pair_lnum = searchpair('^\s\+<[^%\/].*[^\/>]>$', '', '^\s\+<\/\w\+>$', 'bW', "line('.') == ".a:context.lnum." || s:is_string_or_comment(line('.'), col('.'))", max([0, a:context.lnum - g:elixir_indent_max_lookbehind]))
  if pair_lnum
    call s:debug("simple HTML")
    if a:context.text =~ '^\s\+<\/\w\+>$'
      return indent(pair_lnum)
    else
      return indent(pair_lnum) + s:sw()
    endif
  endif

  return -1
endfunction

function! elixir#indent#handle_top_of_file(context)
  if a:context.prev_nb_lnum == 0
    return 0
  else
    return -1
  end
endfunction

function! elixir#indent#handle_starts_with_string_continuation(context)
  if s:syntax_name(a:context.lnum, a:context.first_nb_char_idx) =~ '\(String\|Comment\|CharList\)$'
    return -2
  else
    return -1
  end
endfunction

function! elixir#indent#handle_follow_prev_nb(context)
  return s:get_base_indent(a:context.prev_nb_lnum, a:context.prev_nb_text)
endfunction

" Given the line at `lnum`, returns the indent of the line that acts as the 'base indent'
" for this line. In particular it traverses backwards up things like pipelines
" to find the beginning of the expression
function! s:get_base_indent(lnum, text)
  let prev_nb_lnum = prevnonblank(a:lnum - 1)
  let prev_nb_text = getline(prev_nb_lnum)

  let binary_operator = '\%(=\|<>\|>>>\|<=\|||\|+\|\~\~\~\|-\|&&\|<<<\|/\|\^\^\^\|\*\)'
  let data_structure_close = '\%(\]\|}\|)\)'
  let pipe = '|>'

  if s:_starts_with(a:text, binary_operator, a:lnum)
    return s:get_base_indent(prev_nb_lnum, prev_nb_text)
  elseif s:_starts_with(a:text, pipe, a:lnum)
    return s:get_base_indent(prev_nb_lnum, prev_nb_text)
  elseif s:_ends_with(prev_nb_text, binary_operator, prev_nb_lnum)
    return s:get_base_indent(prev_nb_lnum, prev_nb_text)
  elseif s:_ends_with(a:text, data_structure_close, a:lnum)
    let data_structure_open = '\%(\[\|{\|(\)'
    let close_match_idx = match(a:text, data_structure_close . '\s*$')
    call cursor(a:lnum, close_match_idx + 1)
    let [open_match_lnum, open_match_col] = searchpairpos(data_structure_open, '', data_structure_close, 'bnW')
    let open_match_text = getline(open_match_lnum)
    return s:get_base_indent(open_match_lnum, open_match_text)
  else
    return indent(a:lnum)
  endif
endfunction

function! elixir#indent#handle_following_trailing_binary_operator(context)
  let binary_operator = '\%(=\|<>\|>>>\|<=\|||\|+\|\~\~\~\|-\|&&\|<<<\|/\|\^\^\^\|\*\)'

  if s:prev_ends_with(a:context, binary_operator)
    return indent(a:context.prev_nb_lnum) + s:sw()
  else
    return -1
  endif
endfunction

function! elixir#indent#handle_starts_with_pipe(context)
  if s:starts_with(a:context, '|>')
    let match_operator = '\%(!\|=\|<\|>\)\@<!=\%(=\|>\|\~\)\@!'
    let pos = s:find_last_pos(a:context.prev_nb_lnum, a:context.prev_nb_text, match_operator)
    if pos == -1
      return indent(a:context.prev_nb_lnum)
    else
      let next_word_pos = match(strpart(a:context.prev_nb_text, pos+1, len(a:context.prev_nb_text)-1), '\S')
      if next_word_pos == -1
        return indent(a:context.prev_nb_lnum) + s:sw()
      else
        return pos + 1 + next_word_pos
      end
    end
  else
    return -1
  endif
endfunction

function! elixir#indent#handle_starts_with_end(context)
  if s:starts_with(a:context, s:keyword('end'))
    let pair_lnum = searchpair(s:keyword('do\|fn'), '', s:keyword('end').'\zs', 'bnW', "line('.') == " . line('.') . " || elixir#indent#searchpair_back_skip()")
    return indent(pair_lnum)
  else
    return -1
  endif
endfunction

function! elixir#indent#handle_starts_with_binary_operator(context)
  let binary_operator = '\%(=\|<>\|>>>\|<=\|||\|+\|\~\~\~\|-\|&&\|<<<\|/\|\^\^\^\|\*\)'

  if s:starts_with(a:context, binary_operator)
    let match_operator = '\%(!\|=\|<\|>\)\@<!=\%(=\|>\|\~\)\@!'
    let pos = s:find_last_pos(a:context.prev_nb_lnum, a:context.prev_nb_text, match_operator)
    if pos == -1
      return indent(a:context.prev_nb_lnum)
    else
      let next_word_pos = match(strpart(a:context.prev_nb_text, pos+1, len(a:context.prev_nb_text)-1), '\S')
      if next_word_pos == -1
        return indent(a:context.prev_nb_lnum) + s:sw()
      else
        return pos + 1 + next_word_pos
      end
    end
  else
    return -1
  endif
endfunction

" To handle nested structures properly we need to find the innermost
" nested structure. For example, we might be in a function in a map in a
" function, etc... so we need to first figure out what the innermost structure
" is then forward execution to the proper handler
function! elixir#indent#handle_inside_block(context)
  let start_pattern = '\C\%(\<with\>\|\<if\>\|\<case\>\|\<cond\>\|\<try\>\|\<receive\>\|\<fn\>\|{\|\[\|(\)'
  let end_pattern = '\C\%(\<end\>\|\]\|}\|)\)'
  " hack - handle do: better
  let block_info = searchpairpos(start_pattern, '', end_pattern, 'bnW', "line('.') == " . line('.') . " || elixir#indent#searchpair_back_skip() || getline(line('.')) =~ 'do:'", max([0, a:context.lnum - g:elixir_indent_max_lookbehind]))
  let block_start_lnum = block_info[0]
  call s:debug("block_start_lnum=" . block_start_lnum)
  let block_start_col = block_info[1]
  if block_start_lnum != 0 || block_start_col != 0
    let block_text = getline(block_start_lnum)
    let block_start_char = block_text[block_start_col - 1]
    call s:debug("block_start_char=" . block_start_char)

    let never_match = ''
    let config = {
          \'f': {'aligned_clauses': s:keyword('end'), 'pattern_match_clauses': never_match},
          \'c': {'aligned_clauses': s:keyword('end'), 'pattern_match_clauses': never_match},
          \'t': {'aligned_clauses': s:keyword('end\|catch\|rescue\|after\|else'), 'pattern_match_clauses': s:keyword('catch\|rescue\|else')},
          \'r': {'aligned_clauses': s:keyword('end\|after'), 'pattern_match_clauses': s:keyword('after')},
          \'i': {'aligned_clauses': s:keyword('end\|else'), 'pattern_match_clauses': never_match},
          \'[': {'aligned_clauses': ']', 'pattern_match_clauses': never_match},
          \'{': {'aligned_clauses': '}', 'pattern_match_clauses': never_match},
          \'(': {'aligned_clauses': ')', 'pattern_match_clauses': never_match}
          \}

    " if `with` clause...
    if block_start_char == 'w'
      call s:debug("testing s:handle_with")
      return s:handle_with(block_start_lnum, block_start_col, a:context)
    else
      let block_config = config[block_start_char]
      " if aligned clause (closing tag/`else` clause/etc...) then indent this
      " at the same level as the block open tag (e.g. `if`/`case`/etc...)
      if s:starts_with(a:context, block_config.aligned_clauses)
        call s:debug("clause")
        return indent(block_start_lnum)
      else
        if block_config.pattern_match_clauses == never_match
          let relative_lnum = block_start_lnum
        else
          let clause_lnum = searchpair(block_config.pattern_match_clauses, '', '*', 'bnW', "line('.') == " . line('.') . " || elixir#indent#searchpair_back_skip()", block_start_lnum)
          call s:debug("clause_lum=" . clause_lnum)
          let relative_lnum = max([clause_lnum, block_start_lnum])
        end
        call s:debug("pattern matching relative to lnum " . relative_lnum)
        return s:do_handle_pattern_match_block(relative_lnum, a:context)
      endif
    end
  else
    return -1
  end
endfunction

function! s:handle_with(start_lnum, start_col, context)
  let block_info = searchpairpos('\C\%(\<with\>\|\<do\>\|\<else\>\)', '', s:keyword('end'), 'bnW', "line('.') == " . line('.') . " || elixir#indent#searchpair_back_skip()")
  let block_start_lnum = block_info[0]
  let block_start_col = block_info[1]

  let block_start_text = getline(block_start_lnum)
  let block_start_char = block_start_text[block_start_col - 1]

  if s:starts_with(a:context, s:keyword('do\|else\|end'))
    return indent(a:start_lnum)
  elseif block_start_char == 'w' || s:starts_with(a:context, '\C\(do\|else\):')
    return indent(a:start_lnum) + 5
  elseif s:_starts_with(block_start_text, '\C\(do\|else\):', a:start_lnum)
    return indent(block_start_lnum) + s:sw()
  else
    return s:do_handle_pattern_match_block(a:start_lnum, a:context)
  end
endfunction

function! s:do_handle_pattern_match_block(relative_line, context)
  let relative_indent = indent(a:relative_line)
  " hack!
  if a:context.text =~ '\(fn.*\)\@<!->'
    call s:debug("current line contains ->; assuming match definition")
    return relative_indent + s:sw()
  elseif search('\(fn.*\)\@<!->', 'bnW', a:relative_line) != 0
    call s:debug("a previous line contains ->; assuming match handler")
    return relative_indent + 2 * s:sw()
  else
    call s:debug("couldn't find any previous ->; assuming body text")
    return relative_indent + s:sw()
  end
endfunction

function! elixir#indent#handle_inside_generic_block(context)
  let pair_lnum = searchpair(s:keyword('do\|fn'), '', s:keyword('end'), 'bW', "line('.') == ".a:context.lnum." || s:is_string_or_comment(line('.'), col('.'))", max([0, a:context.lnum - g:elixir_indent_max_lookbehind]))
  if pair_lnum
    " TODO: @jbodah 2017-03-29: this should probably be the case in *all*
    " blocks
    if s:prev_ends_with(a:context, ',')
      return indent(pair_lnum) + 2 * s:sw()
    else
      return indent(pair_lnum) + s:sw()
    endif
  else
    return -1
  endif
endfunction
