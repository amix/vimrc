" Vim auto-load script
" Author: Peter Odding <peter@peterodding.com>
" Last Change: July 18, 2013
" URL: http://peterodding.com/code/vim/notes/

function! xolox#notes#parser#parse_note(text) " {{{1
  " Parser for the note taking syntax used by vim-notes.
  let starttime = xolox#misc#timer#start()
  let context = s:create_parse_context(a:text)
  let note_title = context.next_line()
  let blocks = [{'type': 'title', 'text': note_title}]
  while context.has_more()
    let chr = context.peek(1)
    if chr == "\n"
      " Ignore empty lines.
      call context.next(1)
      continue
    elseif chr == '#'
      let block = s:parse_heading(context)
    elseif chr == '>'
      let block = s:parse_block_quote(context)
    elseif chr == '{' && context.peek(3) == "\{\{\{"
      let block = s:parse_code_block(context)
    else
      let lookahead = s:match_bullet_or_divider(context, 0)
      if !empty(lookahead)
        if lookahead.type =~ 'list'
          let block = s:parse_list(context)
        elseif lookahead.type == 'divider'
          let block = s:parse_divider(context)
        else
          let msg = "Programming error! Unsupported lookahead: %s."
          throw printf(msg, string(lookahead))
        endif
      else
        let block = s:parse_paragraph(context)
      endif
    endif
    " Don't include empty blocks in the output.
    if !empty(block)
      call add(blocks, block)
    endif
  endwhile
  call xolox#misc#timer#stop("notes.vim %s: Parsed note into %i blocks in %s.", g:xolox#notes#version, len(blocks), starttime)
  return blocks
endfunction

function! xolox#notes#parser#view_parse_nodes() " {{{1
  " Parse the current note and show the parse nodes in a temporary buffer.
  let note_text = join(getline(1, '$'), "\n")
  let parse_nodes = xolox#notes#parser#parse_note(note_text)
  vnew
  call setline(1, map(parse_nodes, 'string(v:val)'))
  setlocal filetype=vim nomodified nowrap
endfunction

function! s:create_parse_context(text) " {{{1
  " Create an object to encapsulate the lowest level of parser state.
  let context = {'text': a:text, 'index': 0}
  " The has_more() method returns 1 (true) when more input is available, 0
  " (false) otherwise.
  function context.has_more()
    return self.index < len(self.text)
  endfunction
  " The peek() method returns the next character without consuming it.
  function context.peek(n)
    if self.has_more()
      return self.text[self.index : self.index + (a:n - 1)]
    endif
    return ''
  endfunction
  " The next() method returns the next character and consumes it.
  function context.next(n)
    let result = self.peek(a:n)
    let self.index += a:n
    return result
  endfunction
  " The next_line() method returns the current line and consumes it.
  function context.next_line()
    let line = ''
    while self.has_more()
      let chr = self.next(1)
      if chr == "\n" || chr == ""
        " We hit the end of line or input.
        return line
      else
        " The line continues.
        let line .= chr
      endif
    endwhile
    return line
  endfunction
  return context
endfunction

function! s:match_bullet_or_divider(context, consume_lookahead) " {{{1
  " Check whether the current line starts with a list bullet.
  let result = {}
  let context = copy(a:context)
  let line = context.next_line()
  let bullet = matchstr(line, s:bullet_pattern)
  if !empty(bullet)
    " Disambiguate list bullets from horizontal dividers.
    if line =~ '^\s\+\*\s\*\s\*$'
      let result.type = 'divider'
    else
      " We matched a bullet! Now we still need to distinguish ordered from
      " unordered list items.
      if bullet =~ '\d'
        let result.type = 'ordered-list'
      else
        let result.type = 'unordered-list'
      endif
      let indent = matchstr(bullet, '^\s*')
      let result.indent = len(indent)
      " Since we already skipped the whitespace and matched the bullet, it's
      " very little work to mark our position for the benefit of the caller.
      if a:consume_lookahead
        let a:context.index += len(bullet)
      endif
    endif
  endif
  return result
endfunction

function! s:match_line(context) " {{{1
  " Get the text of the current line, stopping at end of the line or just
  " before the start of a code block marker, whichever comes first.
  let line = ''
  while a:context.has_more()
    let chr = a:context.peek(1)
    if chr == '{' && a:context.peek(3) == "\{\{\{"
      " XXX The start of a code block implies the end of whatever came before.
      " The marker above contains back slashes so that Vim doesn't apply
      " folding because of the marker :-).
      return line
    elseif chr == "\n"
      call a:context.next(1)
      return line . "\n"
    else
      let line .= a:context.next(1)
    endif
  endwhile
  " We hit the end of the input.
  return line
endfunction

function! s:parse_heading(context) " {{{1
  " Parse the upcoming heading in the input stream.
  let level = 0
  while a:context.peek(1) == '#'
    let level += 1
    call a:context.next(1)
  endwhile
  let text = xolox#misc#str#trim(s:match_line(a:context))
  return {'type': 'heading', 'level': level, 'text': text}
endfunction

function! s:parse_block_quote(context) " {{{1
  " Parse the upcoming block quote in the input stream.
  let lines = []
  while a:context.has_more()
    if a:context.peek(1) != '>'
      break
    endif
    let line = s:match_line(a:context)
    let level = len(matchstr(line, '^>\+'))
    let text = matchstr(line, '^>\+\s*\zs.\{-}\ze\_s*$')
    call add(lines, {'level': level, 'text': text})
  endwhile
  return {'type': 'block-quote', 'lines': lines}
endfunction

function! s:parse_code_block(context) " {{{1
  " Parse the upcoming code block in the input stream.
  let language = ''
  let text = ''
  " Skip the start marker.
  call a:context.next(3)
  " Get the optional language name.
  while a:context.peek(1) =~ '\w'
    let language .= a:context.next(1)
  endwhile
  " Skip the whitespace separating the start marker and/or language name from
  " the text.
  while a:context.peek(1) =~ '[ \t]'
    call a:context.next(1)
  endwhile
  " Get the text inside the code block.
  while a:context.has_more()
    let chr = a:context.next(1)
    if chr == '}' && a:context.peek(2) == '}}'
      call a:context.next(2)
      break
    endif
    let text .= chr
  endwhile
  " Strip trailing whitespace.
  let text = substitute(text, '\_s\+$', '', '')
  return {'type': 'code', 'language': language, 'text': text}
endfunction

function! s:parse_divider(context) " {{{1
  " Parse the upcoming horizontal divider in the input stream.
  call a:context.next_line()
  return {'type': 'divider'}
endfunction

function! s:parse_list(context) " {{{1
  " Parse the upcoming sequence of list items in the input stream.
  let list_type = 'unknown'
  let items = []
  let lines = []
  let indent = 0
  " Outer loop to consume one or more list items.
  while a:context.has_more()
    let lookahead = s:match_bullet_or_divider(a:context, 1)
    if !empty(lookahead)
      " Save the previous list item with the old indent level.
      call s:save_item(items, lines, indent)
      let lines = []
      " Set the new indent level (three spaces -> one level).
      let indent = lookahead.indent / 3
      " The current line starts with a list bullet.
      if list_type == 'unknown'
        " The first bullet determines the type of list.
        let list_type = lookahead.type
      endif
    endif
    let line = s:match_line(a:context)
    call add(lines, line)
    if line[-1:] != "\n"
      " XXX When match_line() returns a line that doesn't end in a newline
      " character, it means either we hit the end of the input or the current
      " line continues in a code block (which is not ours to parse :-).
      break
    elseif line =~ '^\_s*$'
      " For now an empty line terminates the list item.
      " TODO Add support for list items with multiple paragraphs of text.
      break
    endif
  endwhile
  call s:save_item(items, lines, indent)
  return {'type': 'list', 'ordered': (list_type == 'ordered-list'), 'items': items}
endfunction

function! s:save_item(items, lines, indent)
  let text = join(a:lines, "\n")
  if text =~ '\S'
    let text = xolox#misc#str#compact(text)
    call add(a:items, {'text': text, 'indent': a:indent})
  endif
endfunction

function! s:parse_paragraph(context) " {{{1
  " Parse the upcoming paragraph in the input stream.
  let lines = []
  while a:context.has_more()
    if !empty(s:match_bullet_or_divider(a:context, 0))
      " If the next line starts with a list bullet it shouldn't
      " be included in the paragraph we're currently parsing.
      break
    else
      let line = s:match_line(a:context)
      call add(lines, line)
      if line =~ '^\_s*$'
        " An empty line marks the end of the paragraph.
        break
      elseif line[-1:] != "\n"
        " XXX When match_line() returns a line that doesn't end in a newline
        " character, it means either we hit the end of the input or the current
        " line continues in a code block (which is not ours to parse :-).
        break
      endif
    endif
  endwhile
  " Don't include empty paragraphs in the output.
  let text = join(lines, "\n")
  if text =~ '\S'
    return {'type': 'paragraph', 'text': xolox#misc#str#compact(text)}
  else
    return {}
  endif
endfunction

function! s:generate_list_item_bullet_pattern() " {{{1
  " Generate a regular expression that matches any kind of list bullet.
  let choices = copy(g:notes_unicode_bullets)
  for bullet in g:notes_ascii_bullets
    call add(choices, xolox#misc#escape#pattern(bullet))
  endfor
  call add(choices, '\d\+[[:punct:]]\?')
  return join(choices, '\|')
endfunction

let s:bullet_pattern = '^\s*\(' . s:generate_list_item_bullet_pattern() . '\)\s\+'

function! xolox#notes#parser#run_tests() " {{{1
  " Tests for the note taking syntax parser.
  call xolox#misc#test#reset()
  call xolox#misc#test#wrap('xolox#notes#parser#test_parsing_of_note_titles')
  call xolox#misc#test#wrap('xolox#notes#parser#test_parsing_of_headings')
  call xolox#misc#test#wrap('xolox#notes#parser#test_parsing_of_paragraphs')
  call xolox#misc#test#wrap('xolox#notes#parser#test_parsing_of_code_blocks')
  call xolox#misc#test#wrap('xolox#notes#parser#test_parsing_of_list_items')
  call xolox#misc#test#wrap('xolox#notes#parser#test_parsing_of_block_quotes')
  call xolox#misc#test#summarize()
endfunction

function! xolox#notes#parser#test_parsing_of_note_titles()
  call xolox#misc#test#assert_equals([{'type': 'title', 'text': 'Just the title'}], xolox#notes#parser#parse_note('Just the title'))
endfunction

function! xolox#notes#parser#test_parsing_of_headings()
  call xolox#misc#test#assert_equals([{'type': 'title', 'text': 'Just the title'}, {'type': 'heading', 'level': 1, 'text': 'This is a heading'}], xolox#notes#parser#parse_note("Just the title\n\n# This is a heading"))
endfunction

function! xolox#notes#parser#test_parsing_of_paragraphs()
  call xolox#misc#test#assert_equals([{'type': 'title', 'text': 'Just the title'}, {'type': 'paragraph', 'text': 'This is a paragraph'}], xolox#notes#parser#parse_note("Just the title\n\nThis is a paragraph"))
  call xolox#misc#test#assert_equals([{'type': 'title', 'text': 'Just the title'}, {'type': 'paragraph', 'text': 'This is a paragraph'}, {'type': 'paragraph', 'text': "And here's another paragraph!"}], xolox#notes#parser#parse_note("Just the title\n\nThis is a paragraph\n\n\n\nAnd here's another paragraph!"))
endfunction

function! xolox#notes#parser#test_parsing_of_code_blocks()
  call xolox#misc#test#assert_equals([{'type': 'title', 'text': 'Just the title'}, {'type': 'code', 'language': '', 'text': "This is a code block\nwith two lines"}], xolox#notes#parser#parse_note("Just the title\n\n{{{ This is a code block\nwith two lines }}}"))
endfunction

function! xolox#notes#parser#test_parsing_of_list_items()
  call xolox#misc#test#assert_equals([{'type': 'title', 'text': 'Just the title'}, {'type': 'list', 'ordered': 1, 'items': [{'indent': 0, 'text': 'item one'}, {'indent': 0, 'text': 'item two'}, {'indent': 0, 'text': 'item three'}]}], xolox#notes#parser#parse_note("Just the title\n\n1. item one\n2. item two\n3. item three"))
endfunction

function! xolox#notes#parser#test_parsing_of_block_quotes()
  call xolox#misc#test#assert_equals([{'type': 'title', 'text': 'Just the title'}, {'type': 'block-quote', 'lines': [{'level': 1, 'text': 'block'}, {'level': 2, 'text': 'quoted'}, {'level': 1, 'text': 'text'}]}], xolox#notes#parser#parse_note("Just the title\n\n> block\n>> quoted\n> text"))
endfunction
