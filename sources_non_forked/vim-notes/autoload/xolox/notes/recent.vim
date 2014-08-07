" Vim auto-load script
" Author: Peter Odding <peter@peterodding.com>
" Last Change: May 16, 2013
" URL: http://peterodding.com/code/vim/notes/

function! xolox#notes#recent#show(bang, title_filter) " {{{1
  call xolox#misc#msg#info("notes.vim %s: Generating overview of recent notes ..", g:xolox#notes#version)
  " Show generated note listing all notes by last modified time.
  let starttime = xolox#misc#timer#start()
  let bufname = '[Recent Notes]'
  " Prepare a buffer to hold the list of recent notes.
  call xolox#misc#buffer#prepare({
        \ 'name': bufname,
        \ 'path': xolox#misc#path#merge($HOME, bufname)})
  " Filter notes by pattern (argument)?
  let notes = []
  let title_filter = '\v' . a:title_filter
  for [fname, title] in items(xolox#notes#get_fnames_and_titles(0))
    if title =~? title_filter
      call add(notes, [getftime(fname), title])
    endif
  endfor
  " Start note with "You have N note(s) [matching filter]".
  let readme = "You have "
  if empty(notes)
    let readme .= "no notes"
  elseif len(notes) == 1
    let readme .= "one note"
  else
    let readme .= len(notes) . " notes"
  endif
  if a:title_filter != ''
    let quote_format = xolox#notes#unicode_enabled() ? '‘%s’' : "`%s'"
    let readme .= " matching " . printf(quote_format, a:title_filter)
  endif
  " Explain the sorting of the notes.
  if empty(notes)
    let readme .= "."
  elseif len(notes) == 1
    let readme .= ", it's listed below."
  else
    let readme .= ". They're listed below grouped by the day they were edited, starting with your most recently edited note."
  endif
  " Add the generated text to the buffer.
  call setline(1, ["Recent notes", "", readme])
  " Reformat the text in the buffer to auto-wrap.
  normal Ggqq
  " Sort, group and format the list of (matching) notes.
  let last_date = ''
  let list_item_format = xolox#notes#unicode_enabled() ? ' • %s' : ' * %s'
  call sort(notes)
  call reverse(notes)
  let lines = []
  for [ftime, title] in notes
    let date = xolox#notes#friendly_date(ftime)
    if date != last_date
      call add(lines, '')
      call add(lines, substitute(date, '^\w', '\u\0', '') . ':')
      let last_date = date
    endif
    call add(lines, printf(list_item_format, title))
  endfor
  " Add the formatted list of notes to the buffer.
  call setline(line('$') + 1, lines)
  " Load the notes file type.
  call xolox#notes#set_filetype()
  let &l:statusline = bufname
  " Change the status line
  " Lock the buffer contents.
  call xolox#misc#buffer#lock()
  " And we're done!
  call xolox#misc#timer#stop("notes.vim %s: Generated %s in %s.", g:xolox#notes#version, bufname, starttime)
endfunction

function! xolox#notes#recent#track() " {{{1
  let fname = expand('%:p')
  let indexfile = expand(g:notes_recentindex)
  call xolox#misc#msg#debug("notes.vim %s: Recording '%s' as most recent note in %s ..", g:xolox#notes#version, fname, indexfile)
  if writefile([fname], indexfile) == -1
    call xolox#misc#msg#warn("notes.vim %s: Failed to record most recent note in %s!", g:xolox#notes#version, indexfile)
  endif
endfunction

function! xolox#notes#recent#edit(bang) " {{{1
  " Edit the most recently edited (not necessarily changed) note.
  let indexfile = expand(g:notes_recentindex)
  call xolox#misc#msg#debug("notes.vim %s: Recalling most recent note from %s ..", g:xolox#notes#version, indexfile)
  try
    let fname = readfile(indexfile)[0]
    if empty(fname)
      throw "The index of recent notes is empty?!"
    endif
  catch
    call xolox#misc#msg#warn("notes.vim %s: Failed to recall most recent note from %s: %s", g:xolox#notes#version, indexfile, v:exception)
    return
  endtry
  call xolox#misc#msg#info("notes.vim %s: Editing most recent note '%s' ..", g:xolox#notes#version, fname)
  execute 'edit' . a:bang fnameescape(fname)
  call xolox#notes#set_filetype()
endfunction
