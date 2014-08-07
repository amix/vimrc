" Vim auto-load script
" Author: Peter Odding <peter@peterodding.com>
" Last Change: May 5, 2013
" URL: http://peterodding.com/code/vim/notes/

if !exists('s:currently_tagged_notes')
  let s:currently_tagged_notes = {} " The in-memory representation of tags and the notes in which they're used.
  let s:previously_tagged_notes = {} " Copy of index as it is / should be now on disk (to detect changes).
  let s:last_disk_sync = 0 " Whether the on-disk representation of the tags has been read.
  let s:buffer_name = 'Tagged Notes' " The buffer name for the list of tagged notes.
  let s:loading_index = 0
endif

function! xolox#notes#tags#load_index() " {{{1
  if s:loading_index
    " Guard against recursive calls.
    return s:currently_tagged_notes
  endif
  let starttime = xolox#misc#timer#start()
  let indexfile = expand(g:notes_tagsindex)
  let lastmodified = getftime(indexfile)
  if lastmodified == -1
    let s:loading_index = 1
    call xolox#notes#tags#create_index()
    let s:loading_index = 0
  elseif lastmodified > s:last_disk_sync
    let s:currently_tagged_notes = {}
    for line in readfile(indexfile)
      let filenames = split(line, "\t")
      if len(filenames) > 1
        let tagname = remove(filenames, 0)
        let s:currently_tagged_notes[tagname] = filenames
      endif
    endfor
    let s:previously_tagged_notes = deepcopy(s:currently_tagged_notes)
    let s:last_disk_sync = lastmodified
    call xolox#misc#timer#stop("notes.vim %s: Loaded tags index in %s.", g:xolox#notes#version, starttime)
  endif
  return s:currently_tagged_notes
endfunction

function! xolox#notes#tags#create_index() " {{{1
  let exists = filereadable(expand(g:notes_tagsindex))
  let starttime = xolox#misc#timer#start()
  let filenames = xolox#notes#get_fnames(0)
  let s:currently_tagged_notes = {}
  for idx in range(len(filenames))
    if filereadable(filenames[idx])
      let title = xolox#notes#fname_to_title(filenames[idx])
      call xolox#misc#msg#info("notes.vim %s: Scanning note %i/%i: %s", g:xolox#notes#version, idx + 1, len(filenames), title)
      call xolox#notes#tags#scan_note(title, join(readfile(filenames[idx]), "\n"))
    endif
  endfor
  if xolox#notes#tags#save_index()
    let s:previously_tagged_notes = deepcopy(s:currently_tagged_notes)
    call xolox#misc#timer#stop('notes.vim %s: %s tags index in %s.', g:xolox#notes#version, exists ? "Updated" : "Created", starttime)
  else
    call xolox#misc#msg#warn("notes.vim %s: Failed to save tags index as %s!", g:xolox#notes#version, g:notes_tagsindex)
  endif
endfunction

function! xolox#notes#tags#save_index() " {{{1
  let indexfile = expand(g:notes_tagsindex)
  let existingfile = filereadable(indexfile)
  let nothingchanged = (s:currently_tagged_notes == s:previously_tagged_notes)
  if existingfile && nothingchanged
    call xolox#misc#msg#debug("notes.vim %s: Index not dirty so not saved.", g:xolox#notes#version)
    return 1 " Nothing to be done
  else
    let lines = []
    for [tagname, filenames] in items(s:currently_tagged_notes)
      call add(lines, join([tagname] + filenames, "\t"))
    endfor
    let status = writefile(lines, indexfile) == 0
    if status
      call xolox#misc#msg#debug("notes.vim %s: Index saved to %s.", g:xolox#notes#version, g:notes_tagsindex)
      let s:last_disk_sync = getftime(indexfile)
    else
      call xolox#misc#msg#debug("notes.vim %s: Failed to save index to %s.", g:xolox#notes#version, g:notes_tagsindex)
    endif
    return status
  endif
endfunction

function! xolox#notes#tags#scan_note(title, text) " {{{1
  " Add a note to the tags index.
  call xolox#notes#tags#load_index()
  " Don't scan tags inside code blocks.
  let text = substitute(a:text, '{{{\w\+\_.\{-}}}}', '', 'g')
  " Split everything on whitespace.
  for token in split(text)
    " Match words that start with @ and don't contain { (BibTeX entries).
    if token =~ '^@\w' && token !~ '{'
      " Strip any trailing punctuation.
      let token = substitute(token[1:], '[[:punct:]]*$', '', '')
      if token != ''
        if !has_key(s:currently_tagged_notes, token)
          let s:currently_tagged_notes[token] = [a:title]
        elseif index(s:currently_tagged_notes[token], a:title) == -1
          " Keep the tags sorted.
          call xolox#misc#list#binsert(s:currently_tagged_notes[token], a:title, 1)
        endif
      endif
    endif
  endfor
endfunction

function! xolox#notes#tags#forget_note(title) " {{{1
  " Remove a note from the tags index.
  call xolox#notes#tags#load_index()
  for tagname in keys(s:currently_tagged_notes)
    call filter(s:currently_tagged_notes[tagname], "v:val != a:title")
    if empty(s:currently_tagged_notes[tagname])
      unlet s:currently_tagged_notes[tagname]
    endif
  endfor
endfunction

function! xolox#notes#tags#show_tags(minsize) " {{{1
  " TODO Mappings to "zoom" in/out (show only big tags).
  let starttime = xolox#misc#timer#start()
  call xolox#notes#tags#load_index()
  let lines = [s:buffer_name, '']
  if empty(s:currently_tagged_notes)
    call add(lines, "You haven't used any tags yet!")
  else
    " Create a dictionary with note titles as keys.
    let unmatched = {}
    for title in xolox#notes#get_titles(0)
      let unmatched[title] = 1
    endfor
    let totalnotes = len(unmatched)
    " Group matching notes and remove them from the dictionary.
    let grouped_notes = []
    let numtags = 0
    for tagname in sort(keys(s:currently_tagged_notes), 1)
      let numnotes = len(s:currently_tagged_notes[tagname])
      if numnotes >= a:minsize
        let matched_notes = s:currently_tagged_notes[tagname]
        for title in matched_notes
          if has_key(unmatched, title)
            unlet unmatched[title]
          endif
        endfor
        call add(grouped_notes, {'name': tagname, 'notes': matched_notes})
        let numtags += 1
      endif
    endfor
    " Add a "fake tag" with all unmatched notes.
    if !empty(unmatched)
      call add(grouped_notes, {'name': "Unmatched notes", 'notes': keys(unmatched)})
    endif
    " Format the results as a note.
    let bullet = xolox#notes#get_bullet('*')
    for group in grouped_notes
      let tagname = group['name']
      let friendly_name = xolox#notes#tags#friendly_name(tagname)
      let numnotes = len(group['notes'])
      if numnotes >= a:minsize
        call extend(lines, ['', printf('# %s (%i note%s)', friendly_name, numnotes, numnotes == 1 ? '' : 's'), ''])
        for title in group['notes']
          let lastmodified = xolox#notes#friendly_date(getftime(xolox#notes#title_to_fname(title)))
          call add(lines, ' ' . bullet . ' ' . title . ' (last edited ' . lastmodified . ')')
        endfor
      endif
    endfor
    if a:minsize <= 1
      let message = printf("You've used %i %s in %i %s",
            \ numtags, numtags == 1 ? "tag" : "tags",
            \ totalnotes, totalnotes == 1 ? "note" : "notes")
    else
      let message = printf("There %s %i %s that %s been used at least %s times",
            \ numtags == 1 ? "is" : "are", numtags,
            \ numtags == 1 ? "tag" : "tags",
            \ numtags == 1 ? "has" : "have", a:minsize)
    endif
    let message .= ", " . (numtags == 1 ? "it's" : "they're")
    let message .= " listed below. Tags and notes are sorted alphabetically and after each note is the date when it was last modified."
    if !empty(unmatched)
      if a:minsize <= 1
        let message .= " At the bottom is a list of untagged notes."
      else
        let message .= " At the bottom is a list of unmatched notes."
      endif
    endif
    if numtags > 1 && !(&foldmethod == 'expr' && &foldenable)
      let message .= " You can enable text folding to get an overview of just the tag names and how many times they've been used."
    endif
    call insert(lines, message, 2)
  endif
  call xolox#misc#buffer#prepare(s:buffer_name)
  call setline(1, lines)
  call xolox#misc#buffer#lock()
  call xolox#notes#set_filetype()
  setlocal nospell wrap
  call xolox#misc#timer#stop('notes.vim %s: Generated [%s] in %s.', g:xolox#notes#version, s:buffer_name, starttime)
endfunction

function! xolox#notes#tags#friendly_name(tagname) " {{{1
  return substitute(a:tagname, '\(\U\)\(\u\)', '\1 \2', 'g')
endfunction
