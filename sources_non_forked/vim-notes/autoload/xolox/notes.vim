" Vim auto-load script
" Author: Peter Odding <peter@peterodding.com>
" Last Change: September 2, 2013
" URL: http://peterodding.com/code/vim/notes/

" Note: This file is encoded in UTF-8 including a byte order mark so
" that Vim loads the script using the right encoding transparently.

let g:xolox#notes#version = '0.23.4'
let g:xolox#notes#url_pattern = '\<\(mailto:\|javascript:\|\w\{3,}://\)\(\S*\w\)\+/\?'
let s:scriptdir = expand('<sfile>:p:h')

function! xolox#notes#init() " {{{1
  " Initialize the configuration of the notes plug-in. This is a bit tricky:
  " We want to be compatible with Pathogen which installs plug-ins as
  " "bundles" under ~/.vim/bundle/*/ so we use a relative path to make sure we
  " 'stay inside the bundle'. However if the notes.vim plug-in is installed
  " system wide the user probably won't have permission to write inside the
  " installation directory, so we have to switch to $HOME then.
  let systemdir = xolox#misc#path#absolute(s:scriptdir . '/../../misc/notes')
  if filewritable(systemdir) == 2
    let localdir = systemdir
  elseif xolox#misc#os#is_win()
    let localdir = xolox#misc#path#absolute('~/vimfiles/misc/notes')
  else
    let localdir = xolox#misc#path#absolute('~/.vim/misc/notes')
  endif
  " Backwards compatibility with old configurations.
  if exists('g:notes_directory')
    call xolox#misc#msg#warn("notes.vim %s: Please upgrade your configuration, see :help notes-backwards-compatibility", g:xolox#notes#version)
    let g:notes_directories = [g:notes_directory]
    unlet g:notes_directory
  endif
  " Define the default location where the user's notes are saved?
  if !exists('g:notes_directories')
    let g:notes_directories = [xolox#misc#path#merge(localdir, 'user')]
  endif
  call s:create_notes_directories()
  " Define the default location of the shadow directory with predefined notes?
  if !exists('g:notes_shadowdir')
    let g:notes_shadowdir = xolox#misc#path#merge(systemdir, 'shadow')
  endif
  " Define the default location for the full text index.
  if !exists('g:notes_indexfile')
    let g:notes_indexfile = xolox#misc#path#merge(localdir, 'index.pickle')
  endif
  " Define the default location for the keyword scanner script.
  if !exists('g:notes_indexscript')
    let g:notes_indexscript = xolox#misc#path#merge(systemdir, 'search-notes.py')
  endif
  " Define the default suffix for note filenames.
  if !exists('g:notes_suffix')
    let g:notes_suffix = ''
  endif
  " Define the default location for the tag name index (used for completion).
  if !exists('g:notes_tagsindex')
    let g:notes_tagsindex = xolox#misc#path#merge(localdir, 'tags.txt')
  endif
  " Define the default location for the file containing the most recent note's
  " filename.
  if !exists('g:notes_recentindex')
    let g:notes_recentindex = xolox#misc#path#merge(localdir, 'recent.txt')
  endif
  " Define the default location of the template for HTML conversion.
  if !exists('g:notes_html_template')
    let g:notes_html_template = xolox#misc#path#merge(localdir, 'template.html')
  endif
  " Define the default action when a note's filename and title are out of sync.
  if !exists('g:notes_title_sync')
    " Valid values are "no", "change_title", "rename_file" and "prompt".
    let g:notes_title_sync = 'prompt'
  endif
  " Smart quotes and such are enabled by default.
  if !exists('g:notes_smart_quotes')
    let g:notes_smart_quotes = 1
  endif
  " Tab/Shift-Tab is used to indent/dedent list items by default.
  if !exists('g:notes_tab_indents')
    let g:notes_tab_indents = 1
  endif
  " Alt-Left/Alt-Right is used to indent/dedent list items by default.
  if !exists('g:notes_alt_indents')
    let g:notes_alt_indents = 1
  endif
  " Text used for horizontal rulers.
  if !exists('g:notes_ruler_text')
    let g:notes_ruler_text = repeat(' ', ((&tw > 0 ? &tw : 79) - 5) / 2) . '* * *'
  endif
  " Symbols used to denote list items with increasing nesting levels.
  let g:notes_unicode_bullets = ['•', '◦', '▸', '▹', '▪', '▫']
  let g:notes_ascii_bullets = ['*', '-', '+']
  if !exists('g:notes_list_bullets')
    if xolox#notes#unicode_enabled()
      let g:notes_list_bullets = g:notes_unicode_bullets
    else
      let g:notes_list_bullets = g:notes_ascii_bullets
    endif
  endif
endfunction

function! s:create_notes_directories()
  for directory in xolox#notes#find_directories(0)
    if !isdirectory(directory)
      call xolox#misc#msg#info("notes.vim %s: Creating notes directory %s (first run?) ..", g:xolox#notes#version, directory)
      call mkdir(directory, 'p')
    endif
    if filewritable(directory) != 2
      call xolox#misc#msg#warn("notes.vim %s: The notes directory %s is not writable!", g:xolox#notes#version, directory)
    endif
  endfor
endfunction

function! xolox#notes#shortcut() " {{{1
  " The "note:" pseudo protocol is just a shortcut for the :Note command.
  let expression = expand('<afile>')
  let bufnr_save = bufnr('%')
  call xolox#misc#msg#debug("notes.vim %s: Expanding shortcut %s ..", g:xolox#notes#version, string(expression))
  let substring = matchstr(expression, 'note:\zs.*')
  call xolox#misc#msg#debug("notes.vim %s: Editing note based on title substring %s ..", g:xolox#notes#version, string(substring))
  call xolox#notes#edit(v:cmdbang ? '!' : '', substring)
  " Clean up the buffer with the name "note:..."?
  let pathname = fnamemodify(bufname(bufnr_save), ':p')
  let basename = fnamemodify(pathname, ':t')
  if basename =~ '^note:'
    call xolox#misc#msg#debug("notes.vim %s: Cleaning up buffer #%i - %s", g:xolox#notes#version, bufnr_save, pathname)
    execute 'bwipeout' bufnr_save
  endif
endfunction

function! xolox#notes#edit(bang, title) abort " {{{1
  " Edit an existing note or create a new one with the :Note command.
  let starttime = xolox#misc#timer#start()
  let title = xolox#misc#str#trim(a:title)
  if title != ''
    let fname = xolox#notes#select(title)
    if fname != ''
      call xolox#misc#msg#debug("notes.vim %s: Editing existing note: %s", g:xolox#notes#version, fname)
      execute 'edit' . a:bang fnameescape(fname)
      if !xolox#notes#unicode_enabled() && xolox#misc#path#equals(fnamemodify(fname, ':h'), g:notes_shadowdir)
        call s:transcode_utf8_latin1()
      endif
      call xolox#notes#set_filetype()
      call xolox#misc#timer#stop('notes.vim %s: Opened note in %s.', g:xolox#notes#version, starttime)
      return
    endif
  else
    let title = 'New note'
  endif
  " At this point we're dealing with a new note.
  let fname = xolox#notes#title_to_fname(title)
  noautocmd execute 'edit' . a:bang fnameescape(fname)
  if line('$') == 1 && getline(1) == ''
    let fname = xolox#misc#path#merge(g:notes_shadowdir, 'New note')
    execute 'silent read' fnameescape(fname)
    1delete
    if !xolox#notes#unicode_enabled()
      call s:transcode_utf8_latin1()
    endif
    setlocal nomodified
  endif
  if title != 'New note'
    call setline(1, title)
  endif
  call xolox#notes#set_filetype()
  doautocmd BufReadPost
  call xolox#misc#timer#stop('notes.vim %s: Started new note in %s.', g:xolox#notes#version, starttime)
endfunction

function! xolox#notes#check_sync_title() " {{{1
  " Check if the note's title and filename are out of sync.
  if g:notes_title_sync != 'no' && xolox#notes#buffer_is_note() && &buftype == ''
    let title = xolox#notes#current_title()
    let name_on_disk = xolox#misc#path#absolute(expand('%:p'))
    let name_from_title = xolox#notes#title_to_fname(title)
    if !xolox#misc#path#equals(name_on_disk, name_from_title)
      call xolox#misc#msg#debug("notes.vim %s: Filename (%s) doesn't match note title (%s)", g:xolox#notes#version, name_on_disk, name_from_title)
      let action = g:notes_title_sync
      if action == 'prompt' && empty(name_from_title)
        " There's no point in prompting the user when there's only one choice.
        let action = 'change_title'
      elseif action == 'prompt'
        " Prompt the user what to do (if anything). First we perform a redraw
        " to make sure the note's content is visible (without this the Vim
        " window would be blank in my tests).
        redraw
        let message = "The note's title and filename do not correspond. What do you want to do?\n\n"
        let message .= "Current filename: " . s:sync_value(name_on_disk) . "\n"
        let message .= "Corresponding title: " . s:sync_value(xolox#notes#fname_to_title(name_on_disk)) . "\n\n"
        let message .= "Current title: " . s:sync_value(title) . "\n"
        let message .= "Corresponding filename: " . s:sync_value(xolox#notes#title_to_fname(title))
        let choice = confirm(message, "Change &title\nRename &file\nDo &nothing", 3, 'Question')
        if choice == 1
          let action = 'change_title'
        elseif choice == 2
          let action = 'rename_file'
        else
          " User chose to do nothing or <Escape>'d the prompt.
          return
        endif
        " Intentional fall through here :-)
      endif
      if action == 'change_title'
        let new_title = xolox#notes#fname_to_title(name_on_disk)
        call setline(1, new_title)
        setlocal modified
        call xolox#misc#msg#info("notes.vim %s: Changed note title to match filename.", g:xolox#notes#version)
      elseif action == 'rename_file'
        let new_fname = xolox#notes#title_to_fname(xolox#notes#current_title())
        if rename(name_on_disk, new_fname) == 0
          execute 'edit' fnameescape(new_fname)
          call xolox#notes#set_filetype()
          call xolox#misc#msg#info("notes.vim %s: Renamed file to match note title.", g:xolox#notes#version)
        else
          call xolox#misc#msg#warn("notes.vim %s: Failed to rename file to match note title?!", g:xolox#notes#version)
        endif
      endif
    endif
  endif
endfunction

function! s:sync_value(s)
  let s = xolox#misc#str#trim(a:s)
  return empty(s) ? '(none)' : s
endfunction

function! xolox#notes#from_selection(bang, cmd) " {{{1
  " Edit a note with the visually selected text as title.
  let selection = s:get_visual_selection()
  if a:cmd != 'edit' | execute a:cmd | endif
  call xolox#notes#edit(a:bang, selection)
endfunction

function! s:get_visual_selection()
  " Why is this not a built-in Vim script function?! See also the question at
  " http://stackoverflow.com/questions/1533565 but note that none of the code
  " posted there worked for me so I wrote this function.
  let [lnum1, col1] = getpos("'<")[1:2]
  let [lnum2, col2] = getpos("'>")[1:2]
  let lines = getline(lnum1, lnum2)
  let lines[-1] = lines[-1][: col2 - (&selection == 'inclusive' ? 1 : 2)]
  let lines[0] = lines[0][col1 - 1:]
  return join(lines, ' ')
endfunction

function! xolox#notes#edit_shadow() " {{{1
  " People using latin1 don't like the UTF-8 curly quotes and bullets used in
  " the predefined notes because there are no equivalent characters in latin1,
  " resulting in the characters being shown as garbage or a question mark.
  execute 'edit' fnameescape(expand('<amatch>'))
  if !xolox#notes#unicode_enabled()
    call s:transcode_utf8_latin1()
  endif
  call xolox#notes#set_filetype()
endfunction

function! xolox#notes#unicode_enabled()
  return &encoding == 'utf-8'
endfunction

function! s:transcode_utf8_latin1()
  let view = winsaveview()
  silent %s/\%xe2\%x80\%x98/`/eg
  silent %s/\%xe2\%x80\%x99/'/eg
  silent %s/\%xe2\%x80[\x9c\x9d]/"/eg
  silent %s/\%xe2\%x80\%xa2/\*/eg
  setlocal nomodified
  call winrestview(view)
endfunction

function! xolox#notes#select(filter) " {{{1
  " Interactively select an existing note whose title contains {filter}.
  let notes = {}
  let filter = xolox#misc#str#trim(a:filter)
  for [fname, title] in items(xolox#notes#get_fnames_and_titles(1))
    if title ==? filter
      call xolox#misc#msg#debug("notes.vim %s: Filter %s exactly matches note: %s", g:xolox#notes#version, string(filter), title)
      return fname
    elseif title =~? filter
      let notes[fname] = title
    endif
  endfor
  if len(notes) == 1
    let fname = keys(notes)[0]
    call xolox#misc#msg#debug("notes.vim %s: Filter %s matched one note: %s", g:xolox#notes#version, string(filter), fname)
    return fname
  elseif !empty(notes)
    call xolox#misc#msg#debug("notes.vim %s: Filter %s matched %i notes.", g:xolox#notes#version, string(filter), len(notes))
    let choices = ['Please select a note:']
    let values = ['']
    for fname in sort(keys(notes), 1)
      call add(choices, ' ' . len(choices) . ') ' . notes[fname])
      call add(values, fname)
    endfor
    let choice = inputlist(choices)
    if choice > 0 && choice < len(choices)
      let fname = values[choice]
      call xolox#misc#msg#debug("notes.vim %s: User selected note: %s", g:xolox#notes#version, fname)
      return fname
    endif
  endif
  return ''
endfunction

function! xolox#notes#cmd_complete(arglead, cmdline, cursorpos) " {{{1
  " Vim's support for custom command completion is a real mess, specifically
  " the completion of multi word command arguments. With or without escaping
  " of spaces, arglead will only contain the last word in the arguments passed
  " to :Note, and worse, the completion candidates we return only replace the
  " last word on the command line.
  " XXX This isn't a real command line parser; it will break on quoted pipes.
  let cmdline = split(a:cmdline, '\\\@<!|')
  let cmdargs = substitute(cmdline[-1], '^\s*\w\+\s\+', '', '')
  let arguments = split(cmdargs)
  let titles = xolox#notes#get_titles(1)
  if a:arglead != '' && len(arguments) == 1
    " If we are completing a single argument and we are able to replace it
    " (the user didn't type <Space><Tab> after the argument) we can select the
    " completion candidates using a substring match on the first argument
    " instead of a prefix match (I consider this to be more user friendly).
    let pattern = xolox#misc#escape#pattern(cmdargs)
    call filter(titles, "v:val =~ pattern")
  else
    " If we are completing more than one argument or the user has typed
    " <Space><Tab> after the first argument, we must select completion
    " candidates using a prefix match on all arguments because Vim doesn't
    " support replacing previous arguments (selecting completion candidates
    " using a substring match would result in invalid note titles).
    let pattern = '^' . xolox#misc#escape#pattern(cmdargs)
    call filter(titles, "v:val =~ pattern")
    " Remove the given arguments as the prefix of every completion candidate
    " because Vim refuses to replace previous arguments.
    let prevargs = '^' . xolox#misc#escape#pattern(cmdargs[0 : len(cmdargs) - len(a:arglead) - 1])
    call map(titles, 'substitute(v:val, prevargs, "", "")')
  endif
  " Sort from shortest to longest as a rough approximation of
  " sorting by similarity to the word that's being completed.
  return reverse(sort(titles, 's:sort_longest_to_shortest'))
endfunction

function! xolox#notes#user_complete(findstart, base) " {{{1
  " Completion of note titles with Control-X Control-U.
  if a:findstart
    let line = getline('.')[0 : col('.') - 2]
    let words = split(line)
    if !empty(words)
      return col('.') - len(words[-1]) - 1
    else
      return -1
    endif
  else
    let titles = xolox#notes#get_titles(1)
    if !empty(a:base)
      let pattern = xolox#misc#escape#pattern(a:base)
      call filter(titles, 'v:val =~ pattern')
    endif
    return titles
  endif
endfunction

function! xolox#notes#omni_complete(findstart, base) " {{{1
  " Completion of tag names with Control-X Control-O.
  if a:findstart
    " For now we assume omni completion was triggered by the mapping for
    " automatic tag completion. Eventually it might be nice to check for a
    " leading "@" here and otherwise make it complete e.g. note names, so that
    " there's only one way to complete inside notes and the plug-in is smart
    " enough to know what the user wants to complete :-)
    return col('.')
  else
    return sort(keys(xolox#notes#tags#load_index()), 1)
  endif
endfunction

function! xolox#notes#save() abort " {{{1
  " When the current note's title is changed, automatically rename the file.
  if xolox#notes#filetype_is_note(&ft)
    let title = xolox#notes#current_title()
    let oldpath = expand('%:p')
    let newpath = xolox#notes#title_to_fname(title)
    if newpath == ''
      echoerr "Invalid note title"
      return
    endif
    let bang = v:cmdbang ? '!' : ''
    execute 'saveas' . bang fnameescape(newpath)
    " XXX If {oldpath} and {newpath} end up pointing to the same file on disk
    " yet xolox#misc#path#equals() doesn't catch this, we might end up
    " deleting the user's one and only note! One way to circumvent this
    " potential problem is to first delete the old note and then save the new
    " note. The problem with this approach is that :saveas might fail in which
    " case we've already deleted the old note...
    if !xolox#misc#path#equals(oldpath, newpath)
      if !filereadable(newpath)
        let message = "The notes plug-in tried to rename your note but failed to create %s so won't delete %s or you could lose your note! This should never happen... If you don't mind me borrowing some of your time, please contact me at peter@peterodding.com and include the old and new filename so that I can try to reproduce the issue. Thanks!"
        call confirm(printf(message, string(newpath), string(oldpath)))
        return
      endif
      call delete(oldpath)
    endif
    " Update the tags index on disk and in-memory.
    call xolox#notes#tags#forget_note(xolox#notes#fname_to_title(oldpath))
    call xolox#notes#tags#scan_note(title, join(getline(1, '$'), "\n"))
    call xolox#notes#tags#save_index()
    " Update in-memory list of all notes.
    call xolox#notes#cache_del(oldpath)
    call xolox#notes#cache_add(newpath, title)
  endif
endfunction

function! xolox#notes#delete(bang, title) " {{{1
  " Delete the note {title} and close the associated buffer & window.
  " If no {title} is given the current note is deleted.
  let title = xolox#misc#str#trim(a:title)
  if title == ''
    " Try the current buffer.
    let title = xolox#notes#fname_to_title(expand('%:p'))
  endif
  if !xolox#notes#exists(title)
    call xolox#misc#msg#warn("notes.vim %s: Failed to delete %s! (not a note)", g:xolox#notes#version, expand('%:p'))
  else
    let filename = xolox#notes#title_to_fname(title)
    if filereadable(filename) && delete(filename)
      call xolox#misc#msg#warn("notes.vim %s: Failed to delete %s!", g:xolox#notes#version, filename)
    else
      call xolox#notes#cache_del(filename)
      execute 'bdelete' . a:bang . ' ' . bufnr(filename)
    endif
  endif
endfunction

function! xolox#notes#search(bang, input) " {{{1
  " Search all notes for the pattern or keywords {input} (current word if none given).
  try
    let starttime = xolox#misc#timer#start()
    let input = a:input
    if input == ''
      let input = s:tag_under_cursor()
      if input == ''
        call xolox#misc#msg#warn("notes.vim %s: No string under cursor", g:xolox#notes#version)
        return
      endif
    endif
    if input =~ '^/.\+/$'
      call s:internal_search(a:bang, input, '', '')
      call s:set_quickfix_title([], input)
    else
      let keywords = split(input)
      let all_keywords = s:match_all_keywords(keywords)
      let any_keyword = s:match_any_keyword(keywords)
      call s:internal_search(a:bang, all_keywords, input, any_keyword)
      if &buftype == 'quickfix'
        " Enable line wrapping in the quick-fix window.
        setlocal wrap
        " Resize the quick-fix window to 1/3 of the screen height.
        let max_height = &lines / 3
        execute 'resize' max_height
        " Make it smaller if the content doesn't fill the window.
        normal G$
        let preferred_height = winline()
        execute 'resize' min([max_height, preferred_height])
        normal gg
        call s:set_quickfix_title(keywords, '')
      endif
    endif
    call xolox#misc#timer#stop("notes.vim %s: Searched notes in %s.", g:xolox#notes#version, starttime)
  catch /^Vim\%((\a\+)\)\=:E480/
    call xolox#misc#msg#warn("notes.vim %s: No matches", g:xolox#notes#version)
  endtry
endfunction

function! s:tag_under_cursor() " {{{2
  " Get the word or @tag under the text cursor.
  try
    let isk_save = &isk
    set iskeyword+=@-@
    return expand('<cword>')
  finally
    let &isk = isk_save
  endtry
endfunction

function! s:match_all_keywords(keywords) " {{{2
  " Create a regex that matches when a file contains all {keywords}.
  let results = copy(a:keywords)
  call map(results, '''\_^\_.*'' . xolox#misc#escape#pattern(v:val)')
  return '/' . escape(join(results, '\&'), '/') . '/'
endfunction

function! s:match_any_keyword(keywords) " {{{2
  " Create a regex that matches every occurrence of all {keywords}.
  let results = copy(a:keywords)
  call map(results, 'xolox#misc#escape#pattern(v:val)')
  return '/' . escape(join(results, '\|'), '/') . '/'
endfunction

function! s:set_quickfix_title(keywords, pattern) " {{{2
  " Set the title of the quick-fix window.
  if &buftype == 'quickfix'
    let num_notes = len(xolox#misc#list#unique(map(getqflist(), 'v:val["bufnr"]')))
    if len(a:keywords) > 0
      let keywords = map(copy(a:keywords), '"`" . v:val . "''"')
      let w:quickfix_title = printf('Found %i note%s containing the word%s %s',
            \ num_notes, num_notes == 1 ? '' : 's',
            \ len(keywords) == 1 ? '' : 's',
            \ len(keywords) > 1 ? (join(keywords[0:-2], ', ') . ' and ' . keywords[-1]) : keywords[0])
    else
      let w:quickfix_title = printf('Found %i note%s containing the pattern %s',
            \ num_notes, num_notes == 1 ? '' : 's',
            \ a:pattern)
    endif
  endif
endfunction

function! xolox#notes#related(bang) " {{{1
  " Find all notes related to the current note or file.
  let starttime = xolox#misc#timer#start()
  let bufname = bufname('%')
  if bufname == ''
    call xolox#misc#msg#warn("notes.vim %s: :RelatedNotes only works on named buffers!", g:xolox#notes#version)
  else
    let filename = xolox#misc#path#absolute(bufname)
    if xolox#notes#buffer_is_note()
      let keywords = xolox#notes#current_title()
      let pattern = '\<' . s:words_to_pattern(keywords) . '\>'
    else
      let pattern = s:words_to_pattern(filename)
      let keywords = filename
      if filename[0 : len($HOME)-1] == $HOME
        let relative = filename[len($HOME) + 1 : -1]
        let pattern = '\(' . pattern . '\|\~/' . s:words_to_pattern(relative) . '\)'
        let keywords = relative
      endif
    endif
    let pattern = '/' . escape(pattern, '/') . '/'
    let friendly_path = fnamemodify(filename, ':~')
    try
      call s:internal_search(a:bang, pattern, keywords, '')
      if &buftype == 'quickfix'
        let w:quickfix_title = 'Notes related to ' . friendly_path
      endif
    catch /^Vim\%((\a\+)\)\=:E480/
      call xolox#misc#msg#warn("notes.vim %s: No related notes found for %s", g:xolox#notes#version, friendly_path)
    endtry
  endif
  call xolox#misc#timer#stop("notes.vim %s: Found related notes in %s.", g:xolox#notes#version, starttime)
endfunction

" Miscellaneous functions. {{{1

function! xolox#notes#find_directories(include_shadow_directory) " {{{2
  " Generate a list of absolute pathnames of all notes directories.
  let directories = copy(g:notes_directories)
  " Add the shadow directory?
  if a:include_shadow_directory
    call add(directories, g:notes_shadowdir)
  endif
  " Return the expanded directory pathnames.
  return map(directories, 'expand(v:val)')
endfunction

function! xolox#notes#set_filetype() " {{{2
  " Load the notes file type if not already loaded.
  if &filetype != 'notes'
    " Change the file type.
    setlocal filetype=notes
  elseif synID(1, 1, 0) == 0
    " Load the syntax. When you execute :RecentNotes, switch to a different
    " buffer and then return to the buffer created by :RecentNotes, it will
    " have lost its syntax highlighting. The following line of code solves
    " this problem. We don't explicitly set the syntax to 'notes' so that we
    " preserve dot separated composed values.
    let &syntax = &syntax
  endif
endfunction

function! xolox#notes#swaphack() " {{{2
  " Selectively ignore the dreaded E325 interactive prompt.
  if exists('s:swaphack_enabled')
    let v:swapchoice = 'o'
  endif
endfunction

function! xolox#notes#autocmd_pattern(directory, use_extension) " {{{2
  " Generate a normalized automatic command pattern. First we resolve the path
  " to the directory with notes (eliminating any symbolic links) so that the
  " automatic command also applies to symbolic links pointing to notes (Vim
  " matches filename patterns in automatic commands after resolving
  " filenames).
  let directory = xolox#misc#path#absolute(a:directory)
  " On Windows we have to replace backslashes with forward slashes, otherwise
  " the automatic command will never trigger! This has to happen before we
  " make the fnameescape() call.
  if xolox#misc#os#is_win()
    let directory = substitute(directory, '\\', '/', 'g')
  endif
  " Escape the directory but not the trailing "*".
  let pattern = fnameescape(directory) . '/*'
  if a:use_extension && !empty(g:notes_suffix)
    let pattern .= g:notes_suffix
  endif
  " On Windows the pattern won't match if it contains repeating slashes.
  return substitute(pattern, '/\+', '/', 'g')
endfunction

function! xolox#notes#filetype_is_note(ft) " {{{2
  " Check whether the given file type value refers to the notes.vim plug-in.
  return index(split(a:ft, '\.'), 'notes') >= 0
endfunction

function! xolox#notes#buffer_is_note() " {{{2
  " Check whether the current buffer is a note (with the correct file type and path).
  let bufpath = expand('%:p:h')
  if xolox#notes#filetype_is_note(&ft)
    for directory in xolox#notes#find_directories(1)
      if xolox#misc#path#equals(bufpath, directory)
        return 1
      endif
    endfor
  endif
endfunction

function! xolox#notes#current_title() " {{{2
  " Get the title of the current note.
  let title = getline(1)
  let trimmed = xolox#misc#str#trim(title)
  if title != trimmed
    call setline(1, trimmed)
  endif
  return trimmed
endfunction

function! xolox#notes#friendly_date(time) " {{{2
  " Format a date as a human readable string.
  let format = '%A, %B %d, %Y'
  let today = strftime(format, localtime())
  let yesterday = strftime(format, localtime() - 60*60*24)
  let datestr = strftime(format, a:time)
  if datestr == today
    return "today"
  elseif datestr == yesterday
    return "yesterday"
  else
    return datestr
  endif
endfunction

function! s:internal_search(bang, pattern, keywords, phase2) " {{{2
  " Search notes for {pattern} regex, try to accelerate with {keywords} search.
  let bufnr_save = bufnr('%')
  let pattern = a:pattern
  silent cclose
  " Find all notes matching the given keywords or regex.
  let notes = []
  let phase2_needed = 1
  if a:keywords != '' && s:run_scanner(a:keywords, notes)
    if a:phase2 != ''
      let pattern = a:phase2
    endif
  else
    call s:vimgrep_wrapper(a:bang, a:pattern, xolox#notes#get_fnames(0))
    let notes = s:qflist_to_filenames()
    if a:phase2 != ''
      let pattern = a:phase2
    else
      let phase2_needed = 0
    endif
  endif
  if empty(notes)
    call xolox#misc#msg#warn("notes.vim %s: No matches", g:xolox#notes#version)
    return
  endif
  " If we performed a keyword search using the scanner.py script we need to
  " run :vimgrep to populate the quick-fix list. If we're emulating keyword
  " search using :vimgrep we need to run :vimgrep another time to get the
  " quick-fix list in the right format :-|
  if phase2_needed
    call s:vimgrep_wrapper(a:bang, pattern, notes)
  endif
  if a:bang == '' && bufnr('%') != bufnr_save
    " If :vimgrep opens the first matching file while &eventignore is still
    " set the file will be opened without activating a file type plug-in or
    " syntax script. Here's a workaround:
    doautocmd filetypedetect BufRead
  endif
  silent cwindow
  if &buftype == 'quickfix'
    execute 'match IncSearch' (&ignorecase ? substitute(pattern, '^/', '/\\c', '') : pattern)
  endif
endfunction

function! s:vimgrep_wrapper(bang, pattern, files) " {{{2
  " Search for {pattern} in {files} using :vimgrep.
  let starttime = xolox#misc#timer#start()
  let args = map(copy(a:files), 'fnameescape(v:val)')
  call insert(args, a:pattern . 'j')
  let s:swaphack_enabled = 1
  try
    let ei_save = &eventignore
    set eventignore=syntax,bufread
    execute 'vimgrep' . a:bang join(args)
    call xolox#misc#timer#stop("notes.vim %s: Populated quick-fix window in %s.", g:xolox#notes#version, starttime)
  finally
    let &eventignore = ei_save
    unlet s:swaphack_enabled
  endtry
endfunction

function! s:qflist_to_filenames() " {{{2
  " Get filenames of matched notes from quick-fix list.
  let names = {}
  for entry in getqflist()
    let names[xolox#misc#path#absolute(bufname(entry.bufnr))] = 1
  endfor
  return keys(names)
endfunction

function! s:run_scanner(keywords, matches) " {{{2
  " Try to run scanner.py script to find notes matching {keywords}.
  call xolox#misc#msg#info("notes.vim %s: Searching notes using keyword index ..", g:xolox#notes#version)
  let [success, notes] = s:python_command(a:keywords)
  if success
    call xolox#misc#msg#debug("notes.vim %s: Search script reported %i matching note%s.", g:xolox#notes#version, len(notes), len(notes) == 1 ? '' : 's')
    call extend(a:matches, notes)
    return 1
  endif
endfunction

function! xolox#notes#keyword_complete(arglead, cmdline, cursorpos) " {{{2
  " Search keyword completion for the :SearchNotes command.
  call inputsave()
  let [success, keywords] = s:python_command('--list=' . a:arglead)
  call inputrestore()
  return keywords
endfunction

function! s:python_command(...) " {{{2
  " Vim function to interface with the "search-notes.py" script.
  let script = xolox#misc#path#absolute(g:notes_indexscript)
  let python = executable('python2') ? 'python2' : 'python'
  let output = []
  let success = 0
  if !(executable(python) && filereadable(script))
    call xolox#misc#msg#debug("notes.vim %s: We can't execute the %s script!", g:xolox#notes#version, script)
  else
    let options = ['--database', g:notes_indexfile]
    if &ignorecase
      call add(options, '--ignore-case')
    endif
    for directory in xolox#notes#find_directories(0)
      call extend(options, ['--notes', directory])
    endfor
    let arguments = map([script] + options + a:000, 'xolox#misc#escape#shell(v:val)')
    let command = join([python] + arguments)
    call xolox#misc#msg#debug("notes.vim %s: Executing external command %s", g:xolox#notes#version, command)
    if !filereadable(xolox#misc#path#absolute(g:notes_indexfile))
      call xolox#misc#msg#info("notes.vim %s: Building keyword index (this might take a while) ..", g:xolox#notes#version)
    endif
    let result = xolox#misc#os#exec({'command': command, 'check': 0})
    if result['exit_code'] != 0
      call xolox#misc#msg#warn("notes.vim %s: Search script failed! Context: %s", g:xolox#notes#version, string(result))
    else
      let lines = result['stdout']
      call xolox#misc#msg#debug("notes.vim %s: Search script output (raw): %s", g:xolox#notes#version, string(lines))
      if !empty(lines) && lines[0] == 'Python works fine!'
        let output = lines[1:]
        let success = 1
        call xolox#misc#msg#debug("notes.vim %s: Search script output (processed): %s", g:xolox#notes#version, string(output))
      else
        call xolox#misc#msg#warn("notes.vim %s: Search script returned invalid output :-(", g:xolox#notes#version)
      endif
    endif
  endif
  return [success, output]
endfunction

" Getters for filenames & titles of existing notes. {{{2

if !exists('s:cache_mtime')
  let s:have_cached_names = 0
  let s:have_cached_titles = 0
  let s:have_cached_items = 0
  let s:cached_fnames = []
  let s:cached_titles = []
  let s:cached_pairs = {}
  let s:cache_mtime = 0
  let s:shadow_notes = ['New note', 'Note taking commands', 'Note taking syntax']
endif

function! xolox#notes#get_fnames(include_shadow_notes) " {{{3
  " Get list with filenames of all existing notes.
  if !s:have_cached_names
    let starttime = xolox#misc#timer#start()
    for directory in xolox#notes#find_directories(0)
      let pattern = xolox#misc#path#merge(directory, '*')
      let listing = glob(xolox#misc#path#absolute(pattern))
      call extend(s:cached_fnames, filter(split(listing, '\n'), 'filereadable(v:val)'))
    endfor
    let s:have_cached_names = 1
    call xolox#misc#timer#stop('notes.vim %s: Cached note filenames in %s.', g:xolox#notes#version, starttime)
  endif
  let fnames = copy(s:cached_fnames)
  if a:include_shadow_notes
    for title in s:shadow_notes
      call add(fnames, xolox#misc#path#merge(g:notes_shadowdir, title))
    endfor
  endif
  return fnames
endfunction

function! xolox#notes#get_titles(include_shadow_notes) " {{{3
  " Get list with titles of all existing notes.
  if !s:have_cached_titles
    let starttime = xolox#misc#timer#start()
    for filename in xolox#notes#get_fnames(0)
      call add(s:cached_titles, xolox#notes#fname_to_title(filename))
    endfor
    let s:have_cached_titles = 1
    call xolox#misc#timer#stop('notes.vim %s: Cached note titles in %s.', g:xolox#notes#version, starttime)
  endif
  let titles = copy(s:cached_titles)
  if a:include_shadow_notes
    call extend(titles, s:shadow_notes)
  endif
  return titles
endfunction

function! xolox#notes#exists(title) " {{{3
  " Return true if the note {title} exists.
  return index(xolox#notes#get_titles(0), a:title, 0, xolox#misc#os#is_win()) >= 0
endfunction

function! xolox#notes#get_fnames_and_titles(include_shadow_notes) " {{{3
  " Get dictionary of filename => title pairs of all existing notes.
  if !s:have_cached_items
    let starttime = xolox#misc#timer#start()
    let fnames = xolox#notes#get_fnames(0)
    let titles = xolox#notes#get_titles(0)
    let limit = len(fnames)
    let index = 0
    while index < limit
      let s:cached_pairs[fnames[index]] = titles[index]
      let index += 1
    endwhile
    let s:have_cached_items = 1
    call xolox#misc#timer#stop('notes.vim %s: Cached note filenames and titles in %s.', g:xolox#notes#version, starttime)
  endif
  let pairs = copy(s:cached_pairs)
  if a:include_shadow_notes
    for title in s:shadow_notes
      let fname = xolox#misc#path#merge(g:notes_shadowdir, title)
      let pairs[fname] = title
    endfor
  endif
  return pairs
endfunction

function! xolox#notes#fname_to_title(filename) " {{{3
  " Convert absolute note {filename} to title.
  let fname = a:filename
  " Strip suffix?
  if fname[-len(g:notes_suffix):] == g:notes_suffix
    let fname = fname[0:-len(g:notes_suffix)-1]
  endif
  " Strip directory path.
  let fname = fnamemodify(fname, ':t')
  " Decode special characters.
  return xolox#misc#path#decode(fname)
endfunction

function! xolox#notes#title_to_fname(title) " {{{3
  " Convert note {title} to absolute filename.
  let filename = xolox#misc#path#encode(a:title)
  if filename != ''
    let directory = xolox#notes#select_directory()
    let pathname = xolox#misc#path#merge(directory, filename . g:notes_suffix)
    return xolox#misc#path#absolute(pathname)
  endif
  return ''
endfunction

function! xolox#notes#select_directory() " {{{3
  " Pick the best suited directory for creating a new note.
  let bufdir = expand('%:p:h')
  let notes_directories = xolox#notes#find_directories(0)
  for directory in notes_directories
    if xolox#misc#path#equals(bufdir, directory)
      return directory
    endif
  endfor
  return notes_directories[0]
endfunction

function! xolox#notes#cache_add(filename, title) " {{{3
  " Add {filename} and {title} of new note to cache.
  let filename = xolox#misc#path#absolute(a:filename)
  if index(s:cached_fnames, filename) == -1
    call add(s:cached_fnames, filename)
    if !empty(s:cached_titles)
      call add(s:cached_titles, a:title)
    endif
    if !empty(s:cached_pairs)
      let s:cached_pairs[filename] = a:title
    endif
    let s:cache_mtime = localtime()
  endif
endfunction

function! xolox#notes#cache_del(filename) " {{{3
  " Delete {filename} from cache.
  let filename = xolox#misc#path#absolute(a:filename)
  let index = index(s:cached_fnames, filename)
  if index >= 0
    call remove(s:cached_fnames, index)
    if !empty(s:cached_titles)
      call remove(s:cached_titles, index)
    endif
    if !empty(s:cached_pairs)
      call remove(s:cached_pairs, filename)
    endif
    let s:cache_mtime = localtime()
  endif
endfunction

function! xolox#notes#unload_from_cache() " {{{3
  " Forget deleted notes automatically (called by "BufUnload" automatic command).
  let bufname = expand('<afile>:p')
  if !filereadable(bufname)
    call xolox#notes#cache_del(bufname)
  endif
endfunction

" Functions called by the file type plug-in and syntax script. {{{2

function! xolox#notes#insert_ruler() " {{{3
  " Insert horizontal ruler delimited by empty lines.
  let lnum = line('.')
  if getline(lnum) =~ '\S' && getline(lnum + 1) !~ '\S'
    let lnum += 1
  endif
  let line1 = prevnonblank(lnum)
  let line2 = nextnonblank(lnum)
  if line1 < lnum && line2 > lnum
    execute printf('%i,%idelete', line1 + 1, line2 - 1)
  endif
  call append(line1, ['', g:notes_ruler_text, ''])
endfunction

function! xolox#notes#insert_quote(style) " {{{3
  " XXX When I pass the below string constants as arguments from the file type
  " plug-in the resulting strings contain mojibake (UTF-8 interpreted as
  " latin1?) even if both scripts contain a UTF-8 BOM! Maybe a bug in Vim?!
  if xolox#notes#unicode_enabled()
    let [open_quote, close_quote] = a:style == 1 ? ['‘', '’'] : ['“', '”']
  else
    let [open_quote, close_quote] = a:style == 1 ? ['`', "'"] : ['"', '"']
  endif
  return getline('.')[col('.')-2] =~ '[^\t (]$' ? close_quote : open_quote
endfunction

function! xolox#notes#insert_bullet(chr) " {{{3
  " Insert a UTF-8 list bullet when the user types "*".
  if getline('.')[0 : max([0, col('.') - 2])] =~ '^\s*$'
    return xolox#notes#get_bullet(a:chr)
  endif
  return a:chr
endfunction

function! xolox#notes#get_bullet(chr)
  return xolox#notes#unicode_enabled() ? '•' : a:chr
endfunction

function! xolox#notes#indent_list(direction, line1, line2) " {{{3
  " Change indent of list items from {line1} to {line2} using {command}.
  let indentstr = repeat(' ', &tabstop)
  if a:line1 == a:line2 && getline(a:line1) == ''
    call setline(a:line1, indentstr)
  else
    " Regex to match a leading bullet.
    let leading_bullet = xolox#notes#leading_bullet_pattern()
    for lnum in range(a:line1, a:line2)
      let line = getline(lnum)
      " Calculate new nesting level, should not result in < 0.
      let level = max([0, xolox#notes#get_list_level(line) + a:direction])
      if a:direction == 1
        " Indent the line.
        let line = indentstr . line
      else
        " Unindent the line.
        let line = substitute(line, '^' . indentstr, '', '')
      endif
      " Replace the bullet.
      let bullet = g:notes_list_bullets[level % len(g:notes_list_bullets)]
      call setline(lnum, substitute(line, leading_bullet, xolox#misc#escape#substitute(bullet), ''))
    endfor
    " Regex to match a trailing bullet.
    if getline('.') =~ xolox#notes#trailing_bullet_pattern()
      " Restore trailing space after list bullet.
      call setline('.', getline('.') . ' ')
    endif
  endif
  normal $
endfunction

function! xolox#notes#leading_bullet_pattern()
  " Return a regular expression pattern that matches any leading list bullet.
  let escaped_bullets = copy(g:notes_list_bullets)
  call map(escaped_bullets, 'xolox#misc#escape#pattern(v:val)')
  return '\(\_^\s*\)\@<=\(' . join(escaped_bullets, '\|') . '\)'
endfunction

function! xolox#notes#trailing_bullet_pattern()
  " Return a regular expression pattern that matches any trailing list bullet.
  let escaped_bullets = copy(g:notes_list_bullets)
  call map(escaped_bullets, 'xolox#misc#escape#pattern(v:val)')
  return '\(' . join(escaped_bullets, '\|') . '\|\*\)$'
endfunction

function! xolox#notes#get_comments_option()
  " Get the value for the &comments option including user defined list bullets.
  let items = copy(g:notes_list_bullets)
  call map(items, '": " . v:val . " "')
  call add(items, ':> ') " <- e-mail style block quotes.
  return join(items, ',')
endfunction

function! xolox#notes#get_list_level(line)
  " Get the nesting level of the list item on the given line. This will only
  " work with the list item indentation style expected by the notes plug-in
  " (that is, top level list items are indented with one space, each nested
  " level below that is indented by pairs of three spaces).
  return (len(matchstr(a:line, '^\s*')) - 1) / 3
endfunction

function! xolox#notes#cleanup_list() " {{{3
  " Automatically remove empty list items on Enter.
  if getline('.') =~ (xolox#notes#leading_bullet_pattern() . '\s*$')
    let s:sol_save = &startofline
    setlocal nostartofline " <- so that <C-u> clears the complete line
    return "\<C-o>0\<C-o>d$\<C-o>o"
  else
    if exists('s:sol_save')
      let &l:startofline = s:sol_save
      unlet s:sol_save
    endif
    return "\<CR>"
  endif
endfunction

function! xolox#notes#refresh_syntax() " {{{3
  " Update syntax highlighting of note names and code blocks.
  if xolox#notes#filetype_is_note(&ft) && line('$') > 1
    let starttime = xolox#misc#timer#start()
    call xolox#notes#highlight_names(0)
    call xolox#notes#highlight_sources(0)
    call xolox#misc#timer#stop("notes.vim %s: Refreshed highlighting in %s.", g:xolox#notes#version, starttime)
  endif
endfunction

function! xolox#notes#highlight_names(force) " {{{3
  " Highlight the names of all notes as "notesName" (linked to "Underlined").
  if a:force || !(exists('b:notes_names_last_highlighted') && b:notes_names_last_highlighted > s:cache_mtime)
    let starttime = xolox#misc#timer#start()
    let current_note = xolox#notes#current_title()
    let titles = filter(xolox#notes#get_titles(1), '!empty(v:val) && v:val != current_note')
    call map(titles, 's:words_to_pattern(v:val)')
    call sort(titles, 's:sort_longest_to_shortest')
    if hlexists('notesName')
      syntax clear notesName
    endif
    execute 'syntax match notesName /\c\%>1l\%(' . escape(join(titles, '\|'), '/') . '\)/'
    let b:notes_names_last_highlighted = localtime()
    call xolox#misc#timer#stop("notes.vim %s: Highlighted note names in %s.", g:xolox#notes#version, starttime)
  endif
endfunction

function! s:words_to_pattern(words)
  " Quote regex meta characters, enable matching of hard wrapped words.
  return substitute(xolox#misc#escape#pattern(a:words), '\s\+', '\\_s\\+', 'g')
endfunction

function! s:sort_longest_to_shortest(a, b)
  " Sort note titles by length, starting with the shortest.
  return len(a:a) < len(a:b) ? 1 : -1
endfunction

function! xolox#notes#highlight_sources(force) " {{{3
  " Syntax highlight source code embedded in notes.
  let starttime = xolox#misc#timer#start()
  " Look for code blocks in the current note.
  let filetypes = {}
  for line in getline(1, '$')
    let ft = matchstr(line, '{{' . '{\zs\w\+\>')
    if ft !~ '^\d*$' | let filetypes[ft] = 1 | endif
  endfor
  " Don't refresh the highlighting if nothing has changed.
  if !a:force && exists('b:notes_previous_sources') && b:notes_previous_sources == filetypes
    return
  else
    let b:notes_previous_sources = filetypes
  endif
  " Now we're ready to actually highlight the code blocks.
  if !empty(filetypes)
    let startgroup = 'notesCodeStart'
    let endgroup = 'notesCodeEnd'
    for ft in keys(filetypes)
      let group = 'notesSnippet' . toupper(ft)
      let include = s:syntax_include(ft)
      let command = 'syntax region %s matchgroup=%s start="{{{%s" matchgroup=%s end="}}}" keepend contains=%s%s'
      execute printf(command, group, startgroup, ft, endgroup, include, has('conceal') ? ' concealends' : '')
    endfor
    if &vbs >= 1
      call xolox#misc#timer#stop("notes.vim %s: Highlighted embedded %s sources in %s.", g:xolox#notes#version, join(sort(keys(filetypes)), '/'), starttime)
    endif
  endif
endfunction

function! s:syntax_include(filetype)
  " Include the syntax highlighting of another {filetype}.
  let grouplistname = '@' . toupper(a:filetype)
  " Unset the name of the current syntax while including the other syntax
  " because some syntax scripts do nothing when "b:current_syntax" is set.
  if exists('b:current_syntax')
    let syntax_save = b:current_syntax
    unlet b:current_syntax
  endif
  try
    execute 'syntax include' grouplistname 'syntax/' . a:filetype . '.vim'
    execute 'syntax include' grouplistname 'after/syntax/' . a:filetype . '.vim'
  catch /E484/
    " Ignore missing scripts.
  endtry
  " Restore the name of the current syntax.
  if exists('syntax_save')
    let b:current_syntax = syntax_save
  elseif exists('b:current_syntax')
    unlet b:current_syntax
  endif
  return grouplistname
endfunction

function! xolox#notes#include_expr(fname) " {{{3
  " Translate string {fname} to absolute filename of note.
  " TODO Use inputlist() when more than one note matches?!
  let notes = copy(xolox#notes#get_fnames_and_titles(1))
  let pattern = xolox#misc#escape#pattern(a:fname)
  call filter(notes, 'v:val =~ pattern')
  if !empty(notes)
    let filtered_notes = items(notes)
    let lnum = line('.')
    for range in range(3)
      let line1 = lnum - range
      let line2 = lnum + range
      let text = s:normalize_ws(join(getline(line1, line2), "\n"))
      for [fname, title] in filtered_notes
        if text =~? xolox#misc#escape#pattern(s:normalize_ws(title))
          return fname
        endif
      endfor
    endfor
  endif
  return ''
endfunction

function! s:normalize_ws(s)
  " Enable string comparison that ignores differences in whitespace.
  return xolox#misc#str#trim(substitute(a:s, '\_s\+', '', 'g'))
endfunction

function! xolox#notes#foldexpr() " {{{3
  " Folding expression to fold atx style Markdown headings.
  let lastlevel = foldlevel(v:lnum - 1)
  let nextlevel = match(getline(v:lnum), '^#\+\zs')
  let retval = '='
  if lastlevel <= 0 && nextlevel >= 1
    let retval = '>' . nextlevel
  elseif nextlevel >= 1
    if lastlevel > nextlevel
      let retval = '<' . nextlevel
    else
      let retval = '>' . nextlevel
    endif
  endif
  if retval != '='
    " Check whether the change in folding introduced by 'rv'
    " is invalidated because we're inside a code block.
    let pos_save = getpos('.')
    try
      call setpos('.', [0, v:lnum, 1, 0])
      if search('{{{\|\(}}}\)', 'bnpW') == 1
        let retval = '='
      endif
    finally
      " Always restore the cursor position!
      call setpos('.', pos_save)
    endtry
  endif
  return retval
endfunction

function! xolox#notes#foldtext() " {{{3
  " Replace atx style "#" markers with "-" fold marker.
  let line = getline(v:foldstart)
  if line == ''
    let line = getline(v:foldstart + 1)
  endif
  let matches = matchlist(line, '^\(#\+\)\s*\(.*\)$')
  if len(matches) >= 3
    let prefix = repeat('-', len(matches[1]))
    return prefix . ' ' . matches[2] . ' '
  else
    return line
  endif
endfunction

" }}}1

" Make sure the plug-in configuration has been properly initialized before
" any of the auto-load functions in this Vim script can be called.
call xolox#notes#init()

" vim: ts=2 sw=2 et bomb
