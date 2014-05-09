"===============================================================================
" Initialization
"===============================================================================

" Tweak key settings. If the key is set using 'expr-quote' (h: expr-quote), then
" there's nothing that we need to do. If it's set using raw strings, then we
" need to convert it.  We need to resort to such voodoo exec magic here to get
" it to work the way we like. '<C-n>' is converted to '\<C-n>' by the end and
" the global vars are replaced by their new value. This is ok since the mapping
" using '<C-n>' should already have completed in the plugin file.
for key in [ 'g:multi_cursor_next_key',
           \ 'g:multi_cursor_prev_key',
           \ 'g:multi_cursor_skip_key',
           \ 'g:multi_cursor_quit_key' ]
  if exists(key)
    " Translate raw strings like "<C-n>" into key code like "\<C-n>"
    exec 'let temp = '.key
    if temp =~ '^<.*>$'
      exec 'let '.key.' = "\'.temp.'"' 
    endif
  else
    " If the user didn't define it, initialize it to an empty string so the
    " logic later don't break
    exec 'let '.key.' = ""'
  endif
endfor

" These keys will not be replicated at every cursor location. Make sure that
" this assignment happens AFTER the key tweak setting above
let s:special_keys = {
      \ 'v': [ g:multi_cursor_next_key, g:multi_cursor_prev_key, g:multi_cursor_skip_key ],
      \ 'n': [ g:multi_cursor_next_key ],
      \ }

" The highlight group we use for all the cursors
let s:hi_group_cursor = 'multiple_cursors_cursor'

" The highlight group we use for all the visual selection
let s:hi_group_visual = 'multiple_cursors_visual'

" Set up highlighting
if !hlexists(s:hi_group_cursor)
  exec "highlight ".s:hi_group_cursor." term=reverse cterm=reverse gui=reverse"
endif
if !hlexists(s:hi_group_visual)
  exec "highlight link ".s:hi_group_visual." Visual"
endif

"===============================================================================
" Internal Mappings
"===============================================================================

inoremap <silent> <Plug>(i) <C-o>:call <SID>process_user_input()<CR>
nnoremap <silent> <Plug>(i) :call <SID>process_user_input()<CR>
xnoremap <silent> <Plug>(i) :<C-u>call <SID>process_user_input()<CR>

inoremap <silent> <Plug>(a) <C-o>:call <SID>apply_user_input_next('i')<CR>
nnoremap <silent> <Plug>(a) :call <SID>apply_user_input_next('n')<CR>
xnoremap <silent> <Plug>(a) :<C-u>call <SID>apply_user_input_next('v')<CR>

inoremap <silent> <Plug>(d) <C-o>:call <SID>detect_bad_input()<CR>
nnoremap <silent> <Plug>(d) :call <SID>detect_bad_input()<CR>
xnoremap <silent> <Plug>(d) :<C-u>call <SID>detect_bad_input()<CR>

inoremap <silent> <Plug>(w) <C-o>:call <SID>wait_for_user_input('')<CR>
nnoremap <silent> <Plug>(w) :call <SID>wait_for_user_input('')<CR>
xnoremap <silent> <Plug>(w) :<C-u>call <SID>wait_for_user_input('')<CR>

" Note that although these mappings are seemingly triggerd from Visual mode,
" they are in fact triggered from Normal mode. We quit visual mode to allow the
" virtual highlighting to take over
nnoremap <silent> <Plug>(p) :<C-u>call multiple_cursors#prev()<CR>
nnoremap <silent> <Plug>(s) :<C-u>call multiple_cursors#skip()<CR>
nnoremap <silent> <Plug>(n) :<C-u>call multiple_cursors#new('v')<CR>

"===============================================================================
" Public Functions
"===============================================================================

" Print some debugging info
function! multiple_cursors#debug()
  call s:cm.debug()
endfunction

function! multiple_cursors#get_latency_debug_file()
  return s:latency_debug_file
endfunction

" Creates a new cursor. Different logic applies depending on the mode the user
" is in and the current state of the buffer.
" 1. In normal mode, a new cursor is created at the end of the word under Vim's
" normal cursor
" 2. In visual mode, if the visual selection covers more than one line, a new
" cursor is created at the beginning of each line
" 3. In visual mode, if the visual selection covers a single line, a new cursor
" is created at the end of the visual selection. Another cursor will be
" attempted to be created at the next occurrence of the visual selection
function! multiple_cursors#new(mode)
  if a:mode ==# 'n'
    " Reset all existing cursors, don't restore view and setting
    call s:cm.reset(0, 0)

    " Select the word under cursor to set the '< and '> marks
    exec "normal! viw"
    call s:exit_visual_mode()

    " Add cursor with the current visual selection
    call s:cm.add(s:pos("'>"), s:region("'<", "'>"))
    call s:wait_for_user_input('v')
  elseif a:mode ==# 'v'
    " If the visual area covers the same line, then do a search for next
    " occurrence
    let start = line("'<")
    let finish = line("'>")
    if start != finish
      call s:cm.reset(0, 0)
      let col = col("'<")
      for line in range(line("'<"), line("'>"))
        let pos = [line, col]
        call s:cm.add(pos)
      endfor
      " Start in normal mode
      call s:wait_for_user_input('n')
    else
      " Came directly from visual mode
      if s:cm.is_empty()
        call s:cm.reset(0, 0)

        if visualmode() ==# 'V'
          let left = [line('.'), 1]
          let right = [line('.'), col('$')-1]
          if right[1] == 0 " empty line
            return
          endif
          call s:cm.add(right, [left, right])
        else
          call s:cm.add(s:pos("'>"), s:region("'<", "'>"))
        endif
      endif
      let content = s:get_text(s:region("'<", "'>"))
      let next = s:find_next(content)
      if s:cm.add(next[1], next)
        call s:update_visual_markers(next)
      else
        call cursor(s:cm.get_current().position)
        echohl WarningMsg | echo 'No more matches' | echohl None
      endif
      call s:wait_for_user_input('v')
    endif
  endif
endfunction

" Delete the current cursor. If there's no more cursors, stop the loop
function! multiple_cursors#prev()
  call s:cm.delete_current()
  if !s:cm.is_empty()
    call s:update_visual_markers(s:cm.get_current().visual)
    call cursor(s:cm.get_current().position)
    call s:wait_for_user_input('v')
  endif
endfunction

" Skip the current cursor and move to the next cursor
function! multiple_cursors#skip()
  call s:cm.delete_current()
  let content = s:get_text(s:region("'<", "'>"))
  let next = s:find_next(content)
  call s:cm.add(next[1], next)
  call s:update_visual_markers(next)
  call s:wait_for_user_input('v')
endfunction

" Search for pattern between the start and end line number. For each match, add
" a virtual cursor at the end and start multicursor mode
" This function is called from a command. User commands in Vim do not support
" passing in column ranges. If the user selects a block of text in visual mode,
" but not visual line mode, we only want to match patterns within the actual
" visual selection. We get around this by checking the last visual selection and
" see if its start and end lines match the input. If so, we assume that the user
" did a normal visual selection and we use the '< and '> marks to define the
" region instead of start and end from the method parameter.
function! multiple_cursors#find(start, end, pattern)
  let s:cm.saved_winview = winsaveview()
  let s:cm.start_from_find = 1
  if visualmode() ==# 'v' && a:start == line("'<") && a:end == line("'>")
    let pos1 = s:pos("'<")
    let pos2 = s:pos("'>")
  else
    let pos1 = [a:start, 1]
    let pos2 = [a:end, col([a:end, '$'])]
  endif
  call cursor(pos1)
  let first = 1
  while 1
    if first
      " First search starts from the current position
      let match = search(a:pattern, 'cW')
      let first = 0
    else
      let match = search(a:pattern, 'W')
    endif
    if !match
      break
    endif
    let left = s:pos('.')
    call search(a:pattern, 'ceW')
    let right = s:pos('.')
    if s:compare_pos(right, pos2) > 0
      break
    endif
    call s:cm.add(right, [left, right])
    " Redraw here forces the cursor movement to be updated. This prevents the
    " jerky behavior when doing any action once the cursors are added. But it
    " also slows down adding the cursors dramatically. We need to a better
    " solution here
    " redraw
  endwhile
  if s:cm.is_empty()
    call winrestview(s:cm.saved_winview)
    echohl ErrorMsg | echo 'No match found' | echohl None
    return
  else
    echohl Normal | echo 'Added '.s:cm.size().' cursor'.(s:cm.size()>1?'s':'') | echohl None
    call s:wait_for_user_input('v')
  endif
endfunction

"===============================================================================
" Cursor class
"===============================================================================
let s:Cursor = {}

" Create a new cursor. Highlight it and save the current line length
function! s:Cursor.new(position)
  let obj = copy(self)
  let obj.position = copy(a:position)
  let obj.visual = []
  let obj.cursor_hi_id = s:highlight_cursor(a:position)
  let obj.visual_hi_id = 0
  let obj.line_length = col([a:position[0], '$'])
  return obj
endfunction

" Return the line the cursor is on
function! s:Cursor.line() dict
  return self.position[0]
endfunction

" Return the column the cursor is on
function! s:Cursor.column() dict
  return self.position[1]
endfunction

" Move the cursor location by the number of lines and columns specified in the
" input. The input can be negative.
function! s:Cursor.move(line, column) dict
  let self.position[0] += a:line
  let self.position[1] += a:column
  if !empty(self.visual)
    let self.visual[0][0] += a:line
    let self.visual[0][1] += a:column
    let self.visual[1][0] += a:line
    let self.visual[1][1] += a:column
  endif
  call self.update_highlight()
endfunction

" Update the current position of the cursor
function! s:Cursor.update_position(pos) dict
  let self.position[0] = a:pos[0]
  let self.position[1] = a:pos[1]
  call self.update_highlight()
endfunction

" Reapply the highlight on the cursor
function! s:Cursor.update_highlight() dict
  call s:cm.remove_highlight(self.cursor_hi_id)
  let self.cursor_hi_id = s:highlight_cursor(self.position)
endfunction

" Refresh the length of the line the cursor is on. This could change from
" underneath
function! s:Cursor.update_line_length() dict
  let self.line_length = col([self.line(), '$'])
endfunction

" Update the visual selection and its highlight
function! s:Cursor.update_visual_selection(region) dict
  let self.visual = deepcopy(a:region)
  call s:cm.remove_highlight(self.visual_hi_id)
  let self.visual_hi_id = s:highlight_region(a:region)
endfunction

" Remove the visual selection and its highlight
function! s:Cursor.remove_visual_selection() dict
  let self.visual = []
  " TODO(terryma): Move functionality into separate class
  call s:cm.remove_highlight(self.visual_hi_id)
  let self.visual_hi_id = 0
endfunction

"===============================================================================
" CursorManager class
"===============================================================================
let s:CursorManager = {}

" Constructor
function! s:CursorManager.new()
  let obj = copy(self)
  " List of Cursors we're managing
  let obj.cursors = []
  " Current index into the s:cursors array
  let obj.current_index = -1
  " This marks the starting cursor index into the s:cursors array
  let obj.starting_index = -1
  " We save some user settings when the plugin loads initially
  let obj.saved_settings = {
        \ 'virtualedit': &virtualedit,
        \ 'cursorline': &cursorline,
        \ 'lazyredraw': &lazyredraw,
        \ 'paste': &paste,
        \ }
  " We save the window view when multicursor mode is entered
  let obj.saved_winview = []
  " Track whether we started multicursor mode from calling multiple_cursors#find
  let obj.start_from_find = 0
  return obj
endfunction

" Clear all cursors and their highlights
function! s:CursorManager.reset(restore_view, restore_setting) dict
  if a:restore_view
    " Return the view back to the beginning
    if !empty(self.saved_winview)
      call winrestview(self.saved_winview)
    endif

    " If the cursor moved, just restoring the view could get confusing, let's
    " put the cursor at where the user left it. Only do this if we didn't start
    " from find mode
    if !self.is_empty() && !self.start_from_find
      call cursor(self.get(0).position)
    endif
  endif

  " Delete all cursors and clear their highlights. Don't do clearmatches() as
  " that will potentially interfere with other plugins
  if !self.is_empty()
    for i in range(self.size())
      call self.remove_highlight(self.get(i).cursor_hi_id)
      call self.remove_highlight(self.get(i).visual_hi_id)
    endfor
  endif

  let self.cursors = []
  let self.current_index = -1
  let self.starting_index = -1
  let self.saved_winview = []
  let self.start_from_find = 0
  let s:char = ''
  if a:restore_setting
    call self.restore_user_settings()
  endif
endfunction

" Returns 0 if it's not managing any cursors at the moment
function! s:CursorManager.is_empty() dict
  return self.size() == 0
endfunction

" Returns the number of cursors it's managing
function! s:CursorManager.size() dict
  return len(self.cursors)
endfunction

" Returns the current cursor
function! s:CursorManager.get_current() dict
  return self.cursors[self.current_index]
endfunction

" Returns the cursor at index i
function! s:CursorManager.get(i) dict
  return self.cursors[a:i]
endfunction

" Removes the current cursor and all its associated highlighting. Also update
" the current index
function! s:CursorManager.delete_current() dict
  call self.remove_highlight(self.get_current().cursor_hi_id)
  call self.remove_highlight(self.get_current().visual_hi_id)
  call remove(self.cursors, self.current_index)
  let self.current_index -= 1
endfunction

" Remove the highlighting if its matchid exists
function! s:CursorManager.remove_highlight(hi_id) dict
  if a:hi_id
    " If the user did a matchdelete or a clearmatches, we don't want to barf if
    " the matchid is no longer valid
    silent! call matchdelete(a:hi_id)
  endif
endfunction

function! s:CursorManager.debug() dict
  let i = 0
  for c in self.cursors
    echom 'cursor #'.i.': pos='.string(c.position).' visual='.string(c.visual)
    let i+=1
  endfor
  echom 'input = '.s:char
  echom 'index = '.self.current_index
  echom 'pos = '.string(s:pos('.'))
  echom '''< = '.string(s:pos("'<"))
  echom '''> = '.string(s:pos("'>"))
  echom 'to mode = '.s:to_mode
  echom 'from mode = '.s:from_mode
  " echom 'special keys = '.string(s:special_keys)
  echom ' '
endfunction

" Sync the current cursor to the current Vim cursor. This includes updating its
" location, its highlight, and potentially its visual region. Return true if the
" position changed, false otherwise
function! s:CursorManager.update_current() dict
  let cur = self.get_current()
  if s:to_mode ==# 'v' || s:to_mode ==# 'V'
    " If we're in visual line mode, we need to go to visual mode before we can
    " update the visual region
    if s:to_mode ==# 'V'
      exec "normal! gvv\<Esc>"
    endif

    " Sets the cursor at the right place
    exec "normal! gv\<Esc>"
    call cur.update_visual_selection(s:get_visual_region(s:pos('.')))
  elseif s:from_mode ==# 'v' || s:from_mode ==# 'V'
    call cur.remove_visual_selection()
  endif
  let vdelta = line('$') - s:saved_linecount
  " If the total number of lines changed in the buffer, we need to potentially
  " adjust other cursor locations
  if vdelta != 0
    if self.current_index != self.size() - 1
      let cur_line_length = len(getline(cur.line()))
      let new_line_length = len(getline('.'))
      for i in range(self.current_index+1, self.size()-1)
        let hdelta = 0
        " Note: some versions of Vim don't like chaining function calls like
        " a.b().c(). For compatibility reasons, don't do it
        let c = self.get(i)
        " If there're other cursors on the same line, we need to adjust their
        " columns. This needs to happen before we adjust their line!
        if cur.line() == c.line()
          if vdelta > 0
            " Added a line
            let hdelta = cur_line_length * -1
          else
            " Removed a line
            let hdelta = new_line_length
          endif
        endif
        call c.move(vdelta, hdelta)
      endfor
    endif
  else
    " If the line length changes, for all the other cursors on the same line as
    " the current one, update their cursor location as well
    let hdelta = col('$') - cur.line_length
    " Only do this if we're still on the same line as before
    if hdelta != 0 && cur.line() == line('.')
      " Update all the cursor's positions that occur after the current cursor on
      " the same line
      if self.current_index != self.size() - 1
        for i in range(self.current_index+1, self.size()-1)
          let c = self.get(i)
          " Only do it for cursors on the same line
          if cur.line() == c.line()
            call c.move(0, hdelta)
          else
            " Early exit, if we're not on the same line, neither will any cursor
            " that come after this
            break
          endif
        endfor
      endif
    endif
  endif

  let pos = s:pos('.')
  if cur.position == pos
    return 0
  endif
  call cur.update_position(pos)
  return 1
endfunction

" Advance to the next cursor
function! s:CursorManager.next() dict
  let self.current_index = (self.current_index + 1) % self.size()
endfunction

" Start tracking cursor updates
function! s:CursorManager.start_loop() dict
  let self.starting_index = self.current_index
endfunction

" Returns true if we're cycled through all the cursors
function! s:CursorManager.loop_done() dict
  return self.current_index == self.starting_index
endfunction

" Tweak some user settings, and save our current window view. This is called
" every time multicursor mode is entered.
" virtualedit needs to be set to onemore for updates to work correctly
" cursorline needs to be turned off for the cursor highlight to work on the line
" where the real vim cursor is
" lazyredraw needs to be turned on to prevent jerky screen behavior with many
" cursors on screen
" paste mode needs to be switched off since it turns off a bunch of features
" that's critical for the plugin to function
function! s:CursorManager.initialize() dict
  let self.saved_settings['virtualedit'] = &virtualedit
  let self.saved_settings['cursorline'] = &cursorline
  let self.saved_settings['lazyredraw'] = &lazyredraw
  let self.saved_settings['paste'] = &paste
  let &virtualedit = "onemore"
  let &cursorline = 0
  let &lazyredraw = 1
  let &paste = 0
  " We could have already saved the view from multiple_cursors#find
  if !self.start_from_find
    let self.saved_winview = winsaveview()
  endif
endfunction

" Restore user settings.
function! s:CursorManager.restore_user_settings() dict
  if !empty(self.saved_settings)
    let &virtualedit = self.saved_settings['virtualedit']
    let &cursorline = self.saved_settings['cursorline']
    let &lazyredraw = self.saved_settings['lazyredraw']
    let &paste = self.saved_settings['paste']
  endif
endfunction

" Reselect the current cursor's region in visual mode
function! s:CursorManager.reapply_visual_selection() dict
  call s:select_in_visual_mode(self.get_current().visual)
endfunction

" Creates a new virtual cursor as 'pos'
" Optionally a 'region' object can be passed in as second argument. If set, the
" visual region of the cursor will be set to it
" Return true if the cursor has been successfully added, false otherwise
" Mode change: Normal -> Normal
" Cursor change: None (TODO Should we set Vim's cursor to pos?)
function! s:CursorManager.add(pos, ...) dict
  " Lazy init
  if self.is_empty()
    call self.initialize()
  endif

  " Don't add duplicates
  let i = 0
  for c in self.cursors
    if c.position == a:pos
      return 0
    endif
    let i+=1
  endfor

  let cursor = s:Cursor.new(a:pos)

  " Save the visual selection
  if a:0 > 0
    call cursor.update_visual_selection(a:1)
  endif

  call add(self.cursors, cursor)
  let self.current_index += 1
  return 1
endfunction

"===============================================================================
" Variables
"===============================================================================

" This is the last user input that we're going to replicate, in its string form
let s:char = ''
" This is the mode the user is in before s:char
let s:from_mode = ''
" This is the mode the user is in after s:char
let s:to_mode = ''
" This is the total number of lines in the buffer before processing s:char
let s:saved_linecount = -1
" This is used to apply the highlight fix. See s:apply_highight_fix()
let s:saved_line = 0
" This is the number of cursor locations where we detected an input that we
" cannot play back
let s:bad_input = 0
" Singleton cursor manager instance
let s:cm = s:CursorManager.new()

"===============================================================================
" Utility functions
"===============================================================================

" Return the position of the input marker as a two element array. First element
" is the line number, second element is the column number
function! s:pos(mark)
  let pos = getpos(a:mark)
  return [pos[1], pos[2]]
endfunction

" Return the region covered by the input markers as a two element array. First
" element is the position of the start marker, second element is the position of
" the end marker
function! s:region(start_mark, end_mark)
  return [s:pos(a:start_mark), s:pos(a:end_mark)]
endfunction

" Exit visual mode and go back to normal mode
" The reason for the additional gv\<Esc> is that it allows the cursor to stay
" on where it was before exiting
" Mode change: Normal -> Normal or Visual -> Normal
" Cursor change: If in visual mode, changed to exactly where it was on screen in
" visual mode. If in normal mode, changed to where the cursor was when the last
" visual selection ended
function! s:exit_visual_mode()
  exec "normal! \<Esc>gv\<Esc>"
endfunction

" Visually select input region, where region is an array containing the start
" and end position. If start is after end, the selection simply goes backwards.
" Typically m<, m>, and gv would be a simple way of accomplishing this, but on
" some systems, the m< and m> marks are not supported. Note that v`` has random
" behavior if `` is the same location as the cursor location.
" Mode change: Normal -> Visual
" Cursor change: Set to end of region
" TODO: Refactor this and s:update_visual_markers
" FIXME: By using m` we're destroying the user's jumplist. We should use a
" different mark and use :keepjump
function! s:select_in_visual_mode(region)
  if a:region[0] == a:region[1]
    normal! v
  else 
    call cursor(a:region[1])
    normal! m`
    call cursor(a:region[0])
    normal! v``
  endif

  " Unselect and reselect it again to properly set the '< and '> markers
  exec "normal! \<Esc>gv"
endfunction

" Update '< and '> to the input region
" Mode change: Normal -> Normal
" Cursor change: Set to the end of the region
function! s:update_visual_markers(region)
  if a:region[0] == a:region[1]
    normal! v
  else 
    call cursor(a:region[1])
    normal! m`
    call cursor(a:region[0])
    normal! v``
  endif
  call s:exit_visual_mode()
endfunction

" Finds the next occurrence of the input text in the current buffer.
" Search is case sensitive
" Mode change: Normal -> Normal
" Cursor change: Set to the end of the match
function! s:find_next(text)
  let pattern = '\V\C'.substitute(escape(a:text, '\'), '\n', '\\n', 'g')
  call search(pattern)
  let start = s:pos('.')
  call search(pattern, 'ce')
  let end = s:pos('.')
  return [start, end]
endfunction

" Highlight the position using the cursor highlight group
function! s:highlight_cursor(pos)
  " Give cursor highlight high priority, to overrule visual selection
  return matchadd(s:hi_group_cursor, '\%'.a:pos[0].'l\%'.a:pos[1].'c', 99999)
endfunction

" Compare two position arrays. Return a negative value if lhs occurs before rhs,
" positive value if after, and 0 if they are the same.
function! s:compare_pos(l, r)
  " If number lines are the same, compare columns
  return a:l[0] ==# a:r[0] ? a:l[1] - a:r[1] : a:l[0] - a:r[0]
endfunction

" Highlight the area bounded by the input region. The logic here really stinks,
" it's frustrating that Vim doesn't have a built in easier way to do this. None
" of the \%V or \%'m solutions work because we need the highlighting to stay for
" multiple places.
function! s:highlight_region(region)
  let s = sort(copy(a:region), "s:compare_pos")
  if s:to_mode ==# 'V'
    let pattern = '\%>'.(s[0][0]-1).'l\%<'.(s[1][0]+1).'l.*\ze.\_$'
  else
    if (s[0][0] == s[1][0])
      " Same line
      let pattern = '\%'.s[0][0].'l\%>'.(s[0][1]-1).'c.*\%<'.(s[1][1]+1).'c.'
    else
      " Two lines
      let s1 = '\%'.s[0][0].'l.\%>'.s[0][1].'c.*'
      let s2 = '\%'.s[1][0].'l.*\%<'.s[1][1].'c..'
      let pattern = s1.'\|'.s2
      " More than two lines
      if (s[1][0] - s[0][0] > 1)
        let pattern = pattern.'\|\%>'.s[0][0].'l\%<'.s[1][0].'l.*\ze.\_$' 
      endif
    endif
  endif
  return matchadd(s:hi_group_visual, pattern)
endfunction

" Perform the operation that's necessary to revert us from one mode to another
function! s:revert_mode(from, to)
  if a:to ==# 'v'
    call s:cm.reapply_visual_selection()
  endif
  if a:to ==# 'V'
    call s:cm.reapply_visual_selection()
    normal! V
  endif
  if a:to ==# 'n' && a:from ==# 'i'
    stopinsert
  endif
endfunction

" Consume all the additional character the user typed between the last
" getchar() and here, to avoid potential race condition.
" TODO(terryma): This solves the problem of cursors getting out of sync, but
" we're potentially losing user input. We COULD replay these characters as
" well...
function! s:feedkeys(keys)
  while 1
    let c = getchar(0)
    " Checking type is important, when strings are compared with integers,
    " strings are always converted to ints, and all strings are equal to 0
    if type(c) == 0 && c == 0
      break
    endif
  endwhile
  call feedkeys(a:keys)
endfunction

" Take the user input and apply it at every cursor
function! s:process_user_input()
  " Grr this is frustrating. In Insert mode, between the feedkey call and here,
  " the current position could actually CHANGE for some odd reason. Forcing a
  " position reset here
  call cursor(s:cm.get_current().position)

  " Before applying the user input, we need to revert back to the mode the user
  " was in when the input was entered
  call s:revert_mode(s:to_mode, s:from_mode)

  " Update the line length BEFORE applying any actions. TODO(terryma): Is there
  " a better place to do this?
  call s:cm.get_current().update_line_length()
  let s:saved_linecount = line('$')

  " Apply the user input. Note that the above could potentially change mode, we
  " use the mapping below to help us determine what the new mode is
  " Note that it's possible that \<Plug>(a) never gets called, we have a
  " detection mechanism using \<Plug>(d). See its documentation for more details

  " Assume that input is not valid
  let s:valid_input = 0

  " If we're coming from insert mode or going into insert mode, always chain the
  " undos together.
  " FIXME(terryma): Undo always places the cursor at the beginning of the line.
  " Figure out why.
  if s:from_mode ==# 'i' || s:to_mode ==# 'i'
    silent! undojoin | call s:feedkeys(s:char."\<Plug>(a)")
  else
    call s:feedkeys(s:char."\<Plug>(a)")
  endif
  
  " Even when s:char produces invalid input, this method is always called. The
  " 't' here is important
  call feedkeys("\<Plug>(d)", 't')
endfunction

" This method is always called during fanout, even when a bad user input causes
" s:apply_user_input_next to not be called. We detect that and force the method
" to be called to continue the fanout process
function! s:detect_bad_input()
  if !s:valid_input
    " We ignore the bad input and force invoke s:apply_user_input_next
    call feedkeys("\<Plug>(a)")
    let s:bad_input += 1
  endif
endfunction

" Apply the user input at the next cursor location
function! s:apply_user_input_next(mode)
  let s:valid_input = 1

  " Save the current mode, only if we haven't already
  if empty(s:to_mode)
    let s:to_mode = a:mode
    if s:to_mode ==# 'v'
      if visualmode() ==# 'V'
        let s:to_mode = 'V'
      endif
    endif
  endif

  " Update the current cursor's information
  let changed = s:cm.update_current()

  " Advance the cursor index
  call s:cm.next()

  " We're done if we're made the full round
  if s:cm.loop_done()
    if s:to_mode ==# 'v' || s:to_mode ==# 'V'
      " This is necessary to set the "'<" and "'>" markers properly
      call s:update_visual_markers(s:cm.get_current().visual)
    endif
    call feedkeys("\<Plug>(w)")
  else
    " Continue to next
    call feedkeys("\<Plug>(i)")
  endif
endfunction

" If pos is equal to the left side of the visual selection, the region start
" from end to start
function! s:get_visual_region(pos)
  let left = s:pos("'<")
  let right = s:pos("'>")
  if a:pos == left
    let region = [right, left]
  else
    let region = [left, right]
  endif
  return region
endfunction

" Return the content of the buffer between the input region. This is used to
" find the next match in the buffer
" Mode change: Normal -> Normal
" Cursor change: None
function! s:get_text(region)
  let lines = getline(a:region[0][0], a:region[1][0])
  let lines[-1] = lines[-1][:a:region[1][1] - 1]
  let lines[0] = lines[0][a:region[0][1] - 1:]
  return join(lines, "\n")
endfunction

" Wrapper around getchar() that returns the string representation of the user
" input
function! s:get_char()
  let c = getchar()
  " If the character is a number, then it's not a special key
  if type(c) == 0
    let c = nr2char(c)
  endif
  return c
endfunction

" Quits multicursor mode and clears all cursors. Return true if exited
" successfully.
function! s:exit()
  if s:char !=# g:multi_cursor_quit_key
    return 0
  endif
  let exit = 0
  if s:from_mode ==# 'n'
    let exit = 1
  elseif (s:from_mode ==# 'v' || s:from_mode ==# 'V') &&
        \ g:multi_cursor_exit_from_visual_mode
    let exit = 1
  elseif s:from_mode ==# 'i' && g:multi_cursor_exit_from_insert_mode
    stopinsert
    let exit = 1
  endif
  if exit
    call s:cm.reset(1, 1)
    return 1
  endif
  return 0
endfunction

" These keys don't get faned out to all cursor locations. Instead, they're used
" to add new / remove existing cursors
" Precondition: The function is only called when the keys and mode respect the
" setting in s:special_keys
function! s:handle_special_key(key, mode)
  " Use feedkeys here instead of calling the function directly to prevent
  " increasing the call stack, since feedkeys execute after the current call
  " finishes
  if a:key == g:multi_cursor_next_key
    call s:feedkeys("\<Plug>(n)")
  elseif a:key == g:multi_cursor_prev_key
    call s:feedkeys("\<Plug>(p)")
  elseif a:key == g:multi_cursor_skip_key
    call s:feedkeys("\<Plug>(s)")
  endif
endfunction

" The last line where the normal Vim cursor is always seems to highlighting
" issues if the cursor is on the last column. Vim's cursor seems to override the
" highlight of the virtual cursor. This won't happen if the virtual cursor isn't
" the last character on the line. This is a hack to add an empty space on the
" Vim cursor line right before we do the redraw, we'll revert the change
" immedidately after the redraw so the change should not be intrusive to the
" user's buffer content
function! s:apply_highlight_fix()
  " Only do this if we're on the last character of the line
  if col('.') == col('$')
    let s:saved_line = getline('.')
    if s:from_mode ==# 'i'
      silent! undojoin | call setline('.', s:saved_line.' ')
    else
      call setline('.', s:saved_line.' ')
    endif
  endif
endfunction

" Revert the fix if it was applied earlier
function! s:revert_highlight_fix()
  if type(s:saved_line) == 1
    if s:from_mode ==# 'i'
      silent! undojoin | call setline('.', s:saved_line)
    else
      call setline('.', s:saved_line)
    endif
  endif
  let s:saved_line = 0
endfunction

function! s:display_error()
  if s:bad_input > 0
    echohl ErrorMsg |
          \ echo "Key '".s:char."' cannot be replayed at ".
          \ s:bad_input." cursor location".(s:bad_input == 1 ? '' : 's') |
          \ echohl Normal
  endif
  let s:bad_input = 0
endfunction

let s:latency_debug_file = ''
function! s:start_latency_measure()
  if g:multi_cursor_debug_latency
    let s:start_time = reltime()
  endif
endfunction

function! s:skip_latency_measure()
  if g:multi_cursor_debug_latency
    let s:skip_latency_measure = 1
  endif
endfunction

function! s:end_latency_measure()
  if g:multi_cursor_debug_latency && !empty(s:char)
    if empty(s:latency_debug_file)
      let s:latency_debug_file = tempname()
      exec 'redir >> '.s:latency_debug_file
        silent! echom "Starting latency debug at ".reltimestr(reltime())
      redir END
    endif

    if !s:skip_latency_measure
      exec 'redir >> '.s:latency_debug_file
        silent! echom "Processing '".s:char."' took ".string(str2float(reltimestr(reltime(s:start_time)))*1000).' ms in '.s:cm.size().' cursors. mode = '.s:from_mode
      redir END
    endif
  endif
  let s:skip_latency_measure = 0
endfunction

function! s:wait_for_user_input(mode)
  let s:from_mode = a:mode
  if empty(a:mode)
    let s:from_mode = s:to_mode
  endif
  let s:to_mode = ''

  call s:display_error()

  " Right before redraw, apply the highlighting bug fix
  call s:apply_highlight_fix()

  redraw

  " Immediately revert the change to leave the user's buffer unchanged
  call s:revert_highlight_fix()

  call s:end_latency_measure()

  let s:char = s:get_char()

  call s:start_latency_measure()

  " Clears any echoes we might've added
  normal! :<Esc>

  if s:exit()
    return
  endif

  " If the key is a special key and we're in the right mode, handle it
  if index(get(s:special_keys, s:from_mode, []), s:char) != -1
    call s:handle_special_key(s:char, s:from_mode)
    call s:skip_latency_measure()
  else
    call s:cm.start_loop()
    call s:feedkeys("\<Plug>(i)")
  endif
endfunction
