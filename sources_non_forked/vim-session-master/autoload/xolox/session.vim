" Public API for the vim-session plug-in.
"
" Author: Peter Odding
" Last Change: November 1, 2015
" URL: http://peterodding.com/code/vim/session/

let g:xolox#session#version = '2.13.1'

" Public API for session persistence. {{{1

function! xolox#session#save_session(commands, filename) " {{{2
  " Save the current Vim editing session to a Vim script using the
  " [:mksession] [] command and some additional Vim magic provided by the
  " vim-session plug-in. When the generated session script is later sourced
  " using the [:source] [] command (possibly in another process or even on
  " another machine) it will restore the editing session to its previous state
  " (provided all of the files involved in the session are still there at
  " their original locations).
  "
  " The first argument is expected to be a list, it will be extended with the
  " lines to be added to the session script. The second argument is expected
  " to be the filename under which the script will later be saved (it's
  " embedded in a comment at the top of the script).
  "
  " [:mksession]: http://vimdoc.sourceforge.net/htmldoc/starting.html#:mksession
  " [:source]: http://vimdoc.sourceforge.net/htmldoc/repeat.html#:source
  let is_all_tabs = xolox#session#include_tabs()
  call add(a:commands, '" ' . a:filename . ':')
  call add(a:commands, '" Vim session script' . (is_all_tabs ? '' : ' for a single tab page') . '.')
  call add(a:commands, '" Created by session.vim ' . g:xolox#session#version . ' on ' . strftime('%d %B %Y at %H:%M:%S.'))
  call add(a:commands, '" Open this file in Vim and run :source % to restore your session.')
  call add(a:commands, '')
  if &verbose >= 1
    call add(a:commands, 'set verbose=' . &verbose)
  endif
  " We save the GUI options only for global sessions, not for tab scoped
  " sessions. Also, if the Vim we're currently running in doesn't have GUI
  " support, Vim will report &go as an empty string. We should never persist
  " this value if the user didn't specifically set it! Otherwise the next time
  " the session is restored in a GUI Vim, things will look funky :-).
  if has('gui') && is_all_tabs
    call add(a:commands, 'set guioptions=' . escape(&go, ' "\'))
    if xolox#misc#option#get('session_persist_font', 1)
      call add(a:commands, 'silent! set guifont=' . escape(&gfn, ' "\'))
    endif
  endif
  call xolox#session#save_globals(a:commands)
  if is_all_tabs
    call xolox#session#save_features(a:commands)
    if g:session_persist_colors
      call xolox#session#save_colors(a:commands)
    endif
  endif
  call xolox#session#save_qflist(a:commands)
  call xolox#session#save_state(a:commands)
  if is_all_tabs
    call xolox#session#save_fullscreen(a:commands)
    call add(a:commands, 'doautoall SessionLoadPost')
  else
    call add(a:commands, 'let s:winrestcmd = winrestcmd()')
    call add(a:commands, 'windo doautocmd SessionLoadPost')
    call s:jump_to_window(a:commands, tabpagenr(), winnr())
    call add(a:commands, 'silent! execute s:winrestcmd')
  endif
  call add(a:commands, 'unlet SessionLoad')
  call add(a:commands, '" vim: ft=vim ro nowrap smc=128')
endfunction

function! xolox#session#save_globals(commands) " {{{2
  " Serialize the values of the global variables configured by the user with
  " the `g:session_persist_globals` option. The first argument is expected to
  " be a list, it will be extended with the lines to be added to the session
  " script.
  for global in g:session_persist_globals
    call add(a:commands, printf("let %s = %s", global, string(eval(global))))
  endfor
endfunction

function! xolox#session#save_features(commands) " {{{2
  " Save the current state of the following Vim features:
  "
  " - Whether syntax highlighting is enabled (`:syntax on`)
  " - Whether file type detection is enabled (`:filetype on`)
  " - Whether file type plug-ins are enabled (`:filetype plugin on`)
  " - Whether file type indent plug-ins are enabled (`:filetype indent on`)
  "
  " The first argument is expected to be a list, it will be extended with the
  " lines to be added to the session script.
  let template = "if exists('%s') != %i | %s %s | endif"
  for [global, command] in [
          \ ['g:syntax_on', 'syntax'],
          \ ['g:did_load_filetypes', 'filetype'],
          \ ['g:did_load_ftplugin', 'filetype plugin'],
          \ ['g:did_indent_on', 'filetype indent']]
    let active = exists(global)
    let toggle = active ? 'on' : 'off'
    call add(a:commands, printf(template, global, active, command, toggle))
  endfor
endfunction

function! xolox#session#save_colors(commands) " {{{2
  " Save the current color scheme and background color. The first argument is
  " expected to be a list, it will be extended with the lines to be added to
  " the session script.
  call add(a:commands, 'if &background != ' . string(&background))
  call add(a:commands, "\tset background=" . &background)
  call add(a:commands, 'endif')
  if exists('g:colors_name') && type(g:colors_name) == type('') && g:colors_name != ''
    let template = "if !exists('g:colors_name') || g:colors_name != %s | colorscheme %s | endif"
    call add(a:commands, printf(template, string(g:colors_name), fnameescape(g:colors_name)))
  endif
endfunction

function! xolox#session#save_fullscreen(commands) " {{{2
  " Save the full screen state of Vim. This function provides integration
  " between my [vim-session] [] and [vim-shell] [] plug-ins. The first
  " argument is expected to be a list, it will be extended with the lines to
  " be added to the session script.
  "
  " [vim-session]: http://peterodding.com/code/vim/session
  " [vim-shell]: http://peterodding.com/code/vim/shell
  try
    let commands = xolox#shell#persist_fullscreen()
    if !empty(commands)
      call add(a:commands, "try")
      for line in commands
        call add(a:commands, "  " . line)
      endfor
      if has('gui_running') && (has('gui_gtk') || has('gui_gtk2') || has('gui_gnome'))
        " Without this hack GVim on GTK doesn't preserve the window size.
        call add(a:commands, "  call feedkeys(\":set lines=" . &lines . " columns=" . &columns . "\\<CR>\")")
      endif
      call add(a:commands, "catch " . '/^Vim\%((\a\+)\)\=:E117/')
      call add(a:commands, "  \" Ignore missing full-screen plug-in.")
      call add(a:commands, "endtry")
    endif
  catch /^Vim\%((\a\+)\)\=:E117/
    " Ignore missing full-screen functionality.
  endtry
endfunction

function! xolox#session#save_qflist(commands) " {{{2
  " Save the contents of the quick-fix list. The first argument is expected to
  " be a list, it will be extended with the lines to be added to the session
  " script.
  if has('quickfix')
    let qf_list = []
    for qf_entry in getqflist()
      if has_key(qf_entry, 'bufnr')
        if !has_key(qf_entry, 'filename')
          let qf_entry.filename = bufname(qf_entry.bufnr)
        endif
        unlet qf_entry.bufnr
      endif
      call add(qf_list, qf_entry)
    endfor
    call add(a:commands, 'call setqflist(' . string(qf_list) . ')')
  endif
endfunction

function! xolox#session#save_state(commands) " {{{2
  " Wrapper for the [:mksession] [] command that slightly massages the
  " generated Vim script to get rid of some strange quirks in the way Vim
  " generates sessions. Also implements support for buffers with content that
  " was generated by other Vim plug-ins. The first argument is expected to
  " be a list, it will be extended with the lines to be added to the session
  " script.
  let tempfile = tempname()
  let ssop_save = &sessionoptions
  try
    " The default value of &sessionoptions includes "options" which causes
    " :mksession to include all Vim options and mappings in generated session
    " scripts. This can significantly increase the size of session scripts
    " which makes them slower to generate and evaluate. It can also be a bit
    " buggy, e.g. it breaks Ctrl-S when :runtime mswin.vim has been used. The
    " value of &sessionoptions is changed temporarily to avoid these issues.
    set ssop-=options
    execute 'mksession' fnameescape(tempfile)
    let lines = readfile(tempfile)
    " Remove the mode line added by :mksession because we'll add our own in
    " xolox#session#save_session().
    call s:eat_trailing_line(lines, '" vim: set ft=vim :')
    " Remove the "SessionLoadPost" event firing at the end of the :mksession
    " output. We will fire the event ourselves when we're really done.
    call s:eat_trailing_line(lines, 'unlet SessionLoad')
    call s:eat_trailing_line(lines, 'doautoall SessionLoadPost')
    call xolox#session#save_special_windows(lines)
    if !xolox#session#include_tabs()
      " Customize the output of :mksession for tab scoped sessions.
      let buffers = tabpagebuflist()
      call map(lines, 's:tabpage_filter(buffers, v:val)')
    endif
    call extend(a:commands, map(lines, 's:state_filter(v:val)'))
    " Re-implement Vim's special handling of the initial, empty buffer.
    call add(a:commands, "if exists('s:wipebuf')")
    call add(a:commands, "  if empty(bufname(s:wipebuf))")
    call s:cleanup_after_plugin(a:commands, 's:wipebuf')
    call add(a:commands, "  endif")
    call add(a:commands, "endif")
    return 1
  finally
    let &sessionoptions = ssop_save
    call delete(tempfile)
  endtry
endfunction

function! s:eat_trailing_line(session, line) " {{{3
  " Remove matching, trailing strings from a list of strings.
  if a:session[-1] == a:line
    call remove(a:session, -1)
  endif
endfunction

function! s:tabpage_filter(buffers, line) " {{{3
  " Change output of :mksession if for single tab page.
  if a:line =~ '^badd +\d\+ '
    " The :mksession command adds all open buffers to a session even for tab
    " scoped sessions. That makes sense, but we want only the buffers in the
    " tab page to be included. That's why we filter out any references to the
    " rest of the buffers from the script generated by :mksession.
    let pathname = matchstr(a:line, '^badd +\d\+ \zs.*')
    let bufnr = bufnr('^' . pathname . '$')
    if index(a:buffers, bufnr) == -1
      return '" ' . a:line
    endif
  elseif a:line =~ '^let v:this_session\s*='
    " The :mksession command unconditionally adds the global v:this_session
    " variable definition to the session script, but we want a differently
    " scoped variable for tab scoped sessions.
    return substitute(a:line, 'v:this_session', 't:this_session', 'g')
  endif
  " Preserve all other lines.
  return a:line
endfunction

function! s:state_filter(line) " {{{3
  " Various changes to the output of :mksession.
  if a:line =~ '^normal!\? zo$'
    " Silence "E490: No fold found" errors.
    return 'silent! ' . a:line
  elseif a:line =~ '^file .\{-}\<NERD_tree_\d\+$'
    " Silence "E95: Buffer with this name already exists" when restoring
    " mirrored NERDTree windows.
    return '" ' . a:line
  elseif a:line =~ '^file .\{-}\[BufExplorer\]$'
    " Same trick (about the E95) for BufExplorer.
    return '" ' . a:line
  elseif a:line =~ '^file .\{-}__Tag_List__$'
    " Same trick (about the E95) for TagList.
    return '" ' . a:line
  elseif a:line =~ "^\\s*silent exe 'bwipe ' \\. s:wipebuf$" || a:line =~ '^unlet! s:wipebuf$'
    " Disable Vim's special handling of the initial, empty buffer because it
    " breaks restoring of special windows with content generated by a Vim
    " plug-in. The :mksession command doesn't have this problem because it
    " simply doesn't support buffers with generated contents...
    return '" ' . a:line
  else
    return a:line
  endif
endfunction

function! xolox#session#save_special_windows(session) " {{{2
  " Implements support for buffers with content that was generated by other
  " Vim plug-ins. The first argument is expected to be a list, it will be
  " extended with the lines to be added to the session script.
  let tabpage = tabpagenr()
  let window = winnr()
  let s:nerdtrees = {}
  call add(a:session, '')
  call add(a:session, '" Support for special windows like quick-fix and plug-in windows.')
  call add(a:session, '" Everything down here is generated by vim-session (not supported')
  call add(a:session, '" by :mksession out of the box).')
  call add(a:session, '')
  try
    if xolox#session#include_tabs()
      tabdo call s:check_special_tabpage(a:session)
    else
      call s:check_special_tabpage(a:session)
    endif
  finally
    unlet s:nerdtrees
    execute 'tabnext' tabpage
    execute window . 'wincmd w'
    call s:jump_to_window(a:session, tabpage, window)
  endtry
endfunction

function! s:check_special_tabpage(session)
  let status = 0
  let winrestcmd = winrestcmd()
  let window = winnr()
  windo let status += s:check_special_window(a:session)
  execute window . 'wincmd w'
  silent! execute winrestcmd
  if status > 0 && winnr('$') > 1
    call add(a:session, winrestcmd)
  endif
endfunction

function! s:check_special_window(session)
  " If we detected a special window and the argument to the command is not a
  " pathname, this variable should be set to false to disable normalization.
  let do_normalize_path = 1
  let bufname = expand('%:t')
  if exists('b:NERDTreeRoot')
    if !has_key(s:nerdtrees, bufnr('%'))
      let command = 'NERDTree'
      let argument = b:NERDTreeRoot.path.str()
      let s:nerdtrees[bufnr('%')] = 1
    else
      let command = 'NERDTreeMirror'
      let argument = ''
    endif
  elseif bufname == '[BufExplorer]'
    let command = 'BufExplorer'
    let argument = ''
  elseif bufname == '__Tag_List__'
    let command = 'Tlist'
    let argument = ''
  elseif exists('g:proj_running') && g:proj_running == bufnr('%')
    let command = 'Project'
    let argument = expand('%:p')
  elseif exists('b:ConqueTerm_Idx')
    let command = 'ConqueTerm'
    let argument = g:ConqueTerm_Terminals[b:ConqueTerm_Idx]['program_name']
    let do_normalize_path = 0
  elseif &filetype == 'netrw'
    let command = 'edit'
    let argument = bufname('%')
  elseif &buftype == 'quickfix'
    let command = 'cwindow'
    let argument = ''
  endif
  if exists('command')
    call s:jump_to_window(a:session, tabpagenr(), winnr())
    call add(a:session, 'let s:bufnr_save = bufnr("%")')
    call add(a:session, 'let s:cwd_save = getcwd()')
    if argument == ''
      call add(a:session, command)
    else
      if do_normalize_path
        let argument = fnamemodify(argument, ':~')
        if xolox#session#options_include('slash')
          let argument = substitute(argument, '\', '/', 'g')
        endif
      endif
      call add(a:session, command . ' ' . fnameescape(argument))
    endif
    call s:cleanup_after_plugin(a:session, 's:bufnr_save')
    call add(a:session, 'execute "cd" fnameescape(s:cwd_save)')
    return 1
  endif
endfunction

function! s:jump_to_window(session, tabpage, window)
  call add(a:session, a:window . 'wincmd w')
  if xolox#session#include_tabs()
    call add(a:session, 'tabnext ' . a:tabpage)
  endif
endfunction

function! s:nerdtree_persist()
  " Remember current working directory and whether NERDTree is loaded.
  if exists('b:NERDTreeRoot')
    return 'NERDTree ' . fnameescape(b:NERDTreeRoot.path.str()) . ' | only'
  else
    return 'cd ' . fnameescape(getcwd())
  endif
endfunction

function! s:cleanup_after_plugin(commands, bufnr_var)
  call add(a:commands, "if !getbufvar(" . a:bufnr_var . ", '&modified')")
  call add(a:commands, "  let s:wipebuflines = getbufline(" . a:bufnr_var . ", 1, '$')")
  call add(a:commands, "  if len(s:wipebuflines) <= 1 && empty(get(s:wipebuflines, 0, ''))")
  call add(a:commands, "    silent execute 'bwipeout' " . a:bufnr_var)
  call add(a:commands, "  endif")
  call add(a:commands, "endif")
endfunction

" Automatic commands to manage the default session. {{{1

function! xolox#session#auto_load() " {{{2
  " Automatically load the default or last used session when Vim starts.
  " Normally called by the [VimEnter] [] automatic command event.
  "
  " [VimEnter]: http://vimdoc.sourceforge.net/htmldoc/autocmd.html#VimEnter
  if g:session_autoload == 'no'
    return
  endif
  " Check that the user has started Vim without editing any files.
  if xolox#session#is_empty()
    " Check whether a session matching the user-specified server name exists.
    if v:servername !~ '^\cgvim\d*$'
      for session in xolox#session#get_names(0)
        if v:servername ==? session
          call xolox#session#open_cmd(session, '', 'OpenSession')
          return
        endif
      endfor
    endif
    " Default to the last used session or the default session?
    let [has_last_session, session] = s:get_last_or_default_session()
    let path = xolox#session#name_to_path(session)
    if (g:session_default_to_last == 0 || has_last_session) && filereadable(path) && !s:session_is_locked(session, 'OpenSession')
      " Compose the message for the prompt.
      let is_default_session = (session == g:session_default_name)
      let msg = printf("Do you want to restore your %s editing session%s?",
            \ is_default_session ? 'default' : 'last used',
            \ is_default_session ? '' : printf(' (%s)', session))
      " Prepare the list of choices.
      let choices = ["&Restore", "&Don't Restore"]
      if g:session_default_to_last && has_last_session
        call add(choices, "&Forget")
      endif
      " Prompt the user (if not configured otherwise).
      let choice = s:prompt(msg, choices, 'g:session_autoload')
      if choice == 1
        call xolox#session#open_cmd(session, '', 'OpenSession')
      elseif choice == 3
        call s:last_session_forget()
      endif
    endif
  endif
endfunction

function! xolox#session#is_empty() " {{{2
  " Check that the user has started Vim without editing any files. Used by
  " `xolox#session#auto_load()` to determine whether automatic session loading
  " should be performed. Currently checks the following conditions:
  "
  " 1. That the current buffer is either empty (contains no lines and is not
  "    modified) or showing [vim-startify] [].
  " 2. That the buffer list either empty or persistent.
  "
  " [vim-startify]: https://github.com/mhinz/vim-startify/
  let current_buffer_is_empty = (&modified == 0 && getline(1, '$') == [''])
  let current_buffer_is_startify = (&filetype == 'startify')
  let buffer_list_is_empty = (bufname('%') == '' && empty(filter(range(1, bufnr('$')), 'buflisted(v:val) && v:val != ' . bufnr(''))))
  let buffer_list_is_persistent = (index(xolox#misc#option#split(&viminfo), '%') >= 0)
  return (current_buffer_is_empty || current_buffer_is_startify) && (buffer_list_is_empty || buffer_list_is_persistent)
endfunction

function! xolox#session#auto_save() " {{{2
  " Automatically save the current editing session when Vim is closed.
  " Normally called by the [VimLeavePre] [] automatic command event.
  "
  " [VimLeavePre]: http://vimdoc.sourceforge.net/htmldoc/autocmd.html#VimLeavePre
  if v:dying
    " We won't save the session if Vim is not terminating normally.
    return
  endif
  if g:session_autosave == 'no'
    " We won't save the session if auto-save is explicitly disabled.
    return
  endif
  " Get the name of the session for automatic saving.
  let name = xolox#misc#option#get('session_autosave_to')
  if empty(name)
    " Get the name of the active session (if any).
    let name = xolox#session#find_current_session()
    " If no session is active and the user doesn't have any sessions yet,
    " help them get started by suggesting to create the default session.
    if empty(name) && (empty(xolox#session#get_names(0)) || g:session_default_overwrite)
      let name = g:session_default_name
    endif
  endif
  " Prompt the user to save the active/first/default session?
  if !empty(name)
    let is_tab_scoped = xolox#session#is_tab_scoped()
    let msg = "Do you want to save your %s before quitting Vim?"
    if s:prompt(printf(msg, xolox#session#get_label(name, is_tab_scoped)), ["&Save", "&Don't Save"], 'g:session_autosave') == 1
      if g:session_default_overwrite && (name == g:session_default_name)
        let bang = '!'
      else
        let bang = ''
      endif
      if is_tab_scoped
        call xolox#session#save_tab_cmd(name, bang, 'SaveTabSession')
      else
        call xolox#session#save_cmd(name, bang, 'SaveSession')
      endif
    endif
  endif
endfunction

function! xolox#session#auto_save_periodic() " {{{2
  " Automatically saves the current editing session every few minutes.
  " Normally called by the [CursorHold] [] and [CursorHoldI] [] automatic
  " command events.
  "
  " [CursorHold]: http://vimdoc.sourceforge.net/htmldoc/autocmd.html#CursorHold
  " [CursorHoldI]: http://vimdoc.sourceforge.net/htmldoc/autocmd.html#CursorHoldI
  if g:session_autosave_periodic > 0
    let interval = g:session_autosave_periodic * 60
    let next_save = s:session_last_flushed + interval
    if localtime() > next_save
      let name = xolox#session#find_current_session()
      if !empty(name)
        if xolox#session#is_tab_scoped()
          let function = 'xolox#session#save_tab_cmd'
          let arguments = [name, '', 'SaveTabSession']
        else
          let function = 'xolox#session#save_cmd'
          let arguments = [name, '', 'SaveSession']
        endif
        if xolox#misc#option#get('session_autosave_silent', 0)
          " Silence informational messages perceived as noisy.
          " https://github.com/xolox/vim-session/issues/120
          silent call call(function, arguments)
        else
          call call(function, arguments)
        endif
      endif
    endif
  endif
endfunction

function! s:flush_session()
  let s:session_last_flushed = localtime()
endfunction

if !exists('s:session_last_flushed')
  call s:flush_session()
endif

function! xolox#session#auto_unlock() " {{{2
  " Automatically unlock all sessions when Vim quits. Normally called by the
  " [VimLeavePre] [] automatic command event.
  "
  " [VimLeavePre]: http://vimdoc.sourceforge.net/htmldoc/autocmd.html#VimLeavePre
  if xolox#session#locking_enabled()
    let i = 0
    while i < len(s:lock_files)
      let lock_file = s:lock_files[i]
      if delete(lock_file) == 0
        call remove(s:lock_files, i)
      else
        let i += 1
      endif
    endwhile
  endif
endfunction

" Commands that enable users to manage multiple sessions. {{{1

function! s:prompt(msg, choices, option_name)
  let option_value = eval(a:option_name)
  if option_value == 'yes'
    return 1
  elseif option_value == 'no'
    return 0
  else
    if g:session_verbose_messages
      let format = "%s\n\nNote that you can permanently disable this dialog by adding the following line to your %s script:\n\n\t:let %s = 'no'"
      let prompt = printf(format, a:msg, xolox#misc#os#is_win() ? '~\_vimrc' : '~/.vimrc', a:option_name)
    else
      let prompt = a:msg
    endif
    return confirm(prompt, join(a:choices, "\n"), 1, 'Question')
  endif
endfunction

function! xolox#session#open_cmd(name, bang, command) abort " {{{2
  let name = s:unescape(a:name)
  if empty(name)
    let name = xolox#session#prompt_for_name('restore')
  endif
  if name == ''
    return -1
  else
    let starttime = xolox#misc#timer#start()
    let path = xolox#session#name_to_path(name)
    if !filereadable(path)
      let msg = "session.vim %s: The %s session at %s doesn't exist!"
      call xolox#misc#msg#warn(msg, g:xolox#session#version, string(name), fnamemodify(path, ':~'))
      return 0
    elseif a:bang == '!' || !s:session_is_locked(name, a:command)
      let oldcwd = s:nerdtree_persist()
      call xolox#session#close_cmd(a:bang, 1, name != xolox#session#find_current_session(), a:command)
      call s:lock_session(path)
      execute 'source' fnameescape(path)
      if xolox#session#is_tab_scoped()
        call s:lock_session(path) " Retroactively (this is only known after the session has been loaded) add the tabpage to the lock.
        let t:session_old_cwd = oldcwd
        let session_type = 'tab scoped'
      else
        let g:session_old_cwd = oldcwd
        let session_type = 'global'
      endif
      call s:last_session_persist(name)
      call s:flush_session()
      call xolox#misc#timer#stop("session.vim %s: Opened %s %s session in %s.", g:xolox#session#version, session_type, string(name), starttime)
      call xolox#misc#msg#info("session.vim %s: Opened %s %s session from %s.", g:xolox#session#version, session_type, string(name), fnamemodify(path, ':~'))
    endif
  endif
  return 1
endfunction

function! xolox#session#view_cmd(name) abort " {{{2
  let name = s:unescape(a:name)
  " Default to the current session?
  if empty(name)
    let name = xolox#session#find_current_session()
  endif
  " Prompt the user to select a session.
  if empty(name)
    let name = xolox#session#prompt_for_name('view')
  endif
  if name != ''
    let path = xolox#session#name_to_path(name)
    if !filereadable(path)
      let msg = "session.vim %s: The %s session at %s doesn't exist!"
      call xolox#misc#msg#warn(msg, g:xolox#session#version, string(name), fnamemodify(path, ':~'))
    else
      if has('gui_running')
        execute 'tab drop' fnameescape(path)
      else
        execute 'tabedit' fnameescape(path)
      endif
      call xolox#misc#msg#info("session.vim %s: Viewing session script %s.", g:xolox#session#version, fnamemodify(path, ':~'))
    endif
  endif
endfunction

function! xolox#session#save_cmd(name, bang, command) abort " {{{2
  let starttime = xolox#misc#timer#start()
  let name = s:unescape(a:name)
  if empty(name)
    let name = xolox#session#find_current_session()
  endif
  if empty(name)
    let name = g:session_default_name
  endif
  let path = xolox#session#name_to_path(name)
  let friendly_path = fnamemodify(path, ':~')
  if a:bang == '!' || !s:session_is_locked(name, a:command)
    let lines = []
    call xolox#session#save_session(lines, friendly_path)
    if xolox#misc#os#is_win() && !xolox#session#options_include('unix')
      call map(lines, 'v:val . "\r"')
    endif
    if writefile(lines, path) != 0
      let msg = "session.vim %s: Failed to save %s session to %s!"
      call xolox#misc#msg#warn(msg, g:xolox#session#version, string(name), friendly_path)
    else
      call s:last_session_persist(name)
      call s:flush_session()
      let label = xolox#session#get_label(name, !xolox#session#include_tabs())
      call xolox#misc#timer#stop("session.vim %s: Saved %s in %s.", g:xolox#session#version, label, starttime)
      call xolox#misc#msg#info("session.vim %s: Saved %s to %s.", g:xolox#session#version, label, friendly_path)
      if xolox#session#include_tabs()
        let v:this_session = path
      else
        let t:this_session = path
      endif
      call s:lock_session(path)
    endif
  endif
endfunction

function! xolox#session#delete_cmd(name, bang) " {{{2
  let name = s:unescape(a:name)
  if empty(name)
    let name = xolox#session#prompt_for_name('delete')
  endif
  if name != ''
    let path = xolox#session#name_to_path(name)
    if !filereadable(path)
      let msg = "session.vim %s: The %s session at %s doesn't exist!"
      call xolox#misc#msg#warn(msg, g:xolox#session#version, string(name), fnamemodify(path, ':~'))
    elseif a:bang == '!' || !s:session_is_locked(name, 'DeleteSession')
      if delete(path) != 0
        let msg = "session.vim %s: Failed to delete %s session at %s!"
        call xolox#misc#msg#warn(msg, g:xolox#session#version, string(name), fnamemodify(path, ':~'))
      else
        call s:unlock_session(path)
        let msg = "session.vim %s: Deleted %s session at %s."
        call xolox#misc#msg#info(msg, g:xolox#session#version, string(name), fnamemodify(path, ':~'))
      endif
    endif
  endif
endfunction

function! xolox#session#close_cmd(bang, silent, save_allowed, command) abort " {{{2
  let is_all_tabs = xolox#session#include_tabs()
  let name = xolox#session#find_current_session()
  if name != ''
    if a:save_allowed
      let msg = "Do you want to save your %s before closing it?"
      let label = xolox#session#get_label(name, !is_all_tabs)
      if s:prompt(printf(msg, label), ["&Save", "&Don't Save"], 'g:session_autosave') == 1
        call xolox#session#save_cmd(name, a:bang, a:command)
      endif
    else
      call xolox#misc#msg#debug("session.vim %s: Session reset requested, not saving changes to session ..", g:xolox#session#version)
    endif
    call s:unlock_session(xolox#session#name_to_path(name))
  endif
  " Close al but the current tab page?
  if is_all_tabs && tabpagenr('$') > 1
    execute 'tabonly' . a:bang
  endif
  " Close all but the current window.
  if winnr('$') > 1
    execute 'only' . a:bang
  endif
  " Start editing a new, empty buffer.
  execute 'enew' . a:bang
  " Close all but the current buffer.
  let bufnr_save = bufnr('%')
  let all_buffers = is_all_tabs ? range(1, bufnr('$')) : tabpagebuflist()
  for bufnr in all_buffers
    if buflisted(bufnr) && bufnr != bufnr_save
      execute 'silent bdelete' bufnr
    endif
  endfor
  " Restore working directory (and NERDTree?) from before :OpenSession.
  if is_all_tabs && exists('g:session_old_cwd')
    execute g:session_old_cwd
    unlet g:session_old_cwd
  elseif !is_all_tabs && exists('t:session_old_cwd')
    execute t:session_old_cwd
    unlet t:session_old_cwd
  endif
  call s:flush_session()
  if !a:silent
    let msg = "session.vim %s: Closed %s."
    let label = xolox#session#get_label(xolox#session#find_current_session(), !is_all_tabs)
    call xolox#misc#msg#info(msg, g:xolox#session#version, label)
  endif
  if xolox#session#is_tab_scoped()
    let t:this_session = ''
  else
    let v:this_session = ''
  endif
  return 1
endfunction

function! xolox#session#open_tab_cmd(name, bang, command) abort " {{{2
  try
    call xolox#session#change_tab_options()
    call xolox#session#open_cmd(a:name, a:bang, a:command)
  finally
    call xolox#session#restore_tab_options()
  endtry
endfunction

function! xolox#session#save_tab_cmd(name, bang, command) abort " {{{2
  try
    call xolox#session#change_tab_options()
    call xolox#session#save_cmd(a:name, a:bang, a:command)
  finally
    call xolox#session#restore_tab_options()
  endtry
endfunction

function! xolox#session#append_tab_cmd(name, bang, count, command) abort " {{{2
  try
    call xolox#session#change_tab_options()
    let original_tabpage = tabpagenr()
    execute printf('%stabnew', a:count == -1 ? '' : a:count)
    let status = xolox#session#open_cmd(a:name, a:bang, a:command)
    if status <= 0 && empty(bufname(''))
      tabclose
      if tabpagenr() != original_tabpage
        execute original_tabpage . 'tabnext'
      endif
      if status == 0
        " Switching tab pages cleared the warning message. Repeat it now.
        call xolox#misc#msg#warn(get(g:xolox_messages, -1, ''))
      endif
    endif
  finally
    call xolox#session#restore_tab_options()
  endtry
endfunction

function! xolox#session#close_tab_cmd(bang, command) abort " {{{2
  let save_allowed = xolox#session#is_tab_scoped()
  try
    call xolox#session#change_tab_options()
    call xolox#session#close_cmd(a:bang, 0, save_allowed, a:command)
  finally
    call xolox#session#restore_tab_options()
  endtry
endfunction

function! xolox#session#restart_cmd(bang, args) abort " {{{2
  if !has('gui_running')
    " In console Vim we can't start a new Vim and kill the old one...
    let msg = "session.vim %s: The :RestartVim command only works in graphical Vim!"
    call xolox#misc#msg#warn(msg, g:xolox#session#version)
  else
    " Save the current session (if there is no active
    " session we will create a session called "restart").
    let name = xolox#session#find_current_session()
    if empty(name)
      let name = 'restart'
    endif
    call xolox#session#save_cmd(name, a:bang, 'RestartVim')
    " Generate the Vim command line.
    let progname = xolox#misc#escape#shell(xolox#misc#os#find_vim())
    let command = progname . ' -g -c ' . xolox#misc#escape#shell('OpenSession\! ' . fnameescape(name))
    let args = matchstr(a:args, '^\s*|\s*\zs.\+$')
    if !empty(args)
      let command .= ' -c ' . xolox#misc#escape#shell(args)
    endif
    if !empty(v:servername)
      let command .= ' --servername ' . xolox#misc#escape#shell(v:servername)
    endif
    " Close the session, releasing the session lock.
    call xolox#session#close_cmd(a:bang, 0, 1, 'RestartVim')
    " Start the new Vim instance.
    if xolox#misc#os#is_win()
      " On Microsoft Windows.
      execute '!start' command
    else
      " On anything other than Windows (UNIX like).
      let cmdline = []
      for variable in g:session_restart_environment
        call add(cmdline, variable . '=' . xolox#misc#escape#shell(fnameescape(eval('$' . variable))))
      endfor
      call add(cmdline, command)
      call add(cmdline, printf("--cmd ':set enc=%s'", escape(&enc, '\ ')))
      silent execute '!' join(cmdline, ' ') '&'
    endif
    " Close Vim.
    silent quitall
  endif
endfunction

" Miscellaneous functions. {{{1

function! s:unescape(s) " {{{2
  " Undo escaping of special characters (preceded by a backslash).
  let s = substitute(a:s, '\\\(.\)', '\1', 'g')
  " Expand any environment variables in the user input.
  let s = substitute(s, '\(\$[A-Za-z0-9_]\+\)', '\=expand(submatch(1))', 'g')
  return s
endfunction

function! xolox#session#prompt_for_name(action) " {{{2
  " Prompt the user to select one of the existing sessions. The first argument
  " is expected to be a string describing what will be done to the session
  " once it's been selected. Returns the name of the selected session as a
  " string. If no session is selected an empty string is returned. Here's
  " an example of what the prompt looks like:
  "
  "     :call xolox#session#prompt_for_name('trash')
  "
  "     Please select the session to trash:
  "
  "      1. first-session
  "      2. second-session
  "      3. third-session
  "
  "     Type number and <Enter> or click with mouse (empty cancels):
  "
  " If only a single session exists there's nothing to choose from so the name
  " of that session will be returned directly, without prompting the user.
  let sessions = sort(xolox#session#get_names(0), 1)
  if len(sessions) == 1
    return sessions[0]
  elseif !empty(sessions)
    let lines = copy(sessions)
    for i in range(len(sessions))
      let lines[i] = ' ' . (i + 1) . '. ' . lines[i]
    endfor
    redraw
    sleep 100 m
    echo "\nPlease select the session to " . a:action . ":"
    sleep 100 m
    let i = inputlist([''] + lines + [''])
    if i >= 1 && i <= len(sessions)
      return sessions[i - 1]
    endif
  endif
  return ''
endfunction

function! xolox#session#name_to_path(name) " {{{2
  " Convert the name of a session (the first argument, expected to be a
  " string) to an absolute pathname. Any special characters in the session
  " name will be encoded using URL encoding. This means you're free to use
  " whatever naming conventions you like (regardless of special characters
  " like slashes). Returns a string.
  let directory = xolox#misc#path#absolute(g:session_directory)
  let filename = xolox#misc#path#encode(a:name) . g:session_extension
  return xolox#misc#path#merge(directory, filename)
endfunction

function! xolox#session#path_to_name(path) " {{{2
  " Convert the absolute pathname of a session script (the first argument,
  " expected to be a string) to a session name. This function assumes the
  " absolute pathname refers to the configured session directory, but it does
  " not check for it nor does it require it (it simple takes the base name
  " of the absolute pathname of the session script and decodes it). Returns a
  " string.
  return xolox#misc#path#decode(fnamemodify(a:path, ':t:r'))
endfunction

function! xolox#session#get_names(include_suggestions) " {{{2
  " Get the names of all available sessions. This scans the directory
  " configured with `g:session_directory` for files that end with the suffix
  " configured with `g:session_extension`, takes the base name of each file
  " and decodes any URL encoded characters. Returns a list of strings.
  "
  " If the first argument is true (1) then the user defined function
  " configured with `g:session_name_suggestion_function` is called to find
  " suggested session names, which are prefixed to the list of available
  " sessions, otherwise the argument should be false (0).
  let directory = xolox#misc#path#absolute(g:session_directory)
  let filenames = split(glob(xolox#misc#path#merge(directory, '*' . g:session_extension)), "\n")
  call map(filenames, 'xolox#session#path_to_name(v:val)')
  if a:include_suggestions && !empty(g:session_name_suggestion_function)
    let suggested_names = call(g:session_name_suggestion_function, [])
    let filenames = suggested_names + filenames
  endif
  return filenames
endfunction

function! xolox#session#complete_names(arg, line, pos) " {{{2
  " Completion function for user defined Vim commands. Used by commands like
  " `:OpenSession` and `:DeleteSession`  (but not `:SaveSession`) to support
  " user friendly completion.
  let names = filter(xolox#session#get_names(0), 'v:val =~ a:arg')
  return map(names, 'fnameescape(v:val)')
endfunction

function! xolox#session#complete_names_with_suggestions(arg, line, pos) " {{{2
  " Completion function for the Vim command `:SaveSession`.
  let names = filter(xolox#session#get_names(1), 'v:val =~ a:arg')
  return map(names, 'fnameescape(v:val)')
endfunction

function! xolox#session#is_tab_scoped() " {{{2
  " Determine whether the current session is tab scoped or global. Returns 1
  " (true) when the session is tab scoped, 0 (false) otherwise.
  return exists('t:this_session')
endfunction

function! xolox#session#find_current_session() " {{{2
  " Find the name of the current tab scoped or global session. Returns a
  " string. If no session is active an empty string is returned.
  for variable in ['t:this_session', 'v:this_session']
    if exists(variable)
      let filename = eval(variable)
      if !empty(filename)
        let directory = fnamemodify(filename, ':p:h')
        if xolox#misc#path#equals(directory, g:session_directory)
          return xolox#session#path_to_name(filename)
        endif
      endif
    endif
  endfor
  return ''
endfunction

function! xolox#session#get_label(name, is_tab_scoped) " {{{2
  " Get a human readable label based on the scope (tab scoped or global) and
  " name of a session. The first argument is the name (a string) and the
  " second argument is a boolean indicating the scope of the session; 1 (true)
  " means tab scoped and 0 (false) means global scope. Returns a string.
  if a:name == g:session_default_name
    let description = 'default editing session'
  else
    let description = printf('editing session %s', string(a:name))
  endif
  if a:is_tab_scoped
    let description = printf('tab scoped %s', description)
  endif
  return description
endfunction

function! xolox#session#options_include(value) " {{{2
  " Check whether Vim's [sessionoptions] [] option includes the keyword given
  " as the first argument (expected to be a string). Returns 1 (true) when it
  " does, 0 (false) otherwise.
  "
  " [sessionoptions]: http://vimdoc.sourceforge.net/htmldoc/options.html#'sessionoptions'
  return index(xolox#misc#option#split(&sessionoptions), a:value) >= 0
endfunction

" Tab scoped sessions: {{{2

function! xolox#session#include_tabs() " {{{3
  " Check whether Vim's [sessionoptions] [] option includes the `tabpages`
  " keyword. Returns 1 (true) when it does, 0 (false) otherwise.
  return xolox#session#options_include('tabpages')
endfunction

function! xolox#session#change_tab_options() " {{{3
  " Temporarily change Vim's [sessionoptions] [] option so we can save a tab
  " scoped session. Saves a copy of the original value to be restored later.
  let s:ssop_save = &sessionoptions
  " Only persist the current tab page.
  set sessionoptions-=tabpages
  " Don't persist the size and position of the Vim window.
  set ssop-=winpos ssop-=resize
endfunction

function! xolox#session#restore_tab_options() " {{{3
  " Restore the original value of Vim's [sessionoptions] [] option.
  if exists('s:ssop_save')
    let &ssop = s:ssop_save
    unlet s:ssop_save
  endif
endfunction

" Default to last used session: {{{2

function! s:last_session_file()
  let directory = xolox#misc#path#absolute(g:session_directory)
  return xolox#misc#path#merge(directory, 'last-session.txt')
endfunction

function! s:last_session_persist(name)
  if g:session_default_to_last
    if writefile([a:name], s:last_session_file()) != 0
      call xolox#misc#msg#warn("session.vim %s: Failed to persist name of last used session!", g:xolox#session#version)
    endif
  endif
endfunction

function! s:last_session_forget()
  let last_session_file = s:last_session_file()
  if filereadable(last_session_file) && delete(last_session_file) != 0
    call xolox#misc#msg#warn("session.vim %s: Failed to delete name of last used session!", g:xolox#session#version)
  endif
endfunction

function! s:get_last_or_default_session()
  let last_session_file = s:last_session_file()
  let has_last_session = filereadable(last_session_file)
  if g:session_default_to_last && has_last_session
    let lines = readfile(last_session_file)
    return [has_last_session, lines[0]]
  else
    return [has_last_session, g:session_default_name]
  endif
endfunction

" Lock file management: {{{2

if !exists('s:lock_files')
  let s:lock_files = []
endif

function! xolox#session#locking_enabled()
  " Check whether session locking is enabled. Returns true (1) when locking is
  " enabled, false (0) otherwise.
  "
  " By default session locking is enabled but users can opt-out by setting
  " `g:session_lock_enabled` to false (0).
  return xolox#misc#option#get('session_lock_enabled', 1)
endfunction

function! s:vim_instance_id()
  let id = {'pid': getpid()}
  if !empty(v:servername)
    let id['servername'] = v:servername
  endif
  if !xolox#session#include_tabs() || xolox#session#is_tab_scoped()
    let id['tabpage'] = tabpagenr()
  endif
  return id
endfunction

function! s:lock_file_path(session_path)
  let directory = xolox#misc#option#get('session_lock_directory', '')
  if empty(directory)
    " Stale lock files can be really annoying, especially after a reboot
    " because that just shouldn't happen - it's always a bug. References:
    "  - https://github.com/xolox/vim-session/issues/97
    "  - https://github.com/xolox/vim-session/issues/110
    " One simple way to give a large group of users what they want is to use a
    " volatile directory that is specifically meant for storing lock files.
    " I've decided to make this the default when possible. The best reference
    " I've been able to find on the proper system wide location for lock files
    " is the following (yes, I know, it's Linux specific, so sue me):
    " http://www.tldp.org/LDP/Linux-Filesystem-Hierarchy/html/var.html
    let global_lock_directory = '/var/lock'
    if filewritable(global_lock_directory) == 2
      let directory = global_lock_directory
    endif
  endif
  if !empty(directory)
    let pathname = xolox#misc#path#merge(directory, xolox#misc#path#encode(a:session_path))
  else
    let pathname = a:session_path
  endif
  return pathname . '.lock'
endfunction

function! s:lock_session(session_path)
  if !xolox#session#locking_enabled()
    return 1
  endif
  let lock_file = s:lock_file_path(a:session_path)
  if xolox#misc#persist#save(lock_file, s:vim_instance_id())
    if index(s:lock_files, lock_file) == -1
      call add(s:lock_files, lock_file)
    endif
    return 1
  endif
endfunction

function! s:unlock_session(session_path)
  if !xolox#session#locking_enabled()
    return 1
  endif
  let lock_file = s:lock_file_path(a:session_path)
  if delete(lock_file) == 0
    let idx = index(s:lock_files, lock_file)
    if idx >= 0
      call remove(s:lock_files, idx)
    endif
    return 1
  endif
endfunction

function! s:session_is_locked(session_name, command)
  if !xolox#session#locking_enabled()
    return 0
  endif
  let session_path = xolox#session#name_to_path(a:session_name)
  let lock_file = s:lock_file_path(session_path)
  if filereadable(lock_file)
    let this_instance = s:vim_instance_id()
    let other_instance = xolox#misc#persist#load(lock_file)
    let arguments = [g:xolox#session#version, string(a:session_name)]
    if this_instance == other_instance
      " Session belongs to current Vim instance and tab page.
      return 0
    elseif this_instance['pid'] == other_instance['pid']
      if has_key(other_instance, 'tabpage')
        let msg = "session.vim %s: The %s session is already loaded in tab page %s."
        call add(arguments, other_instance['tabpage'])
      else
        let msg = "session.vim %s: The %s session is already loaded in this Vim."
      endif
    else
      let msg = "session.vim %s: The %s session is locked by another Vim instance %s."
      if has_key(other_instance, 'servername')
        call add(arguments, 'named ' . other_instance['servername'])
      else
        call add(arguments, 'with PID ' . other_instance['pid'])
      endif
      let msg .= " If that doesn't seem right maybe you forcefully closed Vim or it crashed?"
    endif
    let msg .= " Use the command ':%s! %s' to override."
    call extend(arguments, [a:command, a:session_name])
    call call('xolox#misc#msg#warn', [msg] + arguments)
    return 1
  endif
endfunction

" vim: ts=2 sw=2 et
