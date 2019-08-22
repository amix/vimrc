" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

let s:go_stack = []
let s:go_stack_level = 0

function! go#def#Jump(mode, type) abort
  let l:fname = fnamemodify(expand("%"), ':p:gs?\\?/?')

  " so guru right now is slow for some people. previously we were using
  " godef which also has it's own quirks. But this issue come up so many
  " times I've decided to support both. By default we still use guru as it
  " covers all edge cases, but now anyone can switch to godef if they wish
  let bin_name = go#config#DefMode()
  if bin_name == 'godef'
    let l:cmd = ['godef',
          \ '-f=' . l:fname,
          \ '-o=' . go#util#OffsetCursor(),
          \ '-t']

    if &modified
      let l:stdin_content = join(go#util#GetLines(), "\n")
      call add(l:cmd, "-i")
      let [l:out, l:err] = go#util#ExecInDir(l:cmd, l:stdin_content)
    else
      let [l:out, l:err] = go#util#ExecInDir(l:cmd)
    endif
  elseif bin_name == 'guru'
    let cmd = [go#path#CheckBinPath(bin_name)]
    let buildtags = go#config#BuildTags()
    if buildtags isnot ''
      let cmd += ['-tags', buildtags]
    endif

    let stdin_content = ""

    if &modified
      let content = join(go#util#GetLines(), "\n")
      let stdin_content = fname . "\n" . strlen(content) . "\n" . content
      call add(cmd, "-modified")
    endif

    call extend(cmd, ["definition", fname . ':#' . go#util#OffsetCursor()])

    if go#util#has_job()
      let l:state = {}
      let l:spawn_args = {
            \ 'cmd': cmd,
            \ 'complete': function('s:jump_to_declaration_cb', [a:mode, bin_name], l:state),
            \ 'for': '_',
            \ 'statustype': 'searching declaration',
            \ }

      if &modified
        let l:spawn_args.input = stdin_content
      endif

      call s:def_job(spawn_args, l:state)
      return
    endif

    if &modified
      let [l:out, l:err] = go#util#ExecInDir(l:cmd, l:stdin_content)
    else
      let [l:out, l:err] = go#util#ExecInDir(l:cmd)
    endif
  elseif bin_name == 'gopls'
    let [l:line, l:col] = go#lsp#lsp#Position()
    " delegate to gopls, with an empty job object and an exit status of 0
    " (they're irrelevant for gopls).
    if a:type
      call go#lsp#TypeDef(l:fname, l:line, l:col, function('s:jump_to_declaration_cb', [a:mode, 'gopls', {}, 0]))
    else
      call go#lsp#Definition(l:fname, l:line, l:col, function('s:jump_to_declaration_cb', [a:mode, 'gopls', {}, 0]))
    endif
    return
  else
    call go#util#EchoError('go_def_mode value: '. bin_name .' is not valid. Valid values are: [godef, guru, gopls]')
    return
  endif

  if l:err
    call go#util#EchoError(out)
    return
  endif

  call go#def#jump_to_declaration(out, a:mode, bin_name)
endfunction

function! s:jump_to_declaration_cb(mode, bin_name, job, exit_status, data) abort dict
  if a:exit_status != 0
    return
  endif

  call go#def#jump_to_declaration(a:data[0], a:mode, a:bin_name)

  " capture the active window so that callbacks for jobs, exit_cb and
  " close_cb, and callbacks for gopls can return to it when a:mode caused a
  " split.
  let self.winid = win_getid(winnr())
endfunction

" go#def#jump_to_declaration parses out (expected to be
" 'filename:line:col: message').
function! go#def#jump_to_declaration(out, mode, bin_name) abort
  let final_out = a:out
  if a:bin_name == "godef"
    " append the type information to the same line so it will be parsed
    " correctly using guru's output format.
    " This makes it compatible with guru output.
    let final_out = join(split(a:out, '\n'), ':')
  endif

  " strip line ending
  let out = split(final_out, go#util#LineEnding())[0]
  if go#util#IsWin()
    let parts = split(out, '\(^[a-zA-Z]\)\@<!:')
  else
    let parts = split(out, ':')
  endif

  if len(parts) == 0
    call go#util#EchoError('go jump_to_declaration '. a:bin_name .' output is not valid.')
    return
  endif

  let line = 1
  let col = 1
  let ident = 0
  let filename = parts[0]
  if len(parts) > 1
    let line = parts[1]
  endif
  if len(parts) > 2
    let col = parts[2]
  endif
  if len(parts) > 3
    let ident = parts[3]
  endif

  " Remove anything newer than the current position, just like basic
  " vim tag support
  if s:go_stack_level == 0
    let s:go_stack = []
  else
    let s:go_stack = s:go_stack[0:s:go_stack_level-1]
  endif

  " increment the stack counter
  let s:go_stack_level += 1

  " push it on to the jumpstack
  let stack_entry = {'line': line("."), 'col': col("."), 'file': expand('%:p'), 'ident': ident}
  call add(s:go_stack, stack_entry)

  " needed for restoring back user setting this is because there are two
  " modes of switchbuf which we need based on the split mode
  let old_switchbuf = &switchbuf

  normal! m'
  if filename != fnamemodify(expand("%"), ':p:gs?\\?/?')
    " jump to existing buffer if, 1. we have enabled it, 2. the buffer is loaded
    " and 3. there is buffer window number we switch to
    if go#config#DefReuseBuffer() && bufloaded(filename) != 0 && bufwinnr(filename) != -1
      " jumpt to existing buffer if it exists
      execute bufwinnr(filename) . 'wincmd w'
    else
      if &modified
        let cmd = 'hide edit'
      else
        let cmd = 'edit'
      endif

      if a:mode == "tab"
        let &switchbuf = "useopen,usetab,newtab"
        if bufloaded(filename) == 0
          tab split
        else
           let cmd = 'sbuf'
        endif
      elseif a:mode == "split"
        split
      elseif a:mode == "vsplit"
        vsplit
      endif

      " open the file and jump to line and column
      exec cmd fnameescape(fnamemodify(filename, ':.'))
    endif
  endif
  call cursor(line, col)

  " also align the line to middle of the view
  normal! zz

  let &switchbuf = old_switchbuf
endfunction

function! go#def#SelectStackEntry() abort
  let target_window = go#ui#GetReturnWindow()
  if empty(target_window)
    let target_window = winnr()
  endif

  let highlighted_stack_entry = matchstr(getline("."), '^..\zs\(\d\+\)')
  if !empty(highlighted_stack_entry)
    execute target_window . "wincmd w"
    call go#def#Stack(str2nr(highlighted_stack_entry))
  endif

  call go#ui#CloseWindow()
endfunction

function! go#def#StackUI() abort
  if len(s:go_stack) == 0
    call go#util#EchoError("godef stack empty")
    return
  endif

  let stackOut = ['" <Up>,<Down>:navigate <Enter>:jump <Esc>,q:exit']

  let i = 0
  while i < len(s:go_stack)
    let entry = s:go_stack[i]
    let prefix = ""

    if i == s:go_stack_level
      let prefix = ">"
    else
      let prefix = " "
    endif

    call add(stackOut, printf("%s %d %s|%d col %d|%s",
          \ prefix, i+1, entry["file"], entry["line"], entry["col"], entry["ident"]))
    let i += 1
  endwhile

  if s:go_stack_level == i
    call add(stackOut, "> ")
  endif

  call go#ui#OpenWindow("GoDef Stack", stackOut, "godefstack")

  noremap <buffer> <silent> <CR>  :<C-U>call go#def#SelectStackEntry()<CR>
  noremap <buffer> <silent> <Esc> :<C-U>call go#ui#CloseWindow()<CR>
  noremap <buffer> <silent> q     :<C-U>call go#ui#CloseWindow()<CR>
endfunction

function! go#def#StackClear(...) abort
  let s:go_stack = []
  let s:go_stack_level = 0
endfunction

function! go#def#StackPop(...) abort
  if len(s:go_stack) == 0
    call go#util#EchoError("godef stack empty")
    return
  endif

  if s:go_stack_level == 0
    call go#util#EchoError("at bottom of the godef stack")
    return
  endif

  if !len(a:000)
    let numPop = 1
  else
    let numPop = a:1
  endif

  let newLevel = str2nr(s:go_stack_level) - str2nr(numPop)
  call go#def#Stack(newLevel + 1)
endfunction

function! go#def#Stack(...) abort
  if len(s:go_stack) == 0
    call go#util#EchoError("godef stack empty")
    return
  endif

  if !len(a:000)
    " Display interactive stack
    call go#def#StackUI()
    return
  else
    let jumpTarget = a:1
  endif

  if jumpTarget !~ '^\d\+$'
    if jumpTarget !~ '^\s*$'
      call go#util#EchoError("location must be a number")
    endif
    return
  endif

  let jumpTarget = str2nr(jumpTarget) - 1

  if jumpTarget >= 0 && jumpTarget < len(s:go_stack)
    let s:go_stack_level = jumpTarget
    let target = s:go_stack[s:go_stack_level]

    " jump
    if expand('%:p') != target["file"]
      if &modified
        exec 'hide edit' target["file"]
      else
        exec 'edit' target["file"]
      endif
    endif
    call cursor(target["line"], target["col"])
    normal! zz
  else
    call go#util#EchoError("invalid location. Try :GoDefStack to see the list of valid entries")
  endif
endfunction

function s:def_job(args, state) abort
  let l:start_options = go#job#Options(a:args)

  let l:state = a:state
  function! s:exit_cb(next, job, exitval) dict
    call call(a:next, [a:job, a:exitval])
    if has_key(self, 'winid')
      call win_gotoid(self.winid)
    endif
  endfunction
  let l:start_options.exit_cb = funcref('s:exit_cb', [l:start_options.exit_cb], l:state)

  function! s:close_cb(next, ch) dict
    call call(a:next, [a:ch])
    if has_key(self, 'winid')
      call win_gotoid(self.winid)
    endif
  endfunction
  let l:start_options.close_cb = funcref('s:close_cb', [l:start_options.close_cb], l:state)

  if &modified
    let l:tmpname = tempname()
    call writefile(split(a:args.input, "\n"), l:tmpname, "b")
    let l:start_options.in_io = "file"
    let l:start_options.in_name = l:tmpname
  endif

  call go#job#Start(a:args.cmd, l:start_options)
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
