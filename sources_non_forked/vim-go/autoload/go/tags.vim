function! go#tags#Add(start, end, count, ...) abort
  let fname = fnamemodify(expand("%"), ':p:gs?\\?/?')
  if &modified
    " Write current unsaved buffer to a temp file and use the modified content
    let l:tmpname = tempname()
    call writefile(getline(1, '$'), l:tmpname)
    let fname = l:tmpname
  endif

  let offset = 0
  if a:count == -1
    let offset = go#util#OffsetCursor()
  endif

  let test_mode = 0
  call call("go#tags#run", [a:start, a:end, offset, "add", fname, test_mode] + a:000)

  " if exists, delete it as we don't need it anymore
  if exists("l:tmpname")
    call delete(l:tmpname)
  endif
endfunction

function! go#tags#Remove(start, end, count, ...) abort
  let fname = fnamemodify(expand("%"), ':p:gs?\\?/?')
  if &modified
    " Write current unsaved buffer to a temp file and use the modified content
    let l:tmpname = tempname()
    call writefile(getline(1, '$'), l:tmpname)
    let fname = l:tmpname
  endif

  let offset = 0
  if a:count == -1
    let offset = go#util#OffsetCursor()
  endif

  let test_mode = 0
  call call("go#tags#run", [a:start, a:end, offset, "remove", fname, test_mode] + a:000)

  " if exists, delete it as we don't need it anymore
  if exists("l:tmpname")
    call delete(l:tmpname)
  endif
endfunction

" run runs gomodifytag. This is an internal test so we can test it
function! go#tags#run(start, end, offset, mode, fname, test_mode, ...) abort
  " do not split this into multiple lines, somehow tests fail in that case
  let args = {'mode': a:mode,'start': a:start,'end': a:end,'offset': a:offset,'fname': a:fname,'cmd_args': a:000}

  let result = s:create_cmd(args)
  if has_key(result, 'err')
    call go#util#EchoError(result.err)
    return -1
  endif

  let command = join(result.cmd, " ")

  call go#cmd#autowrite()
  let out = go#util#System(command)
  if go#util#ShellError() != 0
    call go#util#EchoError(out)
    return
  endif

  if a:test_mode
    exe 'edit ' . a:fname
  endif

  call s:write_out(out)

  if a:test_mode
    exe 'write! ' . a:fname
  endif
endfunc


" write_out writes back the given output to the current buffer
func s:write_out(out) abort
  " not a json output
  if a:out[0] !=# '{'
    return
  endif

  " nothing to do
  if empty(a:out) || type(a:out) != type("")
    return
  endif

  let result = json_decode(a:out)
  if type(result) != type({})
    call go#util#EchoError(printf("malformed output from gomodifytags: %s", a:out))
    return
  endif

  let lines = result['lines']
  let start_line = result['start']
  let end_line = result['end']

  let index = 0
  for line in range(start_line, end_line)
    call setline(line, lines[index])
    let index += 1
  endfor
endfunc


" create_cmd returns a dict that contains the command to execute gomodifytags
func s:create_cmd(args) abort
  if !exists("*json_decode")
    return {'err': "requires 'json_decode'. Update your Vim/Neovim version."}
  endif

  let bin_path = go#path#CheckBinPath('gomodifytags')
  if empty(bin_path)
    return {'err': "gomodifytags does not exist"}
  endif

  let l:start = a:args.start
  let l:end = a:args.end
  let l:offset = a:args.offset
  let l:mode = a:args.mode
  let l:cmd_args = a:args.cmd_args
  let l:modifytags_transform = get(g:, 'go_addtags_transform', "snakecase")

  " start constructing the command
  let cmd = [bin_path]
  call extend(cmd, ["-format", "json"])
  call extend(cmd, ["-file", a:args.fname])
  call extend(cmd, ["-transform", l:modifytags_transform])

  if l:offset != 0
    call extend(cmd, ["-offset", l:offset])
  else
    let range = printf("%d,%d", l:start, l:end)
    call extend(cmd, ["-line", range])
  endif

  if l:mode == "add"
    let l:tags = []
    let l:options = []

    if !empty(l:cmd_args)
      for item in l:cmd_args
        let splitted = split(item, ",")

        " tag only
        if len(splitted) == 1
          call add(l:tags, splitted[0])
        endif

        " options only
        if len(splitted) == 2
          call add(l:tags, splitted[0])
          call add(l:options, printf("%s=%s", splitted[0], splitted[1]))
        endif
      endfor
    endif

    " construct options
    if !empty(l:options)
      call extend(cmd, ["-add-options", join(l:options, ",")])
    else
      " default value
      if empty(l:tags)
        let l:tags = ["json"]
      endif

      " construct tags
      call extend(cmd, ["-add-tags", join(l:tags, ",")])
    endif
  elseif l:mode == "remove"
    if empty(l:cmd_args)
      call add(cmd, "-clear-tags")
    else
      let l:tags = []
      let l:options = []
      for item in l:cmd_args
        let splitted = split(item, ",")

        " tag only
        if len(splitted) == 1
          call add(l:tags, splitted[0])
        endif

        " options only
        if len(splitted) == 2
          call add(l:options, printf("%s=%s", splitted[0], splitted[1]))
        endif
      endfor

      " construct tags
      if !empty(l:tags)
        call extend(cmd, ["-remove-tags", join(l:tags, ",")])
      endif

      " construct options
      if !empty(l:options)
        call extend(cmd, ["-remove-options", join(l:options, ",")])
      endif
    endif
  else
    return {'err': printf("unknown mode: %s", l:mode)}
  endif

  return {'cmd': cmd}
endfunc
