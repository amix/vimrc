" old code

augroup TOVLWrite
augroup end

" =========== scratch buffer =========================================
" a scratch buffer is a temporary buffer where the user can enter some text
" It can be used to get commit messages, edit configuration options and so on

function! tovl#scratch_buffer#KeepIntactLineNr()
  let i = 0
  while getline(i)!= b:keepIntact && i < line('$')
    let i = i+1
  endwhile
  if i > line('$')
    return -1
  else
    return i
  endif
endfunction

" opens a buffer and runs an action when the buffer is written
" keys: 
"  name :   the name of the buffer
"  onWrite : will be called on write
"            onWrite is responsible for setlocal nomodified to indicate that
"            saving has been successful
"  help  : callback returning additional information lines
"  getContent : callback returning lines
"  cmds    : extra commands to be run (optional)
"            (maybe you prefer adding them the default way afer the
"            ScratchBuffer call. They'll be rerun on GetContents
"  sp_cmd  : the command to use to create the new buffer. Defaults to :e
"  buftype : ...
"  modifiable : 1 / 0 defaults to 1
function! tovl#scratch_buffer#ScratchBuffer(opts)
  let a:opts['name'] = get(a:opts,'name', 'strach_buffer_without_name')
  exec get(a:opts, 'sp_cmd', 'e').' '.escape(a:opts['name'],' ')
  let b:settings = a:opts
  let b:settings['modifiable'] = get(a:opts,'modifiable', 1)
  setlocal buftype=acwrite
  command! -buffer -nargs=0 Help call tovl#scratch_buffer#Help()

  " setup write notification
  au TOVLWrite BufWriteCmd <buffer> call tovl#scratch_buffer#Write()

  if has_key(a:opts,'getContent')
    command! -buffer -nargs=0 GetContents call tovl#scratch_buffer#GetContents()
    GetContents
    if !b:settings['modifiable']
      setlocal nomodifiable
    endif
  endif
  "let u=&undolevels
  "setlocal undolevels=-1
  "exec 'setlocal undolevels='.u

  " mark buffer as not modified
  setlocal nomodified

  au BufReadCmd <buffer> GetContents

  " run addittional commands
  for cmd in get(a:opts,'cmds',[])
    exec cmd
  endfor
  silent echo get(a:opts,'echo_help', "type :Help for help")
endfunction

" =========== utility functions ======================================

function! tovl#scratch_buffer#Write()
  if has_key(b:settings, 'onWrite')
    call funcref#Call(b:settings['onWrite'])
  else
    echo "don't know how to write. Option hasn't been passed"
  endif
endfunction

function! tovl#scratch_buffer#GetContents()
  setlocal modifiable
  " empty buffer
  %g!//d
  call append(0, funcref#Call(b:settings['getContent']))
  if !b:settings['modifiable']
    setlocal nomodifiable
  endif
  for cmd in get(b:settings,'cmds',[])
    exec cmd
  endfor
endfunction

function! tovl#scratch_buffer#Help()
  let help = ["use :e! to reload contents, ZZ or :w(q) to write and quit"
          \ ,""
          \ ,"Help for this scratch buffer:"
          \ ,"=======================================================","",""]
    \ + funcref#Call(get(b:settings, 'help', []))
  call tovl#scratch_buffer#ScratchBuffer({
        \ 'name' : "return Help of ".b:settings['name'],
        \ 'getContent' : help
        \ })
endfunction
