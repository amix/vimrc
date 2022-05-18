scriptencoding utf-8
let s:root = expand('<sfile>:h:h:h')

function! s:checkVim(test, name, patchlevel) abort
  if a:test
    if !has(a:patchlevel)
      call health#report_error(a:name . ' version not satisfied, ' . a:patchlevel . ' and above required')
      return 0
    else
      call health#report_ok(a:name . ' version satisfied')
      return 1
    endif
  endif
  return 0
endfunction

function! s:checkEnvironment() abort
  let valid
    \ = s:checkVim(has('nvim'), 'nvim', 'nvim-0.3.2')
    \ + s:checkVim(!has('nvim'), 'vim', 'patch-0.8.1453')
  let node = get(g:, 'coc_node_path', $COC_NODE_PATH == '' ? 'node' : $COC_NODE_PATH)
  if !executable(node)
    let valid = 0
    call health#report_error('Executable node.js not found, install node.js from http://nodejs.org/')
  endif
  let output = system(node . ' --version')
  if v:shell_error && output !=# ""
    let valid = 0
    call health#report_error(output)
  endif
  let ms = matchlist(output, 'v\(\d\+\).\(\d\+\).\(\d\+\)')
  if empty(ms)
    let valid = 0
    call health#report_error('Unable to detect version of node, make sure your node executable is http://nodejs.org/')
  elseif str2nr(ms[1]) < 12 || (str2nr(ms[1]) == 12 && str2nr(ms[2]) < 12)
    let valid = 0
    call health#report_warn('Node.js version '.trim(output).' < 12.12.0, please upgrade node.js')
  endif
  if valid
    call health#report_ok('Environment check passed')
  endif
  if has('pythonx')
    try
      silent pyx print("")
    catch /.*/
      call health#report_warn('pyx command not work, some extensions may fail to work, checkout ":h pythonx"')
      if has('nvim')
        call health#report_warn('Install pynvim by command: pip install pynvim --upgrade')
      endif
    endtry
  endif
  return valid
endfunction

function! s:checkCommand()
  let file = s:root.'/build/index.js'
  if filereadable(file)
    call health#report_ok('Javascript bundle build/index.js found')
  else
    call health#report_error('Javascript entry not found, please compile coc.nvim by esbuild.')
  endif
endfunction

function! s:checkAutocmd()
  let cmds = ['CursorHold', 'CursorHoldI', 'CursorMovedI', 'InsertCharPre', 'TextChangedI']
  for cmd in cmds
    let lines = split(execute('verbose autocmd '.cmd), '\n')
    let n = 0
    for line in lines
      if line =~# 'CocAction(' && n < len(lines) - 1
        let next = lines[n + 1]
        let ms = matchlist(next, 'Last set from \(.*\)')
        if !empty(ms)
          call health#report_warn('Use CocActionAsync to replace CocAction for better performance on '.cmd)
          call health#report_warn('Checkout the file '.ms[1])
        endif
      endif
      let n = n + 1
    endfor
  endfor
endfunction

function! s:checkInitialize() abort
  if coc#client#is_running('coc')
    call health#report_ok('Service started')
    return 1
  endif
  call health#report_error('service could not be initialized', [
        \ 'Use command ":messages" to get error messages.',
        \ 'Open a issue at https://github.com/neoclide/coc.nvim/issues for feedback.'
        \])
  return 0
endfunction

function! health#coc#check() abort
    call s:checkEnvironment()
    call s:checkCommand()
    call s:checkInitialize()
    call s:checkAutocmd()
endfunction
