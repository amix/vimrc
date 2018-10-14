
" TODO refactor: create glob function
" noremap \og :call<space>glob_linux#FileByGlobCurrentDir('**/*'.input('glob open '),"\\.git\\<bar>\\.hg\\<bar>node_modules\\<bar>\\.pyc" )<cr>
" noremap \og :call<space>glob_linux#FileByGlobCurrentDir('**/*'.input('glob open '),"default" )<cr>
function! glob_linux#FileByGlobCurrentDir(glob, exclude_pattern)
  if a:exclude_pattern == "default"
    let exclude_pattern = '\.git\|\.hg\|node_modules\|\.pyc'
  else
    let exclude_pattern = a:exclude_pattern
  endif

  " let files = split(glob(a:glob),"\n")
  let g = a:glob
  let replace = {'**': '.*','*': '[^/\]*','.': '\.'}
  let g = substitute(g, '\(\*\*\|\*\|\.\)', '\='.string(replace).'[submatch(1)]','g')

  let exclude = exclude_pattern == '' ? '' : ' | grep -v -e '.shellescape(exclude_pattern)

  let cmd = 'find | grep -e '.shellescape(g).exclude
  let files = split(system(cmd),"\n")
  " for nom in a:excludes
  "   call filter(files,nom)
  " endfor
  if len(files) > 1000
    echoe "more than ".2000." files - would be too slow. Open the file in another way"
  else
    if empty(files)
      echoe "no file found"
    elseif len(files) == 1
      exec 'e '.fnameescape(files[0])
    else
      let g:abc=7
      call tovl#ui#filter_list#ListView({
            \ 'number' : 1,
            \ 'selectByIdOrFilter' : 1,
            \ 'Continuation' : funcref#Function('exec "e ".fnameescape(ARGS[0])'),
            \ 'items' : files,
            \ 'cmds' : ['wincmd J']
            \ })
    endif
  endif
endfunction
