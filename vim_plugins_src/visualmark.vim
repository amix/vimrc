" Visual Mark
" 2005-10-27, brian wang
"
" Acknowledgements:
"   - Thanks to Hari Krishna Dara's genutils.vim (http://vim.sourceforge.net/scripts/script.php?script_id=197)
"   - Thanks to Mr. Charles E. Campbell, Jr. for making this script more plugin-like :-)
"   - Thanks to Mr. Charles E. Campbell, Jr. for making this script adapt to
"     dark/light backgrounds
"   - Thanks to Evgeny Filatov for noticing a nasty bug in Vm_get_line_number :-)

if exists("loaded_VisualMark")
  finish
endif
let loaded_VisualMark = 1
if !has("signs")
 echoerr "***sorry*** [".expand("%")."] your vim doesn't support signs"
 finish
endif

if &bg == "dark"
 highlight SignColor ctermfg=white ctermbg=blue guifg=white guibg=RoyalBlue3
else
 highlight SignColor ctermbg=white ctermfg=blue guibg=grey guifg=RoyalBlue3
endif

" ---------------------------------------------------------------------
"  Public Interface:
if !hasmapto('<Plug>Vm_toggle_sign')
  map <unique> <c-F2> <Plug>Vm_toggle_sign
  map <silent> <unique> mm <Plug>Vm_toggle_sign 
endif
nnoremap <silent> <script> <Plug>Vm_toggle_sign	:call Vm_toggle_sign()<cr>

if !hasmapto('<Plug>Vm_goto_next_sign')
  map <unique> <F2> <Plug>Vm_goto_next_sign
endif
nnoremap <silent> <script> <Plug>Vm_goto_next_sign	:call Vm_goto_next_sign()<cr>

if !hasmapto('<Plug>Vm_goto_prev_sign')
  map <unique> <s-F2> <Plug>Vm_goto_prev_sign
endif
nnoremap <silent> <script> <Plug>Vm_goto_prev_sign	:call Vm_goto_prev_sign()<cr>

" ---------------------------------------------------------------------
"  GetVimCmdOutput:
" Stole from Hari Krishna Dara's genutils.vim (http://vim.sourceforge.net/scripts/script.php?script_id=197)
"  to ease the scripts dependency issue
fun! s:GetVimCmdOutput(cmd)
"  call Dfunc("GetVimCmdOutput(cmd.".a:cmd.">)")

  " Save the original locale setting for the messages
  let old_lang = v:lang

  " Set the language to English
  exec ":lan mes en_US"

  let v:errmsg = ''
  let output   = ''
  let _z       = @z

  try
    redir @z
    silent exe a:cmd
  catch /.*/
    let v:errmsg = substitute(v:exception, '^[^:]\+:', '', '')
  finally
    redir END
    if v:errmsg == ''
      let output = @z
    endif
    let @z = _z
  endtry

  " Restore the original locale
  exec ":lan mes " . old_lang

"  call Dret("GetVimCmdOutput <".output.">")
  return output
endfun

" ---------------------------------------------------------------------
"  Vm_place_sign:
fun! s:Vm_place_sign()
"  call Dfunc("Vm_place_sign()")

  if !exists("b:Vm_sign_number")
    let b:Vm_sign_number = 1
  endif

  let ln = line(".")

  exe 'sign define SignSymbol linehl=SignColor texthl=SignColor'
  exe 'sign place ' . b:Vm_sign_number . ' line=' . ln . ' name=SignSymbol buffer=' . winbufnr(0)

  let vsn              = b:Vm_sign_number
  let b:Vm_sign_number = b:Vm_sign_number + 1

"  call Dret("Vm_place_sign : sign#".vsn." line#".ln." buf#".winbufnr(0))
endfun

" ---------------------------------------------------------------------
" Vm_remove_sign:
fun! s:Vm_remove_sign(sign_id)
"  call Dfunc("Vm_remove_sign(sign_id=".a:sign_id.")")
  silent! exe 'sign unplace ' . a:sign_id . ' buffer=' . winbufnr(0)
"  call Dret("Vm_remove_sign")
endfun

" ---------------------------------------------------------------------
" Vm_remove_all_signs:
fun! s:Vm_remove_all_signs()
"  call Dfunc("Vm_remove_all_signs()")
  silent! exe 'sign unplace *'
"  call Dret("Vm_remove_all_signs")
endfun

" ---------------------------------------------------------------------
" Vm_get_sign_id_from_line:
fun! s:Vm_get_sign_id_from_line(line_number)
"  call Dfunc("Vm_get_sign_id_from_line(line_number=".a:line_number.")")

  let sign_list = s:GetVimCmdOutput('sign place buffer=' . winbufnr(0))
"  call Decho(sign_list)

  let line_str_index = match(sign_list, "line=" . a:line_number, 0)
  if line_str_index < 0
"    call Dret("Vm_get_sign_id_from_line -1")
    return -1
  endif

  let id_str_index = matchend(sign_list, "id=", line_str_index)
"  let tmp = strpart(sign_list, id_str_index, 10)   "Decho
"  call Decho("ID str index: " . tmp)
  if id_str_index < 0
"    call Dret("Vm_get_sign_id_from_line -1")
    return -1
  endif

  let space_index = match(sign_list, " ", id_str_index)
  let id          = strpart(sign_list, id_str_index, space_index - id_str_index)

"  call Dret("Vm_get_sign_id_from_line ".id)
  return id
endfun

" ---------------------------------------------------------------------
" Vm_toggle_sign:
fun! Vm_toggle_sign()
"  call Dfunc("Vm_toggle_sign()")

  let curr_line_number = line(".")
  let sign_id          = s:Vm_get_sign_id_from_line(curr_line_number)

  if sign_id < 0
    let is_on = 0
  else
    let is_on = 1
  endif

  if (is_on != 0)
    call s:Vm_remove_sign(sign_id)
  else
    call s:Vm_place_sign()
  endif

"  call Dret("Vm_toggle_sign")
endfun

" ---------------------------------------------------------------------
" Vm_get_line_number:
fun! s:Vm_get_line_number(string)
"  call Dfunc("Vm_get_line_number(string<".a:string.">)")

  let line_str_index = match(a:string, "line=", b:Vm_start_from)
  if line_str_index <= 0
"    call Dret("Vm_get_line_number -1")
    return -1
  endif

  let equal_sign_index = match(a:string, "=", line_str_index)
  let space_index      = match(a:string, " ", equal_sign_index)
  let line_number      = strpart(a:string, equal_sign_index + 1, space_index - equal_sign_index - 1)
  let b:Vm_start_from  = space_index

"  call Dret("Vm_get_line_number ".line_number." : =indx:".equal_sign_index." _indx=".space_index)
  return line_number + 0
endfun

" ---------------------------------------------------------------------
" Vm_get_next_sign_line:
fun! s:Vm_get_next_sign_line(curr_line_number)
  " call Dfunc("Vm_get_next_sign_line(curr_line_number=".a:curr_line_number.">)")

  let b:Vm_start_from = 1
  let sign_list = s:GetVimCmdOutput('sign place buffer=' . winbufnr(0))
  " call Decho("sign_list<".sign_list.">")

  let curr_line_number = a:curr_line_number
  let line_number = 1
  let is_no_sign  = 1
  let min_line_number = -1
  let min_line_number_diff = 0
  
  while 1
    let line_number = s:Vm_get_line_number(sign_list)
    if line_number < 0
      break
    endif

    " Record the very first line that has a sign
    if is_no_sign != 0 
      let min_line_number = line_number
    elseif line_number < min_line_number
      let min_line_number = line_number
    endif
    let is_no_sign = 0

    " let tmp_diff = curr_line_number - line_number
    let tmp_diff = line_number - curr_line_number
    if tmp_diff > 0
      " line_number is below curr_line_number
      if min_line_number_diff > 0 
        if tmp_diff < min_line_number_diff
          let min_line_number_diff = tmp_diff
        endif
      else
        let min_line_number_diff = tmp_diff
      endif
    endif

    " call Decho("[DBG] Line Diff: #" . min_line_number_diff)
  endwhile

  let line_number = curr_line_number + min_line_number_diff
  " call Decho("[DBG] Line Diff: #" . min_line_number_diff)
  " call Decho("[DBG] Line Num: #" . line_number)

  if is_no_sign != 0 || min_line_number_diff <= 0
    let line_number = min_line_number
  endif

  " call Dret("Vm_get_next_sign_line ".line_number . " XXX")
  return line_number
endfun

" ---------------------------------------------------------------------
" Vm_get_prev_sign_line:
fun! s:Vm_get_prev_sign_line(curr_line_number)
  " call Dfunc("Vm_get_prev_sign_line(curr_line_number=".a:curr_line_number.">)")

  let b:Vm_start_from = 1
  let sign_list = s:GetVimCmdOutput('sign place buffer=' . winbufnr(0))
  " call Decho("sign_list<".sign_list.">")

  let curr_line_number = a:curr_line_number
  let line_number = 1
  let is_no_sign  = 1
  let max_line_number = -1
  let max_line_number_diff = 0
  
  while 1
    let line_number = s:Vm_get_line_number(sign_list)
    if line_number < 0
      break
    endif

    " Record the very first line that has a sign
    if is_no_sign != 0 
      let max_line_number = line_number
    elseif line_number > max_line_number 
      let max_line_number = line_number
    endif
    let is_no_sign = 0

    let tmp_diff = curr_line_number - line_number
    if tmp_diff > 0
      " line_number is below curr_line_number
      if max_line_number_diff > 0 
        if tmp_diff < max_line_number_diff 
          let max_line_number_diff = tmp_diff
        endif
      else
        let max_line_number_diff = tmp_diff
      endif
    endif

    " call Decho("[DBG] Line Diff: #" . max_line_number_diff)
    " call Decho("[DBG] Tmp Diff: #" . tmp_diff)
  endwhile

  let line_number = curr_line_number - max_line_number_diff 
  " call Decho("[DBG] Line Diff: #" . max_line_number_diff)
  " call Decho("[DBG] Line Num: #" . line_number)

  if is_no_sign != 0 || max_line_number_diff <= 0
    let line_number = max_line_number 
  endif

  " call Dret("Vm_get_prev_sign_line ".line_number . " XXX")
  return line_number
endfun

" ---------------------------------------------------------------------
" Vm_goto_next_sign:
fun! Vm_goto_next_sign()
  " call Dfunc("Vm_goto_next_sign()")

  let curr_line_number      = line(".")
  let next_sign_line_number = s:Vm_get_next_sign_line(curr_line_number)

"  call Decho("Next sign line #:  " . next_sign_line_number)
  if next_sign_line_number >= 0
    exe ":" . next_sign_line_number
    "call Decho("Going to Line #" . next_sign_line_number)
  endif

"  call Dret("Vm_goto_next_sign")
endfun

" ---------------------------------------------------------------------
" Vm_goto_prev_sign:
fun! Vm_goto_prev_sign()
  " call Dfunc("Vm_goto_prev_sign()")

  let curr_line_number      = line(".")
  let prev_sign_line_number = s:Vm_get_prev_sign_line(curr_line_number)
"  call Decho("Previous sign line #:  " . prev_sign_line_number)

  if prev_sign_line_number >= 0
    exe prev_sign_line_number 
  endif

  " call Dret("Vm_goto_prev_sign")
endfun

" ---------------------------------------------------------------------
