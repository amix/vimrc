" MRU plugin unit-tests

" MRU plugin settings
let MRU_File='vim_mru_file'
let MRU_Auto_Close=1
let MRU_Max_Entries=10
let MRU_buffer_name = '-RecentFiles-'

" Set the $MRU_PROFILE environment variable to profile the MRU plugin
let s:do_profile = 0
if exists('$MRU_PROFILE')
  let s:do_profile = 1
endif

" Profile the MRU plugin
if s:do_profile
  profile start mru_profile.txt
  profile! file */mru.vim
endif

" Tests assume that 'hidden' option is not set
set nohidden

source ../plugin/mru.vim

" Function to log test results
func! LogResult(test, result)
  call add(g:results, a:test . ': ' . a:result)
endfunc

" ==========================================================================
" Test1
" When the MRU list is empty, invoking the MRU command should return an error
" ==========================================================================
func Test_01()
  let test_name = 'test1'

  redir => msg
  MRU
  redir END
  if msg =~# "MRU file list is empty"
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test2
" Open the MRU window and check the order of files listed in the window
" Open the MRU window when the window is already opened.
" ==========================================================================
func Test_02()
  let test_name = 'test2'

  edit file1.txt
  edit file2.txt
  edit file3.txt
  edit file2.txt
  edit file1.txt

  MRU
  MRU

  let l = getline(1, "$")
  if l[0] =~# "file1.txt" && l[1] =~# "file2.txt" && l[2] =~# "file3.txt"
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test3
" Select a file from the MRU window and check whether it is opened
" ==========================================================================
func Test_03()
  let test_name = 'test3'

  " Go to the last but one line
  $

  " Select the last file in the MRU window
  exe "normal \<Enter>"

  if fnamemodify(@%, ':p:t') !=# 'file3.txt'
    call LogResult(test_name, "FAIL (1)")
  else
    " Make sure the MRU window is closed
    if bufwinnr(g:MRU_buffer_name) == -1
      call LogResult(test_name, 'pass')
    else
      call LogResult(test_name, "FAIL (2)")
    endif
  endif
endfunc

" ==========================================================================
" Test4
" MRU opens a selected file in the previous/last window
" ==========================================================================
func Test_04()
  let test_name = 'test4'

  " Edit a file and then open a new window, open the MRU window and select the
  " file
  split file1.txt
  only
  below new

  MRU
  call search('file2.txt')
  exe "normal \<Enter>"

  if winnr() == 2
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test5
" MRU opens a selected file in the same window if the file is already opened
" ==========================================================================
func Test_05()
  let test_name = 'test5'

  edit file1.txt
  only
  below split file2.txt
  below split file3.txt

  MRU
  call search('file1.txt')
  exe "normal \<Enter>"

  if winnr() != 1 || fnamemodify(@%, ':p:t') !=# 'file1.txt'
    call LogResult(test_name, "FAIL (1)")
  else
    MRU
    call search('file2.txt')
    exe "normal \<Enter>"
    if winnr() != 2 || fnamemodify(@%, ':p:t') !=# 'file2.txt'
      call LogResult(test_name, "FAIL (2)")
    else
      MRU
      call search('file3.txt')
      exe "normal \<Enter>"
      if winnr() != 3 || fnamemodify(@%, ':p:t') !=# 'file3.txt'
        call LogResult(test_name, "FAIL (3)")
      else
        call LogResult(test_name, 'pass')
      endif
    endif
  endif
endfunc

" ==========================================================================
" Test6
" MRU opens a file selected with 'o' command in a new window
" ==========================================================================
func Test_06()
  let test_name = 'test6'
  enew | only

  edit file1.txt
  below new

  MRU
  normal o

  if winnr() == 3 && fnamemodify(@%, ':p:t') ==# 'file1.txt'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test7
" MRU opens the selected file in a new window if the previous buffer is
" modified.
" ==========================================================================
func Test_07()
  let test_name = 'test7'
  enew | only

  insert
  MRU plugin test
.
  MRU
  call search('file3.txt')
  exe "normal \<Enter>"
  if winnr() == 1 && winnr('$') == 2 &&
        \ fnamemodify(@%, ':p:t') ==# 'file3.txt'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif

  " Discard changes in the new buffer
  wincmd b
  enew!
  only
endfunc

" ==========================================================================
" Test8
" MRU opens a file selected with 'v' command in read-only mode in the current
" window.
" ==========================================================================
func Test_08()
  let test_name = 'test8'
  enew | only

  MRU
  call search('file1.txt')
  normal v
  let r1 = &readonly
  MRU
  call search('file2.txt')
  exe "normal \<Enter>"
  let r2 = &readonly
  MRU
  call search('file1.txt')
  exe "normal \<Enter>"
  let r3 = &readonly
  if r1 == 1 && r2 == 0 && r3 == 1
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test9
" Use 'O' in the MRU window to open a file in a vertically split window
" ==========================================================================
func Test_09()
  let test_name = 'test9'
  enew | only

  edit file1.txt
  MRU
  call search('file2.txt')
  normal O
  let b1 = @%
  wincmd h
  let b2 = @%
  wincmd l
  let b3 = @%
  if winnr('$') == 2 && b1 ==# 'file2.txt' &&
        \ b2 ==# 'file1.txt' && b3 ==# 'file2.txt'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test10
" Use 'p' in the MRU window to open a file in the preview window
" ==========================================================================
func Test_10()
  let test_name = 'test10'
  enew | only

  MRU
  call search('file3.txt')
  normal p
  wincmd P
  let p1 = &previewwindow
  let b1 = @%
  if winnr('$') == 2 && &previewwindow && @% =~# 'file3.txt'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
  pclose
endfunc

" ==========================================================================
" Test11
" MRU opens a file selected with 't' command in a new tab and the tab
" is opened at the end
" ==========================================================================
func Test_11()
  let test_name = 'test11'
  enew | only

  edit a1.txt
  tabnew a2.txt
  tabnew a3.txt
  tabnew a4.txt
  tabfirst
  MRU
  call search('file3.txt')
  normal t
  if fnamemodify(@%, ':p:t') ==# 'file3.txt' && tabpagenr() == 5
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
    call LogResult(test_name, "file = " . fnamemodify(@%, ':p:t'))
    call LogResult(test_name, "tab page = " . tabpagenr())
  endif

  tabonly
endfunc

" ==========================================================================
" Test12
" The 'q' command closes the MRU window
" ==========================================================================
func Test_12()
  let test_name = 'test12'
  enew | only

  MRU
  normal q
  if bufwinnr(g:MRU_buffer_name) == -1
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test13
" A selected file is opened in a new window if the previous window is a
" preview window
" ==========================================================================
func Test_13()
  let test_name = 'test13'
  enew | only

  setlocal previewwindow
  MRU
  call search('file2.txt')
  exe "normal \<Enter>"
  if winnr() == 1 && winnr('$') == 2 &&
        \ &previewwindow == 0 &&
        \ fnamemodify(@%, ':p:t') ==# 'file2.txt'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif

  " Close the preview window created by this test
  new
  only
endfunc

" ==========================================================================
" Test14
" A selected file is opened in a new window if the previous window contains
" a special buffer (used by some other plugin)
" ==========================================================================
func Test_14()
  let test_name = 'test14'
  enew | only

  setlocal buftype=nofile
  MRU
  call search('file3.txt')
  exe "normal \<Enter>"
  if winnr() == 1 && winnr('$') == 2 &&
        \ &buftype == '' &&
        \ fnamemodify(@%, ':p:t') ==# 'file3.txt'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif

  " Discard the special buffer
  enew
endfunc

" ==========================================================================
" Test15
" If a file selected using the 't' command is already opened in a tab,
" then jump to that tab (instead of opening a new tab)
" ==========================================================================
func Test_15()
  let test_name = 'test15'
  enew | only

  " Open the test files in the middle window with empty windows at the top and
  " bottom
  edit file1.txt
  above new
  botright new
  tabedit file2.txt
  above new
  botright new
  tabedit file3.txt
  above new
  botright new
  tabfirst

  MRU
  call search('file3.txt')
  exe "normal t"
  if tabpagenr() != 3
        \ || fnamemodify(@%, ':p:t') !=# 'file3.txt'
        \ || winnr() != 2
    call LogResult(test_name, "FAIL (1)")
  else
    MRU
    call search('file1.txt')
    exe "normal t"
    if tabpagenr() != 1
          \ || fnamemodify(@%, ':p:t') !=# 'file1.txt'
          \ || winnr() != 2
      call LogResult(test_name, "FAIL (2)")
    else
      MRU
      call search('file2.txt')
      exe "normal t"
      if tabpagenr() != 2
            \ || fnamemodify(@%, ':p:t') !=# 'file2.txt'
            \ || winnr() != 2
        call LogResult(test_name, "FAIL (3)")
      else
        call LogResult(test_name, 'pass')
      endif
    endif
  endif

  " Close all the other tabs
  tabonly
  enew
  only
endfunc

" ==========================================================================
" Test16
" Open multiple files from the MRU window using the visual mode and by using a
" count.  Each file should be opened in a separate window.
" ==========================================================================
func Test_16()
  let test_name = 'test16'
  enew | only

  edit file3.txt
  edit file2.txt
  edit file1.txt
  enew
  MRU
  exe "normal 3\<Enter>"
  if winnr('$') == 3 &&
        \ bufwinnr('file3.txt') == 1 &&
        \ bufwinnr('file2.txt') == 2 &&
        \ bufwinnr('file1.txt') == 3
    let test_result = 'pass'
  else
    let test_result = 'FAIL'
  endif

  only | enew

  if test_result == 'pass'
    MRU
    exe "normal V2j\<Enter>"
    if winnr('$') == 3 &&
          \ bufwinnr('file1.txt') == 1 &&
          \ bufwinnr('file2.txt') == 2 &&
          \ bufwinnr('file3.txt') == 3
      let test_result = 'pass'
    else
      let test_result = 'FAIL'
    endif
  endif

  if test_result == 'pass'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test17
" When the MRU list is updated, the MRU file also should updated.
" ==========================================================================
func Test_17()
  let test_name = 'test17'
  enew | only

  edit file1.txt
  let l = readfile(g:MRU_File)
  if l[1] =~# 'file1.txt'
    edit file2.txt
    let l = readfile(g:MRU_File)
    if l[1] =~# 'file2.txt'
      edit file3.txt
      let l = readfile(g:MRU_File)
      if l[1] =~# 'file3.txt'
        call LogResult(test_name, 'pass')
      else
        call LogResult(test_name, "FAIL (3)")
      endif
    else
      call LogResult(test_name, "FAIL (2)")
    endif
  else
    call LogResult(test_name, "FAIL (1)")
  endif
endfunc

" MRU_Test_Add_Files
" Add the supplied List of files to the beginning of the MRU file
func! s:MRU_Test_Add_Files(fnames)
  let l = readfile(g:MRU_File)
  call extend(l, a:fnames, 1)
  call writefile(l, g:MRU_File)
endfunc

" ==========================================================================
" Test18
" When the MRU file is updated by another Vim instance, the MRU plugin
" should update the MRU list
" ==========================================================================
func Test_18()
  let test_name = 'test18'
  enew | only

  call s:MRU_Test_Add_Files(['/software/editors/vim',
        \ '/software/editors/emacs',
        \ '/software/editors/nano'])
  MRU
  if getline(1) ==# 'vim (/software/editors/vim)'
        \ && getline(2) ==# 'emacs (/software/editors/emacs)'
        \ && getline(3) ==# 'nano (/software/editors/nano)'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif

  " Close the MRU window
  close
endfunc

" ==========================================================================
" Test19
" When the MRU file is updated by another Vim instance, the MRU file names
" from the current instance should be merged with that list
" ==========================================================================
func Test_19()
  let test_name = 'test19'
  enew | only

  " Remove all the files from the MRU file
  let l = readfile(g:MRU_File)
  call remove(l, 1, -1)
  call writefile(l, g:MRU_File)
  edit file1.txt
  call s:MRU_Test_Add_Files(['/software/os/unix'])
  edit file2.txt
  call s:MRU_Test_Add_Files(['/software/os/windows'])
  edit file3.txt
  call s:MRU_Test_Add_Files(['/software/os/osx'])
  MRU
  if getline(1) ==# 'osx (/software/os/osx)'
        \ && getline(2) =~# 'file3.txt'
        \ && getline(3) ==# 'windows (/software/os/windows)'
        \ && getline(4) =~# 'file2.txt'
        \ && getline(5) ==# 'unix (/software/os/unix)'
        \ && getline(6) =~# 'file1.txt'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
  close
endfunc

" ==========================================================================
" Test20
" When the MRU list has more than g:MRU_Max_Entries, the list should be
" trimmed. The last entries should be removed.
" ==========================================================================
func Test_20()
  let test_name = 'test20'
  enew | only

  "  Create a MRU list with MRU_Max_Entries
  let flist = []
  for i in range(1, g:MRU_Max_Entries)
    let flist += ['/usr/share/mru_test/mru_file' . i . '.abc']
  endfor

  " Modify the MRU file to contain max entries
  let l = readfile(g:MRU_File)
  call remove(l, 1, -1)
  call extend(l, flist)
  call writefile(l, g:MRU_File)

  enew
  edit file1.txt
  let l = readfile(g:MRU_File)
  if len(l) == (g:MRU_Max_Entries + 1) &&
        \ l[g:MRU_Max_Entries] != '/usr/share/mru_test/mru_file9.abc'
    call LogResult(test_name, "FAIL (1)")
  else
    edit file2.txt
    let l = readfile(g:MRU_File)
    if len(l) == (g:MRU_Max_Entries + 1) &&
          \ l[g:MRU_Max_Entries] != '/usr/share/mru_test/mru_file8.abc'
      call LogResult(test_name, "FAIL (2)")
    else
      edit file3.txt
      let l = readfile(g:MRU_File)
      if len(l) == (g:MRU_Max_Entries + 1) &&
            \ l[g:MRU_Max_Entries] != '/usr/share/mru_test/mru_file7.abc'
        call LogResult(test_name, "FAIL (3)")
      else
        call LogResult(test_name, 'pass')
      endif
    endif
  endif
endfunc

" ==========================================================================
" Test21
" When an filename (already present in the MRU list) is specified to the MRU
" command, it should edit the file.
" ==========================================================================
func Test_21()
  let test_name = 'test21'
  enew | only

  edit file1.txt
  edit file2.txt
  edit file3.txt
  enew
  MRU file2.txt
  if fnamemodify(@%, ':p:t') ==# 'file2.txt' && winnr('$') == 1
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test22
" When a pattern (matching multiple filenames) is specified to the MRU
" command, then the MRU window should be opened with all the matching
" filenames
" ==========================================================================
func Test_22()
  let test_name = 'test22'
  enew | only

  edit file1.txt
  edit file2.txt
  edit file3.txt
  only
  MRU file.*
  if @% != g:MRU_buffer_name
    call LogResult(test_name, 'FAIL')
  else
    let l = getline(1, "$")
    if l[0] =~# "file3.txt" && l[1] =~# "file2.txt" && l[2] =~# "file1.txt"
      call LogResult(test_name, 'pass')
    else
      call LogResult(test_name, 'FAIL')
    endif
  endif
  close
endfunc

" ==========================================================================
" Test23
" When a partial filename (matching multiple filenames) is specified to the
" MRU command, then the MRU window should be opened with all the matching
" filenames
" ==========================================================================
func Test_23()
  let test_name = 'test23'
  enew | only

  let g:MRU_FuzzyMatch = 0
  edit file1.txt
  edit file2.txt
  edit file3.txt
  only
  MRU file
  if @% != g:MRU_buffer_name
    call LogResult(test_name, 'FAIL')
  else
    let l = getline(1, "$")
    if l[0] =~# "file3.txt" && l[1] =~# "file2.txt" && l[2] =~# "file1.txt"
      call LogResult(test_name, 'pass')
    else
      call LogResult(test_name, 'FAIL')
    endif
  endif
  close
endfunc

" ==========================================================================
" Test24
" When a non-existing filename is specified to the MRU command, an error
" message should be displayed.
" ==========================================================================
func Test_24()
  let test_name = 'test24'

  let g:MRU_FuzzyMatch = 0
  redir => msg
  MRU nonexistingfile.txt
  redir END
  if @% == g:MRU_buffer_name ||
        \ msg !~# "MRU file list doesn't contain files " .
        \ "matching nonexistingfile.txt"
    call LogResult(test_name, 'FAIL')
  else
    call LogResult(test_name, 'pass')
  endif
endfunc

" ==========================================================================
" Test25
" The MRU command should support filename completion. Supply a partial file
" name to the MRU command and complete the filenames.
" ==========================================================================
func Test_25()
  let test_name = 'test25'
  enew | only

  edit file1.txt
  edit file2.txt
  edit file3.txt
  exe 'normal! :MRU file' . "\<C-A>" . "\<Home>let m='\<End>'\<CR>"
  let fnames = split(m)
  if fnames[1] =~# 'file3.txt' && fnames[2] =~# 'file2.txt' &&
        \ fnames[3] =~# 'file1.txt'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test26
" When trying to complete filenames for the MRU command without specifying
" any text should return the entire MRU list.
" ==========================================================================
func Test_26()
  let test_name = 'test26'
  enew | only

  call delete(g:MRU_File)
  edit file1.txt
  edit file2.txt
  edit file3.txt

  exe 'normal! :MRU ' . "\<C-A>" . "\<Home>let m='\<End>'\<CR>"
  let fnames = split(m)
  if fnames[1] =~# 'file3.txt' && fnames[2] =~# 'file2.txt' &&
        \ fnames[3] =~# 'file1.txt'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test27
" When the current file/buffer has unsaved changes, MRU should open a selected
" file in a new window (if the 'hidden' option is not set)
" ==========================================================================
func Test_27()
  let test_name = 'test27'
  enew | only

  edit file1.txt
  edit file2.txt
  call append(line('$'), 'Temporary changes to buffer')
  MRU
  call search('file1.txt')
  exe "normal \<Enter>"
  if winnr() == 1 && winnr('$') == 2 &&
        \ fnamemodify(@%, ':p:t') ==# 'file1.txt'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif

  close
  edit!
endfunc

" ==========================================================================
" Test28
" When the current file/buffer has unsaved changes and the 'hidden' option is
" set, then MRU should open a selected file in the current  window
" ==========================================================================
func Test_28()
  let test_name = 'test28'
  enew | only

  edit file2.txt
  edit file1.txt
  call append(line('$'), 'Temporary changes to buffer')
  set hidden

  MRU
  call search('file2.txt')
  exe "normal \<Enter>"
  if winnr('$') == 1 &&
        \ fnamemodify(@%, ':p:t') ==# 'file2.txt'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif

  edit file1.txt
  edit!
  set nohidden
  %bw!
endfunc

" ==========================================================================
" Test29
" Every edited file is added to the top of the MRU list. If a file is already
" present in the MRU list, then it is moved to the top of the list.
" ==========================================================================
func Test_29()
  let test_name = 'test29'
  enew | only

  edit file1.txt
  let f1 = readfile(g:MRU_File, '', 2)
  edit file2.txt
  let f2 = readfile(g:MRU_File, '', 2)
  edit file3.txt
  let f3 = readfile(g:MRU_File, '', 2)
  edit file1.txt
  let f4 = readfile(g:MRU_File, '', 2)
  if f1[1] =~# 'file1.txt' && f2[1] =~# 'file2.txt' && f3[1] =~# 'file3.txt' &&
        \ f4[1] =~# 'file1.txt'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test30
" Only file names matching the regular expression in the MRU_Include_Files
" variable should be added to the MRU list.
" ==========================================================================
func Test_30()
  let test_name = 'test30'
  enew | only

  edit file1.txt
  let g:MRU_Include_Files='\.c'
  edit abc.c
  let f1 = readfile(g:MRU_File, '', 2)
  edit file1.txt
  let f2 = readfile(g:MRU_File, '', 2)
  edit def.c
  let f3 = readfile(g:MRU_File, '', 2)
  if f1[1] =~# 'abc.c' && f2[1] =~# 'abc.c' && f3[1] =~# 'def.c'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
  let g:MRU_Include_Files=''
endfunc

" ==========================================================================
" Test31
" File names matching the regular expression in the MRU_Exclude_Files
" variable should not be added to the MRU list.
" ==========================================================================
func Test_31()
  let test_name = 'test31'
  enew | only

  let g:MRU_Exclude_Files='\.txt'
  edit abc.c
  let f1 = readfile(g:MRU_File, '', 2)
  edit file1.txt
  edit file2.txt
  edit file3.txt
  let f2 = readfile(g:MRU_File, '', 2)
  edit def.c
  let f3 = readfile(g:MRU_File, '', 2)
  let g:MRU_Exclude_Files=''
  edit file1.txt
  let f4 = readfile(g:MRU_File, '', 2)
  if f1[1] =~# 'abc.c' && f2[1] =~# 'abc.c' && f3[1] =~# 'def.c' &&
        \ f4[1] =~# 'file1.txt'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test32
" If the MRU window is open, when adding a file name to the list, the MRU
" window should be refreshed.
" ==========================================================================
func Test_32()
  let test_name = 'test32'
  enew | only

  MRU
  wincmd p
  edit abc.c
  wincmd p
  let s1 = getline(1)
  wincmd p
  edit file1.txt
  wincmd p
  let s2 = getline(1)
  close
  if s1 =~# 'abc.c' && s2 =~# 'file1.txt'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test33
" When MRU_Use_Current_Window is set, the MRU list should be displayed in
" the current window.
" Selecting a file from the MRU window should replace
" the MRU buffer with the selected file.
" ==========================================================================
func Test_33()
  let test_name = 'test33'
  enew | only

  edit file1.txt
  let g:MRU_Use_Current_Window=1
  MRU
  if winnr('$') == 1 && @% == g:MRU_buffer_name
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
  let g:MRU_Use_Current_Window=0
endfunc

" ==========================================================================
" Test34
" When MRU_Use_Current_Window is set, selecting a file from the MRU window
" should replace the MRU buffer with the selected file.
" ==========================================================================
func Test_34()
  let test_name = 'test34'
  enew | only

  let g:MRU_Use_Current_Window=1
  let w:marker=1
  MRU
  if winnr('$') == 1 && w:marker && @% == g:MRU_buffer_name
    call search('file2.txt')
    exe "normal \<Enter>"
    if winnr('$') == 1 && w:marker && @% == 'file2.txt'
      call LogResult(test_name, 'pass')
    else
      call LogResult(test_name, 'FAIL')
    endif
  else
    call LogResult(test_name, 'FAIL')
  endif
  unlet w:marker
  let g:MRU_Use_Current_Window=0
endfunc

" ==========================================================================
" Test35
" When MRU_Use_Current_Window is set, if the current buffer has unsaved
" changes, then the MRU window should be opened in a split window
" ==========================================================================
func Test_35()
  let test_name = 'test35'
  enew | only

  let g:MRU_Use_Current_Window=1
  set modified
  MRU
  if winnr('$') == 2 && winnr() == 2 && @% == g:MRU_buffer_name
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
  close
  set nomodified
  let g:MRU_Use_Current_Window=0
  enew | only
endfunc

" ==========================================================================
" Test36
" When MRU_Auto_Close is not set, the MRU window should not automatically
" close when a file is selected. The MRU window should be kept open.
" ==========================================================================
func Test_36()
  let test_name = 'test36'
  enew | only

  let g:MRU_Auto_Close=0
  new
  MRU
  call search('file1.txt')
  exe "normal \<Enter>"
  2wincmd w
  MRU
  call search('file2.txt')
  exe "normal \<Enter>"
  if winnr('$') == 3 &&
        \ bufwinnr('file1.txt') == 1 &&
        \ bufwinnr('file2.txt') == 2 &&
        \ bufwinnr(g:MRU_buffer_name) == 3
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif

  wincmd b
  close
  let g:MRU_Auto_Close=1
  only
endfunc

" ==========================================================================
" Test37
" When MRU_Open_File_Use_Tabs is set, a selected file should be opened in a
" tab. If the file is already opened in a tab, then the focus should be moved
" to that tab.
" ==========================================================================
func Test_37()
  let test_name = 'test37'
  enew | only

  let g:MRU_Open_File_Use_Tabs=1
  edit file1.txt
  MRU
  call search('file2.txt')
  exe "normal \<Enter>"
  MRU
  call search('file3.txt')
  exe "normal \<Enter>"
  MRU file1.txt
  let t1 = tabpagenr()
  MRU
  call search('file2.txt')
  exe "normal \<Enter>"
  let t2 = tabpagenr()
  MRU
  call search('file3.txt')
  exe "normal \<Enter>"
  let t3 = tabpagenr()

  tabonly | enew

  if t1 == 1 && t2 == 2 && t3 == 3
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif

  let g:MRU_Open_File_Use_Tabs=0
endfunc

" ==========================================================================
" Test38
" If the MRU_Window_Open_Always is set to 0, when the MRU command finds a
" single matching file name, then it should open the MRU window. If this
" variable is set to 1, then the file should be opened without opening the MRU
" window.
" ==========================================================================
func Test_38()
  let test_name = 'test38'
  enew | only

  edit file3.txt
  enew

  let g:MRU_Window_Open_Always=1
  MRU file3.txt
  if winnr('$') == 2 &&
        \ bufwinnr(g:MRU_buffer_name) == 2
    let test_result = 'pass'
  else
    let test_result = 'FAIL'
  endif
  close

  enew | only

  if test_result == 'pass'
    let g:MRU_Window_Open_Always=0
    MRU file3.txt
    if winnr('$') == 1 &&
          \ bufwinnr('file3.txt') == 1
      let test_result = 'pass'
    else
      let test_result = 'FAIL'
    endif
  endif

  let g:MRU_Window_Open_Always=0

  if test_result == 'pass'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test39
" If the current tabpage is empty, then pressing 't' in the MRU window
" should open the file in the current tabpage.
" ==========================================================================
func Test_39()
  let test_name = 'test39'
  enew | only | tabonly
  tabnew
  tabnew
  tabnext 2
  MRU
  call search('file2.txt')
  normal t
  if fnamemodify(@%, ':p:t') ==# 'file2.txt' && tabpagenr() == 2
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
    call LogResult(test_name, "file = " . fnamemodify(@%, ':p:t'))
    call LogResult(test_name, "tab page = " . tabpagenr())
  endif

  tabonly
endfunc

" ==========================================================================
" Test40
" Pressing 'd' in the MRU window should delete the file under the cursor
" from the MRU list
" ==========================================================================
func Test_40()
  let test_name = 'test40'
  edit file2.txt
  enew
  MRU
  call search('file2.txt')
  normal d
  close
  let l = readfile(g:MRU_File)
  if match(l, 'file2.txt') == -1
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
endfunc

" ==========================================================================
" Test41
" Running the :vimgrep command should not add the files to the MRU list
" ==========================================================================
func Test_41()
  let test_name = 'test41'
  call writefile(['bright'], 'dummy1.txt')
  call writefile(['bright'], 'dummy2.txt')
  vimgrep /bright/j dummy*
  let l = readfile(g:MRU_File)
  if match(l, 'dummy') == -1
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
  call delete('dummy1.txt')
  call delete('dummy2.txt')
endfunc

" ==========================================================================
" Test42
" Using a command modifier with the MRU command to open the MRU window
" ==========================================================================
func Test_42()
  if v:version < 800
    " The <mods> command modifier is supported only by Vim 8.0 and above
    return
  endif
  let test_name = 'test42'
  enew | only
  topleft MRU
  if winnr() == 1 && winnr('$') == 2
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
  enew | only
  botright MRU
  if winnr() == 2 && winnr('$') == 2
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
  enew | only
  botright MRU
  if winnr() == 2 && winnr('$') == 2
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
  enew | only
endfunc

" ==========================================================================
" Test43
" Opening a file using the MRU command should jump to the window containing
" the file (if it is already opened).
" ==========================================================================
func Test_43()
  let test_name = 'test43'
  only
  edit file3.txt
  below split file2.txt
  below split file1.txt
  wincmd t
  MRU file1.txt
  if winnr() != 3 || fnamemodify(@%, ':p:t') !=# 'file1.txt'
    call LogResult(test_name, 'FAIL (1)')
  else
    MRU file2.txt
    if winnr() != 2 && fnamemodify(@%, ':p:t') !=# 'file2.txt'
      call LogResult(test_name, 'FAIL (2)')
    else
      MRU file3.txt
      if winnr() != 1 && fnamemodify(@%, ':p:t') !=# 'file3.txt'
        call LogResult(test_name, 'FAIL (3)')
      else
        call LogResult(test_name, 'pass')
      endif
    endif
  endif
  enew | only
endfunc

" ==========================================================================
" Test44
" Opening a file using the MRU command should open the file in a new window if
" the current buffer has unsaved changes.
" ==========================================================================
func Test_44()
  let test_name = 'test44'
  only
  set modified
  MRU file2.txt
  if winnr('$') == 2 && winnr() == 1 &&
        \ fnamemodify(@%, ':p:t') ==# 'file2.txt'
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
  close
  set nomodified
endfunc

" ==========================================================================
" Test45
" Opening a file from the MRU window using 'v' should open the file in a new
" window if the current buffer has unsaved changes.
" ==========================================================================
func Test_45()
  let test_name = 'test45'
  only
  set modified
  MRU
  call search('file3.txt')
  normal v
  if winnr('$') == 2 && winnr() == 1
        \ && fnamemodify(@%, ':p:t') ==# 'file3.txt'
        \ && &readonly
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
  close
  set nomodified
endfunc

" ==========================================================================
" Test46
" Specify a count to the :MRU command to set the MRU window height/width
" ==========================================================================
func Test_46()
  let test_name = 'test46'
  only
  " default height is 8
  MRU
  if winnr() != 2 || winheight(0) != 8
    call LogResult(test_name, 'FAIL (1)')
    return
  endif
  close

  " use a specific height value
  15MRU
  if winnr() != 2 || winheight(0) != 15
    call LogResult(test_name, 'FAIL (2)')
    return
  endif
  close

  if v:version >= 800
    " use a specific height value with a command modifier
    topleft 12MRU
    if winnr() != 1 || winheight(0) != 12
      call LogResult(test_name, 'FAIL (3)')
      return
    endif
    close

    " check for the width (leftmost window)
    vertical topleft 20MRU
    if winnr() != 1 || winwidth(0) != 20
      call LogResult(test_name, 'FAIL (4)')
      return
    endif
    close

    " check for the width (rightmost window)
    vertical botright 25MRU
    if winnr() != 2 || winwidth(0) != 25
      call LogResult(test_name, 'FAIL (5)')
      return
    endif
    close
  endif

  call LogResult(test_name, 'pass')
endfunc

" ==========================================================================
" Test47
" The height of the MRU window should be MRU_Window_Height
" ==========================================================================
func Test_47()
  let test_name = 'test47'
  only

  " default height is 8
  MRU
  if winheight(0) != 8
    call LogResult(test_name, 'FAIL (1)')
    return
  endif
  close

  let g:MRU_Window_Height = 2
  MRU
  if winheight(0) != 2
    call LogResult(test_name, 'FAIL (2)')
    return
  endif
  close
  let g:MRU_Window_Height = 12
  MRU
  if winheight(0) != 12
    call LogResult(test_name, 'FAIL (3)')
    return
  endif
  close

  call LogResult(test_name, 'pass')
  let g:MRU_Window_Height = 8
endfunc

" ==========================================================================
" Test48
" Fuzzy search file names with MRU_FuzzyMatch set to 1.
" ==========================================================================
func Test_48()
  if !exists('*matchfuzzy')
    return
  endif

  let test_name = 'test48'
  enew | only

  let g:MRU_FuzzyMatch = 1
  MRU F1
  if fnamemodify(@%, ':p:t') ==# 'file1.txt' && winnr('$') == 1
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL (1)')
  endif

  let g:MRU_FuzzyMatch = 0
  redir => msg
  MRU F1
  redir END
  if msg =~# "MRU file list doesn't contain files matching F1"
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL (2)')
  endif
  let g:MRU_FuzzyMatch = 1
endfunc

" ==========================================================================
" Test49
" Test for creating a new file by saving an unnamed buffer.
" ==========================================================================
func Test_49()
  let test_name = 'test49'
  enew | only
  call setline(1, 'sample file')
  write sample.txt
  let l = readfile(g:MRU_File)
  if match(l, 'sample.txt') != -1
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL')
  endif
  call delete('sample.txt')
  bwipe sample.txt
endfunc

" ==========================================================================
" Test50
" Test for the MruGetFiles() function
" ==========================================================================
func Test_50()
  let test_name = 'test50'
  enew | only
  let list1 = MruGetFiles()
  let list2 = readfile(g:MRU_File)
  if list1 != list2[1:]
    call LogResult(test_name, 'FAIL (1)')
    return
  endif

  if MruGetFiles('x1y2z3') == []
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL (2)')
  endif
endfunc

" ==========================================================================
" Test51
" Test for the :MruRefresh command
" ==========================================================================
func Test_51()
  let test_name = 'test51'
  enew | only
  if match(MruGetFiles(), 'sample.txt') == -1
    call LogResult(test_name, 'FAIL (1)')
    return
  endif
  MruRefresh
  if match(MruGetFiles(), 'sample.txt') == -1
    call LogResult(test_name, 'pass')
  else
    call LogResult(test_name, 'FAIL (2)')
  endif
endfunc

" ==========================================================================
" Test52
" Test for the re-opening a deleted buffer from the MRU list
" ==========================================================================
func Test_52()
  let test_name = 'test52'
  edit file1.txt
  edit file2.txt
  bd
  " select the file from the MRU window
  MRU
  call search('file2.txt')
  exe "normal \<Enter>"
  if !&buflisted || fnamemodify(@%, ':p:t') !=# 'file2.txt'
    call LogResult(test_name, 'FAIL (1)')
    return
  endif
  " open the file directly using the command
  bw file1.txt file2.txt
  edit file2.txt
  edit file1.txt
  bd
  MRU file1.txt
  if !&buflisted || fnamemodify(@%, ':p:t') !=# 'file1.txt'
    call LogResult(test_name, 'FAIL (2)')
    return
  endif
  call LogResult(test_name, 'pass')
endfunc

" ==========================================================================
" Test53
" Test for using a command modifier when directly opening a file using the
" MRU command.
" ==========================================================================
func Test_53()
  if v:version < 800
    return
  endif
  let test_name = 'test53'
  %bw!
  topleft MRU file2.txt
  if winnr('$') == 2 && winnr() == 1 && fnamemodify(@%, ':p:t') ==# 'file2.txt'
    wincmd j
    if winnr() != 2
      call LogResult(test_name, 'FAIL (1)')
      return
    endif
  else
    call LogResult(test_name, 'FAIL (2)')
    return
  endif
  %bw
  belowright MRU file2.txt
  if winnr('$') == 2 && winnr() == 2 && fnamemodify(@%, ':p:t') ==# 'file2.txt'
    wincmd k
    if winnr() != 1
      call LogResult(test_name, 'FAIL (3)')
      return
    endif
  else
    call LogResult(test_name, 'FAIL (4)')
    return
  endif
  %bw
  vertical topleft MRU file2.txt
  if winnr('$') == 2 && winnr() == 1 && fnamemodify(@%, ':p:t') ==# 'file2.txt'
    wincmd l
    if winnr() != 2
      call LogResult(test_name, 'FAIL (5)')
      return
    endif
  else
    call LogResult(test_name, 'FAIL (6)')
    return
  endif
  %bw
  vertical belowright MRU file2.txt
  if winnr('$') == 2 && winnr() == 2 && fnamemodify(@%, ':p:t') ==# 'file2.txt'
    wincmd h
    if winnr() != 1
      call LogResult(test_name, 'FAIL (7)')
      return
    endif
  else
    call LogResult(test_name, 'FAIL (8)')
    return
  endif
  %bw
  tab MRU file2.txt
  if tabpagenr() != 2 || fnamemodify(@%, ':p:t') !=# 'file2.txt'
    call LogResult(test_name, 'FAIL (9)')
    return
  endif
  %bw
  call LogResult(test_name, 'pass')
endfunc

" ==========================================================================
" Test54
" Test for the :MRUToggle command.
" ==========================================================================
func Test_54()
  let test_name = 'test54'
  only
  " open the MRU window
  MRUToggle
  if bufwinnr(g:MRU_buffer_name) != 2 || winnr() != 2
    call LogResult(test_name, 'FAIL (1)')
    return
  endif
  " close the MRU window
  MRUToggle
  if bufwinnr(g:MRU_buffer_name) != -1 || winnr() != 1
    call LogResult(test_name, 'FAIL (2)')
    return
  endif
  " close the MRU window from some other window
  MRUToggle
  wincmd k
  MRUToggle
  if bufwinnr(g:MRU_buffer_name) != -1 || winnr() != 1
    call LogResult(test_name, 'FAIL (3)')
    return
  endif
  call LogResult(test_name, 'pass')
endfunc

" ==========================================================================
" Test55
" Editing a file selected from the MRU window should set the current file to
" be the alternate file.
" ==========================================================================
func Test_55()
  let test_name = 'test55'
  silent! bw file1.txt file2.txt file3.txt
  new
  edit file1.txt
  edit file2.txt
  MRU
  call search('file3.txt')
  exe "normal \<Enter>"
  if fnamemodify(@%, ':p:t') !=# 'file3.txt'
        \ || fnamemodify(@#, ':p:t') !=# 'file2.txt'
    call LogResult(test_name, 'FAIL')
    return
  endif
  call LogResult(test_name, 'pass')
endfunc

" ==========================================================================
" Test56
" With MRU_Use_Current_Window set to 1, editing a file from the MRU list
" should not change the alternate file.
" ==========================================================================
func Test_56()
  let test_name = 'test56'
  let g:MRU_Use_Current_Window = 1
  bw file1.txt file2.txt file3.txt
  new
  edit file3.txt
  edit file1.txt
  edit file2.txt
  MRU
  call search('file3.txt')
  exe "normal \<Enter>"
  if fnamemodify(@%, ':p:t') !=# 'file3.txt'
        \ || fnamemodify(@#, ':p:t') !=# 'file2.txt'
    call LogResult(test_name, 'FAIL (1)')
    return
  endif
  " try viewing a file
  MRU
  call search('file1.txt')
  normal v
  if fnamemodify(@%, ':p:t') !=# 'file1.txt'
        \ || fnamemodify(@#, ':p:t') !=# 'file3.txt'
        \ || !&readonly
    call LogResult(test_name, 'FAIL (2)')
    return
  endif
  " try opening a wiped out buffer
  bw file2.txt
  MRU
  call search('file2.txt')
  exe "normal \<Enter>"
  if fnamemodify(@%, ':p:t') !=# 'file2.txt'
        \ || fnamemodify(@#, ':p:t') !=# 'file1.txt'
        \ || &readonly
    call LogResult(test_name, 'FAIL (3)')
    return
  endif
  let g:MRU_Use_Current_Window = 0
  bw!
  call LogResult(test_name, 'pass')
endfunc

" ==========================================================================
" Test57
" When the MRU window is closed, the MRU buffer should be unloaded.
" If 'MRU_Use_Current_Window' is set, then the MRU buffer should be wiped out.
" ==========================================================================
func Test_57()
  let test_name = 'test57'
  MRU
  let mrubnum = bufnr('')
  close
  if bufloaded(mrubnum)
    call LogResult(test_name, 'FAIL (1)')
    return
  endif
  let g:MRU_Use_Current_Window = 1
  new
  edit Xfile
  MRU
  let mrubnum = bufnr('')
  edit #
  if bufexists(mrubnum) || @% != 'Xfile'
    call LogResult(test_name, 'FAIL (2)')
    return
  endif
  let g:MRU_Use_Current_Window = 0
  bw!
  call LogResult(test_name, 'pass')
endfunc

" ==========================================================================
" Test58
" When the MRU window is toggled with MRU_Use_Current_Window set to 1, the
" previous buffer should be loaded.
" ==========================================================================
func Test_58()
  let test_name = 'test58'
  let g:MRU_Use_Current_Window = 1
  new
  edit Xfile
  MRUToggle
  if @% != g:MRU_buffer_name || winnr('$') != 2
    call LogResult(test_name, 'FAIL (1)')
    return
  endif
  MRUToggle
  if @% != 'Xfile' || winnr('$') != 2
    call LogResult(test_name, 'FAIL (2)')
    return
  endif
  let g:MRU_Use_Current_Window = 0
  bw!
  call LogResult(test_name, 'pass')
endfunc

" ==========================================================================
" Test59
" When the MRU_Set_Alternate_File is set to 1, on plugin startup, the
" alternate file should be set to the first file in the MRU list.
" ==========================================================================
func Test_59()
  if v:version < 802
    return
  endif
  let test_name = 'test59'
  call writefile([], 'Xfirstfile')
  edit Xfirstfile
  call writefile([
        \ "let MRU_File='vim_mru_file'",
        \ "let MRU_Set_Alternate_File=1",
        \ "source ../plugin/mru.vim",
        \ "call writefile([@#], 'Xoutput')"
        \ ], 'Xscript')
  silent! !vim -u NONE --noplugin i NONE -N -S Xscript -c "qa"
  if !filereadable('Xoutput')
    call LogResult(test_name, 'FAIL (1)')
  else
    let lines = readfile('Xoutput')
    if len(lines) == 1 && lines[0] =~ 'Xfirstfile$'
      call LogResult(test_name, 'pass')
    else
      call LogResult(test_name, 'FAIL (2)')
    endif
  endif
  call delete('Xscript')
  call delete('Xoutput')
  call delete('Xfirstfile')
endfunc

" ==========================================================================
" Test60
" With MRU_Use_Current_Window set to 1, MRU opens a selected file in the
" current window, even when the file is already open in another window
" ==========================================================================
func Test_60()
  let test_name = 'test60'
  let g:MRU_Use_Current_Window = 1

  edit file1.txt
  let bnum = bufnr()
  only
  below split file2.txt

  MRU
  call search('file1.txt')
  exe "normal \<Enter>"

  if winnr() == 2 && winbufnr(1) == bnum && winbufnr(2) == bnum
    call LogResult(test_name, "pass")
  else
    call LogResult(test_name, "FAIL")
  endif
  let g:MRU_Use_Current_Window = 0
endfunc

" ==========================================================================

" Create the files used by the tests
call writefile(['MRU test file1'], 'file1.txt')
call writefile(['MRU test file2'], 'file2.txt')
call writefile(['MRU test file3'], 'file3.txt')

call writefile(['#include <stdio.h', 'int main(){}'], 'abc.c')
call writefile(['#include <stdlib.h', 'int main(){}'], 'def.c')

" Remove the results from the previous test runs
call delete('results.txt')
call delete(g:MRU_File)
let results = []

" Generate a sorted list of Test_ functions to run
redir @q
silent function /^Test_
redir END
let s:tests = split(substitute(@q, '\(function\) \(\k*()\)', '\2', 'g'))

" Run the tests
set nomore
set debug=beep
for one_test in sort(s:tests)
  exe 'call ' . one_test
endfor
set more

call writefile(results, 'results.txt')

" TODO:
" Add the following tests:
" 1. When the MRU list is modified, the MRU menu should be refreshed.
" 2. Try to jump to an already open file from the MRU window and using the
"     MRU command.

" Cleanup the files used by the tests
call delete('file1.txt')
call delete('file2.txt')
call delete('file3.txt')
call delete('abc.c')
call delete('def.c')
call delete(g:MRU_File)

" End of unit test execution
qall

" vim: shiftwidth=2 sts=2 expandtab
