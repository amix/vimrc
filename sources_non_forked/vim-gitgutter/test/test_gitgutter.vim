let s:current_dir = expand('%:p:h')
let s:test_repo   = s:current_dir.'/test-repo'
let s:bufnr       = bufnr('')

"
" Helpers
"

function s:signs(filename)
  redir => signs
    silent execute 'sign place'
  redir END

  let signs = split(signs, '\n')

  " filter out signs for this test file
  " assumes a:filename's signs are last set listed
  let i = index(signs, 'Signs for '.a:filename.':')
  let signs = (i > -1 ? signs[i+1:] : [])

  call map(signs, {_, v -> substitute(v, '    ', '', '')})

  return signs
endfunction

function s:git_diff()
  return split(system('git diff -U0 fixture.txt'), '\n')
endfunction

function s:git_diff_staged()
  return split(system('git diff -U0 --staged fixture.txt'), '\n')
endfunction


"
" SetUp / TearDown
"

function SetUp()
  call system("git init ".s:test_repo.
        \ " && cd ".s:test_repo.
        \ " && cp ../fixture.txt .".
        \ " && git add . && git commit -m 'initial'")
  execute ':cd' s:test_repo
  edit! fixture.txt
  call gitgutter#sign#reset()
endfunction

function TearDown()
  " delete all buffers except this one
  " TODO: move to runner.vim, accounting for multiple test files
  if s:bufnr > 1
    silent! execute '1,'.s:bufnr-1.'bdelete!'
  endif
  silent! execute s:bufnr+1.',$bdelete!'

  execute ':cd' s:current_dir
  call system("rm -rf ".s:test_repo)
endfunction

"
" The tests
"

function Test_add_lines()
  normal ggo*
  write

  let expected = ["line=2  id=3000  name=GitGutterLineAdded"]
  call assert_equal(expected, s:signs('fixture.txt'))
endfunction


function Test_add_lines_fish()
  let _shell = &shell
  set shell=/usr/local/bin/fish

  normal ggo*
  write

  let expected = ["line=2  id=3000  name=GitGutterLineAdded"]
  call assert_equal(expected, s:signs('fixture.txt'))

  let &shell = _shell
endfunction


function Test_modify_lines()
  normal ggi*
  write

  let expected = ["line=1  id=3000  name=GitGutterLineModified"]
  call assert_equal(expected, s:signs('fixture.txt'))
endfunction


function Test_remove_lines()
  execute '5d'
  write

  let expected = ["line=4  id=3000  name=GitGutterLineRemoved"]
  call assert_equal(expected, s:signs('fixture.txt'))
endfunction


function Test_remove_first_lines()
  execute '1d'
  write

  let expected = ["line=1  id=3000  name=GitGutterLineRemovedFirstLine"]
  call assert_equal(expected, s:signs('fixture.txt'))
endfunction


function Test_edit_file_with_same_name_as_a_branch()
  normal 5Gi*
  call system('git checkout -b fixture.txt')
  write

  let expected = ["line=5  id=3000  name=GitGutterLineModified"]
  call assert_equal(expected, s:signs('fixture.txt'))
endfunction


function Test_file_added_to_git()
  let tmpfile = 'fileAddedToGit.tmp'
  call system('touch '.tmpfile.' && git add '.tmpfile)
  execute 'edit '.tmpfile
  normal ihello
  write

  let expected = ["line=1  id=3000  name=GitGutterLineAdded"]
  call assert_equal(expected, s:signs('fileAddedToGit.tmp'))
endfunction


function Test_filename_with_equals()
  call system('touch =fixture=.txt && git add =fixture=.txt')
  edit =fixture=.txt
  normal ggo*
  write

  let expected = [
        \ 'line=1  id=3000  name=GitGutterLineAdded',
        \ 'line=2  id=3001  name=GitGutterLineAdded'
        \ ]
  call assert_equal(expected, s:signs('=fixture=.txt'))
endfunction


function Test_filename_with_square_brackets()
  call system('touch fix[tu]re.txt && git add fix[tu]re.txt')
  edit fix[tu]re.txt
  normal ggo*
  write

  let expected = [
        \ 'line=1  id=3000  name=GitGutterLineAdded',
        \ 'line=2  id=3001  name=GitGutterLineAdded'
        \ ]
  call assert_equal(expected, s:signs('fix[tu]re.txt'))
endfunction


" FIXME: this test fails when it is the first (or only) test to be run
function Test_follow_symlink()
  let tmp = 'symlink'
  call system('ln -nfs fixture.txt '.tmp)
  execute 'edit '.tmp
  6d
  write

  let expected = ['line=5  id=3000  name=GitGutterLineRemoved']
  call assert_equal(expected, s:signs('symlink'))
endfunction


function Test_keep_alt()
  enew
  execute "normal! \<C-^>"

  call assert_equal('fixture.txt', bufname(''))
  call assert_equal('',            bufname('#'))

  normal ggx
  doautocmd CursorHold

  call assert_equal('', bufname('#'))
endfunction


function Test_keep_modified()
  normal 5Go*
  call assert_equal(1, getbufvar('', '&modified'))

  doautocmd CursorHold

  call assert_equal(1, getbufvar('', '&modified'))
endfunction


function Test_keep_op_marks()
  normal 5Go*
  call assert_equal([0,6,1,0], getpos("'["))
  call assert_equal([0,6,2,0], getpos("']"))

  doautocmd CursorHold

  call assert_equal([0,6,1,0], getpos("'["))
  call assert_equal([0,6,2,0], getpos("']"))
endfunction


function Test_no_modifications()
  call assert_equal([], s:signs('fixture.txt'))
endfunction


function Test_orphaned_signs()
  execute "normal 5GoX\<CR>Y"
  write
  6d
  write

  let expected = ['line=6  id=3001  name=GitGutterLineAdded']
  call assert_equal(expected, s:signs('fixture.txt'))
endfunction


function Test_sign_column_always()
  let g:gitgutter_sign_column_always=1
  write

  let expected = ['line=9999  id=2999  name=GitGutterDummy']
  call assert_equal(expected, s:signs('fixture.txt'))

  let g:gitgutter_sign_column_always=0
endfunction


function Test_untracked_file_outside_repo()
  let tmp = tempname()
  call system('touch '.tmp)
  execute 'edit '.tmp

  call assert_equal([], s:signs(tmp))
endfunction


function Test_untracked_file_within_repo()
  let tmp = 'untrackedFileWithinRepo.tmp'
  call system('touch '.tmp)
  execute 'edit '.tmp
  normal ggo*
  doautocmd CursorHold

  call assert_equal([], s:signs(tmp))

  call system('rm '.tmp)
endfunction


function Test_untracked_file_square_brackets_within_repo()
  let tmp = '[un]trackedFileWithinRepo.tmp'
  call system('touch '.tmp)
  execute 'edit '.tmp
  normal ggo*
  doautocmd CursorHold

  call assert_equal([], s:signs(tmp))

  call system('rm '.tmp)
endfunction


function Test_hunk_outside_noop()
  normal 5G
  GitGutterStageHunk

  call assert_equal([], s:signs('fixture.txt'))
  call assert_equal([], s:git_diff())
  call assert_equal([], s:git_diff_staged())

  GitGutterUndoHunk

  call assert_equal([], s:signs('fixture.txt'))
  call assert_equal([], s:git_diff())
  call assert_equal([], s:git_diff_staged())
endfunction


function Test_hunk_stage()
  let _shell = &shell
  set shell=foo

  normal 5Gi*
  GitGutterStageHunk

  call assert_equal('foo', &shell)
  let &shell = _shell

  call assert_equal([], s:signs('fixture.txt'))

  call assert_equal([], s:git_diff())

  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index f5c6aff..ae8e546 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -5 +5 @@ d',
        \ '-e',
        \ '+*e'
        \ ]
  call assert_equal(expected, s:git_diff_staged())
endfunction


function Test_hunk_stage_nearby_hunk()
  execute "normal! 2Gox\<CR>y\<CR>z"
  normal 2jdd
  normal k
  GitGutterStageHunk

  let expected = [
        \ 'line=3  id=3000  name=GitGutterLineAdded',
        \ 'line=4  id=3001  name=GitGutterLineAdded',
        \ 'line=5  id=3002  name=GitGutterLineAdded'
        \ ]
  call assert_equal(expected, s:signs('fixture.txt'))

  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index 53b13df..8fdfda7 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -2,0 +3,3 @@ b',
        \ '+x',
        \ '+y',
        \ '+z',
        \ ]
  call assert_equal(expected, s:git_diff())

  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index f5c6aff..53b13df 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -4 +3,0 @@ c',
        \ '-d',
        \ ]
  call assert_equal(expected, s:git_diff_staged())
endfunction


function Test_hunk_undo()
  let _shell = &shell
  set shell=foo

  normal 5Gi*
  GitGutterUndoHunk
  write  " write file so we can verify git diff (--staged)

  call assert_equal('foo', &shell)
  let &shell = _shell

  call assert_equal([], s:signs('fixture.txt'))
  call assert_equal([], s:git_diff())
  call assert_equal([], s:git_diff_staged())
endfunction


function Test_undo_nearby_hunk()
  execute "normal! 2Gox\<CR>y\<CR>z"
  normal 2jdd
  normal k
  GitGutterUndoHunk
  write  " write file so we can verify git diff (--staged)

  let expected = [
        \ 'line=3  id=3000  name=GitGutterLineAdded',
        \ 'line=4  id=3001  name=GitGutterLineAdded',
        \ 'line=5  id=3002  name=GitGutterLineAdded'
        \ ]
  call assert_equal(expected, s:signs('fixture.txt'))

  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index f5c6aff..3fbde56 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -2,0 +3,3 @@ b',
        \ '+x',
        \ '+y',
        \ '+z',
        \ ]
  call assert_equal(expected, s:git_diff())

  call assert_equal([], s:git_diff_staged())
endfunction
