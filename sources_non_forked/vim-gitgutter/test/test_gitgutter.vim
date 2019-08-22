let s:current_dir = expand('%:p:h')
let s:test_repo   = s:current_dir.'/test-repo'
let s:bufnr       = bufnr('')

"
" Helpers
"

" Ignores unexpected keys.
"
" expected - list of signs
function s:assert_signs(expected, filename)
  if empty(a:expected)
    call assert_equal(a:expected, [])
    return
  endif

  let expected_keys = keys(a:expected[0])
  let actual = sign_getplaced(a:filename, {'group': 'gitgutter'})[0].signs

  for sign in actual
    for k in keys(sign)
      if index(expected_keys, k) == -1
        call remove(sign, k)
      endif
    endfor
  endfor

  call assert_equal(a:expected, actual)
endfunction

function s:git_diff()
  return split(system('git diff -U0 fixture.txt'), '\n')
endfunction

function s:git_diff_staged()
  return split(system('git diff -U0 --staged fixture.txt'), '\n')
endfunction

function s:trigger_gitgutter()
  doautocmd CursorHold
endfunction


"
" SetUp / TearDown
"

function SetUp()
  call system("git init ".s:test_repo.
        \ " && cd ".s:test_repo.
        \ " && cp ../fixture.txt .".
        \ " && git add . && git commit -m 'initial'".
        \ " && git config diff.mnemonicPrefix false")
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
  call s:trigger_gitgutter()

  let expected = [{'lnum': 2, 'name': 'GitGutterLineAdded', 'group': 'gitgutter', 'priority': 10}]
  call s:assert_signs(expected, 'fixture.txt')
endfunction


function Test_add_lines_fish()
  let _shell = &shell
  set shell=/usr/local/bin/fish

  normal ggo*
  call s:trigger_gitgutter()

  let expected = [{'lnum': 2, 'name': 'GitGutterLineAdded'}]
  call s:assert_signs(expected, 'fixture.txt')

  let &shell = _shell
endfunction


function Test_modify_lines()
  normal ggi*
  call s:trigger_gitgutter()

  let expected = [{'lnum': 1, 'name': 'GitGutterLineModified'}]
  call s:assert_signs(expected, 'fixture.txt')
endfunction


function Test_remove_lines()
  execute '5d'
  call s:trigger_gitgutter()

  let expected = [{'lnum': 4, 'name': 'GitGutterLineRemoved'}]
  call s:assert_signs(expected, 'fixture.txt')
endfunction


function Test_remove_first_lines()
  execute '1d'
  call s:trigger_gitgutter()

  let expected = [{'lnum': 1, 'name': 'GitGutterLineRemovedFirstLine'}]
  call s:assert_signs(expected, 'fixture.txt')
endfunction


function Test_priority()
  let g:gitgutter_sign_priority = 5

  execute '1d'
  call s:trigger_gitgutter()

  call s:assert_signs([{'priority': 5}], 'fixture.txt')

  let g:gitgutter_sign_priority = 10
endfunction


function Test_overlapping_hunks()
  execute '3d'
  execute '1d'
  call s:trigger_gitgutter()

  let expected = [{'lnum': 1, 'name': 'GitGutterLineRemovedAboveAndBelow'}]
  call s:assert_signs(expected, 'fixture.txt')
endfunction


function Test_edit_file_with_same_name_as_a_branch()
  normal 5Gi*
  call system('git checkout -b fixture.txt')
  call s:trigger_gitgutter()

  let expected = [{'lnum': 5, 'name': 'GitGutterLineModified'}]
  call s:assert_signs(expected, 'fixture.txt')
endfunction


function Test_file_added_to_git()
  let tmpfile = 'fileAddedToGit.tmp'
  call system('touch '.tmpfile.' && git add '.tmpfile)
  execute 'edit '.tmpfile
  normal ihello
  call s:trigger_gitgutter()

  let expected = [{'lnum': 1, 'name': 'GitGutterLineAdded'}]
  call s:assert_signs(expected, 'fileAddedToGit.tmp')
endfunction


function Test_filename_with_equals()
  call system('touch =fixture=.txt && git add =fixture=.txt')
  edit =fixture=.txt
  normal ggo*
  call s:trigger_gitgutter()

  let expected = [
        \ {'lnum': 1, 'name': 'GitGutterLineAdded'},
        \ {'lnum': 2, 'name': 'GitGutterLineAdded'}
        \ ]
  call s:assert_signs(expected, '=fixture=.txt')
endfunction


function Test_filename_with_square_brackets()
  call system('touch fix[tu]re.txt && git add fix[tu]re.txt')
  edit fix[tu]re.txt
  normal ggo*
  call s:trigger_gitgutter()

  let expected = [
        \ {'lnum': 1, 'name': 'GitGutterLineAdded'},
        \ {'lnum': 2, 'name': 'GitGutterLineAdded'}
        \ ]
  call s:assert_signs(expected, 'fix[tu]re.txt')
endfunction


function Test_filename_leading_dash()
  call system('touch -- -fixture.txt && git add -- -fixture.txt')
  edit -fixture.txt
  normal ggo*
  call s:trigger_gitgutter()

  let expected = [
        \ {'lnum': 1, 'name': 'GitGutterLineAdded'},
        \ {'lnum': 2, 'name': 'GitGutterLineAdded'}
        \ ]
  call s:assert_signs(expected, '-fixture.txt')
endfunction


function Test_filename_umlaut()
  call system('touch -- fixtüre.txt && git add -- fixtüre.txt')
  edit fixtüre.txt
  normal ggo*
  call s:trigger_gitgutter()

  let expected = [
        \ {'lnum': 1, 'name': 'GitGutterLineAdded'},
        \ {'lnum': 2, 'name': 'GitGutterLineAdded'}
        \ ]
  call s:assert_signs(expected, 'fixtüre.txt')
endfunction


" FIXME: this test fails when it is the first (or only) test to be run
function Test_follow_symlink()
  let tmp = 'symlink'
  call system('ln -nfs fixture.txt '.tmp)
  execute 'edit '.tmp
  6d
  call s:trigger_gitgutter()

  let expected = [{'lnum': 5, 'name': 'GitGutterLineRemoved'}]
  call s:assert_signs(expected, 'symlink')
endfunction


function Test_keep_alt()
  enew
  execute "normal! \<C-^>"

  call assert_equal('fixture.txt', bufname(''))
  call assert_equal('',            bufname('#'))

  normal ggx
  call s:trigger_gitgutter()

  call assert_equal('', bufname('#'))
endfunction


function Test_keep_modified()
  normal 5Go*
  call assert_equal(1, getbufvar('', '&modified'))

  call s:trigger_gitgutter()

  call assert_equal(1, getbufvar('', '&modified'))
endfunction


function Test_keep_op_marks()
  normal 5Go*
  call assert_equal([0,6,1,0], getpos("'["))
  call assert_equal([0,6,2,0], getpos("']"))

  call s:trigger_gitgutter()

  call assert_equal([0,6,1,0], getpos("'["))
  call assert_equal([0,6,2,0], getpos("']"))
endfunction


function Test_no_modifications()
  call s:assert_signs([], 'fixture.txt')
endfunction


function Test_orphaned_signs()
  execute "normal 5GoX\<CR>Y"
  call s:trigger_gitgutter()
  6d
  call s:trigger_gitgutter()

  let expected = [{'lnum': 6, 'name': 'GitGutterLineAdded'}]
  call s:assert_signs(expected, 'fixture.txt')
endfunction


function Test_untracked_file_outside_repo()
  let tmp = tempname()
  call system('touch '.tmp)
  execute 'edit '.tmp

  call s:assert_signs([], tmp)
endfunction


function Test_untracked_file_within_repo()
  let tmp = 'untrackedFileWithinRepo.tmp'
  call system('touch '.tmp)
  execute 'edit '.tmp
  normal ggo*
  call s:trigger_gitgutter()

  call s:assert_signs([], tmp)
  call assert_equal(-2, b:gitgutter.path)

  call system('rm '.tmp)
endfunction


function Test_untracked_file_square_brackets_within_repo()
  let tmp = '[un]trackedFileWithinRepo.tmp'
  call system('touch '.tmp)
  execute 'edit '.tmp
  normal ggo*
  call s:trigger_gitgutter()

  call s:assert_signs([], tmp)

  call system('rm '.tmp)
endfunction


function Test_hunk_outside_noop()
  5
  GitGutterStageHunk

  call s:assert_signs([], 'fixture.txt')
  call assert_equal([], s:git_diff())
  call assert_equal([], s:git_diff_staged())

  GitGutterUndoHunk

  call s:assert_signs([], 'fixture.txt')
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

  call s:assert_signs([], 'fixture.txt')

  " Buffer is unsaved
  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index ae8e546..f5c6aff 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -5 +5 @@ d',
        \ '-*e',
        \ '+e'
        \ ]
  call assert_equal(expected, s:git_diff())

  " Index has been updated
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

  " Save the buffer
  write

  call assert_equal([], s:git_diff())
endfunction


function Test_hunk_stage_nearby_hunk()
  execute "normal! 2Gox\<CR>y\<CR>z"
  normal 2jdd
  normal k
  GitGutterStageHunk

  let expected = [
        \ {'lnum': 3, 'name': 'GitGutterLineAdded'},
        \ {'lnum': 4, 'name': 'GitGutterLineAdded'},
        \ {'lnum': 5, 'name': 'GitGutterLineAdded'}
        \ ]
  call s:assert_signs(expected, 'fixture.txt')

  " Buffer is unsaved
  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index 53b13df..f5c6aff 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -3,0 +4 @@ c',
        \ '+d',
        \ ]
  call assert_equal(expected, s:git_diff())

  " Index has been updated
  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index f5c6aff..53b13df 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -4 +3,0 @@ c',
        \ '-d',
        \ ]
  call assert_equal(expected, s:git_diff_staged())

  " Save the buffer
  write

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
endfunction


function Test_hunk_stage_partial_visual_added()
  call append(5, ['A','B','C','D'])
  execute "normal 7GVj:GitGutterStageHunk\<CR>"

  let expected = [
        \ {'lnum': 6, 'name': 'GitGutterLineAdded'},
        \ {'lnum': 9, 'name': 'GitGutterLineAdded'},
        \ ]
  call s:assert_signs(expected, 'fixture.txt')

  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index 8a7026e..f5c6aff 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -6,2 +5,0 @@ e',
        \ '-B',
        \ '-C',
        \ ]
  call assert_equal(expected, s:git_diff())

  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index f5c6aff..8a7026e 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -5,0 +6,2 @@ e',
        \ '+B',
        \ '+C',
        \ ]
  call assert_equal(expected, s:git_diff_staged())
endfunction


function Test_hunk_stage_partial_cmd_added()
  call append(5, ['A','B','C','D'])
  6
  7,8GitGutterStageHunk

  let expected = [
        \ {'lnum': 6, 'name': 'GitGutterLineAdded'},
        \ {'lnum': 9, 'name': 'GitGutterLineAdded'},
        \ ]
  call s:assert_signs(expected, 'fixture.txt')

  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index 8a7026e..f5c6aff 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -6,2 +5,0 @@ e',
        \ '-B',
        \ '-C',
        \ ]
  call assert_equal(expected, s:git_diff())

  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index f5c6aff..8a7026e 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -5,0 +6,2 @@ e',
        \ '+B',
        \ '+C',
        \ ]
  call assert_equal(expected, s:git_diff_staged())
endfunction


function Test_hunk_stage_partial_preview_added()
  call append(5, ['A','B','C','D'])
  6
  GitGutterPreviewHunk
  wincmd P

  " remove C and A so we stage B and D
  3delete
  1delete

  GitGutterStageHunk
  write

  let expected = [
        \ {'lnum': 6, 'name': 'GitGutterLineAdded'},
        \ {'lnum': 8, 'name': 'GitGutterLineAdded'},
        \ ]
  call s:assert_signs(expected, 'fixture.txt')

  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index 975852f..3dd23a3 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -5,0 +6 @@ e',
        \ '+A',
        \ '@@ -6,0 +8 @@ B',
        \ '+C',
        \ ]
  call assert_equal(expected, s:git_diff())

  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index f5c6aff..975852f 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -5,0 +6,2 @@ e',
        \ '+B',
        \ '+D',
        \ ]
  call assert_equal(expected, s:git_diff_staged())
endfunction


function Test_hunk_stage_preview_write()
  call append(5, ['A','B','C','D'])
  6
  GitGutterPreviewHunk
  wincmd P

  " preview window
  call feedkeys(":w\<CR>", 'tx')
  " original window
  write

  call s:assert_signs([], 'fixture.txt')

  call assert_equal([], s:git_diff())

  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index f5c6aff..3dd23a3 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -5,0 +6,4 @@ e',
        \ '+A',
        \ '+B',
        \ '+C',
        \ '+D',
        \ ]
  call assert_equal(expected, s:git_diff_staged())
endfunction


function Test_hunk_stage_partial_preview_added_removed()
  4,5delete
  call append(3, ['A','B','C','D'])
  4
  GitGutterPreviewHunk
  wincmd P

  " -d
  " -e
  " +A
  " +B
  " +C
  " +D

  " remove D and d so they do not get staged
  6delete
  1delete

  GitGutterStageHunk
  write

  let expected = [
        \ {'lnum': 3, 'name': 'GitGutterLineRemoved'},
        \ {'lnum': 7, 'name': 'GitGutterLineAdded'},
        \ ]
  call s:assert_signs(expected, 'fixture.txt')

  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index 9a19589..e63fb0a 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -4 +3,0 @@ c',
        \ '-d',
        \ '@@ -7,0 +7 @@ C',
        \ '+D',
        \ ]
  call assert_equal(expected, s:git_diff())

  let expected = [
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index f5c6aff..9a19589 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -5 +5,3 @@ d',
        \ '-e',
        \ '+A',
        \ '+B',
        \ '+C',
        \ ]
  call assert_equal(expected, s:git_diff_staged())
endfunction


function Test_hunk_undo()
  let _shell = &shell
  set shell=foo

  normal 5Gi*
  GitGutterUndoHunk

  call assert_equal('foo', &shell)
  let &shell = _shell

  call s:assert_signs([], 'fixture.txt')
  call assert_equal([], s:git_diff())
  call assert_equal([], s:git_diff_staged())
endfunction


function Test_undo_nearby_hunk()
  execute "normal! 2Gox\<CR>y\<CR>z"
  normal 2jdd
  normal k
  call s:trigger_gitgutter()
  GitGutterUndoHunk
  call s:trigger_gitgutter()

  let expected = [
        \ {'lnum': 3, 'name': 'GitGutterLineAdded'},
        \ {'lnum': 4, 'name': 'GitGutterLineAdded'},
        \ {'lnum': 5, 'name': 'GitGutterLineAdded'}
        \ ]
  call s:assert_signs(expected, 'fixture.txt')

  call assert_equal([], s:git_diff())

  call assert_equal([], s:git_diff_staged())

  " Save the buffer
  write

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

endfunction


function Test_overlapping_hunk_op()
  func Answer(char)
    call feedkeys(a:char."\<CR>")
  endfunc

  " Undo upper

  execute '3d'
  execute '1d'
  call s:trigger_gitgutter()
  normal gg
  call timer_start(100, {-> Answer('u')} )
  GitGutterUndoHunk
  call s:trigger_gitgutter()

  let expected = [{'lnum': 2, 'name': 'GitGutterLineRemoved'}]
  call s:assert_signs(expected, 'fixture.txt')

  " Undo lower

  execute '1d'
  call s:trigger_gitgutter()
  normal gg
  call timer_start(100, {-> Answer('l')} )
  GitGutterUndoHunk
  call s:trigger_gitgutter()

  let expected = [{'lnum': 1, 'name': 'GitGutterLineRemovedFirstLine'}]
  call s:assert_signs(expected, 'fixture.txt')
endfunction


function Test_write_option()
  set nowrite

  normal ggo*
  call s:trigger_gitgutter()

  let expected = [{'lnum': 2, 'name': 'GitGutterLineAdded'}]
  call s:assert_signs(expected, 'fixture.txt')

  set write
endfunction


function Test_inner_text_object()
  execute "normal! 2Gox\<CR>y\<CR>z\<CR>\<CR>"
  call s:trigger_gitgutter()
  normal dic
  call s:trigger_gitgutter()

  call s:assert_signs([], 'fixture.txt')
  call assert_equal(readfile('fixture.txt'), getline(1,'$'))

  " Excludes trailing lines
  normal 9Gi*
  normal 10Gi*
  call s:trigger_gitgutter()
  execute "normal vic\<Esc>"
  call assert_equal([9, 10], [line("'<"), line("'>")])
endfunction


function Test_around_text_object()
  execute "normal! 2Gox\<CR>y\<CR>z\<CR>\<CR>"
  call s:trigger_gitgutter()
  normal dac
  call s:trigger_gitgutter()

  call s:assert_signs([], 'fixture.txt')
  call assert_equal(readfile('fixture.txt'), getline(1,'$'))

  " Includes trailing lines
  normal 9Gi*
  normal 10Gi*
  call s:trigger_gitgutter()
  execute "normal vac\<Esc>"
  call assert_equal([9, 11], [line("'<"), line("'>")])
endfunction


function Test_user_autocmd()
  autocmd User GitGutter let s:autocmd_user = g:gitgutter_hook_context.bufnr

  " Verify not fired when nothing changed.
  let s:autocmd_user = 0
  call s:trigger_gitgutter()
  call assert_equal(0, s:autocmd_user)

  " Verify fired when there was a change.
  normal ggo*
  let bufnr = bufnr('')
  call s:trigger_gitgutter()
  call assert_equal(bufnr, s:autocmd_user)
endfunction


function Test_fix_file_references()
  " No special characters
  let hunk_diff = join([
        \ 'diff --git a/fixture.txt b/fixture.txt',
        \ 'index f5c6aff..3fbde56 100644',
        \ '--- a/fixture.txt',
        \ '+++ b/fixture.txt',
        \ '@@ -2,0 +3,1 @@ b',
        \ '+x'
        \ ], "\n")."\n"
  let filepath = 'blah.txt'

  let expected = join([
        \ 'diff --git a/blah.txt b/blah.txt',
        \ 'index f5c6aff..3fbde56 100644',
        \ '--- a/blah.txt',
        \ '+++ b/blah.txt',
        \ '@@ -2,0 +3,1 @@ b',
        \ '+x'
        \ ], "\n")."\n"

  call assert_equal(expected, gitgutter#hunk#fix_file_references(filepath, hunk_diff))

  " diff.mnemonicPrefix; spaces in filename
  let hunk_diff = join([
        \ 'diff --git i/x/cat dog w/x/cat dog',
        \ 'index f5c6aff..3fbde56 100644',
        \ '--- i/x/cat dog',
        \ '+++ w/x/cat dog',
        \ '@@ -2,0 +3,1 @@ b',
        \ '+x'
        \ ], "\n")."\n"
  let filepath = 'blah.txt'

  let expected = join([
        \ 'diff --git i/blah.txt w/blah.txt',
        \ 'index f5c6aff..3fbde56 100644',
        \ '--- i/blah.txt',
        \ '+++ w/blah.txt',
        \ '@@ -2,0 +3,1 @@ b',
        \ '+x'
        \ ], "\n")."\n"

  call assert_equal(expected, gitgutter#hunk#fix_file_references(filepath, hunk_diff))

  " Backslashes in filename; quotation marks
  let hunk_diff = join([
        \ 'diff --git "a/C:\\Users\\FOO~1.PAR\\AppData\\Local\\Temp\\nvimJcmSv9\\11.1.vim" "b/C:\\Users\\FOO~1.PAR\\AppData\\Local\\Temp\\nvimJcmSv9\\12.1.vim"',
        \ 'index f42aeb0..4930403 100644',
        \ '--- "a/C:\\Users\\FOO~1.PAR\\AppData\\Local\\Temp\\nvimJcmSv9\\11.1.vim"',
        \ '+++ "b/C:\\Users\\FOO~1.PAR\\AppData\\Local\\Temp\\nvimJcmSv9\\12.1.vim"',
        \ '@@ -172,0 +173 @@ stuff',
        \ '+x'
        \ ], "\n")."\n"
  let filepath = 'init.vim'

  let expected = join([
        \ 'diff --git "a/init.vim" "b/init.vim"',
        \ 'index f42aeb0..4930403 100644',
        \ '--- "a/init.vim"',
        \ '+++ "b/init.vim"',
        \ '@@ -172,0 +173 @@ stuff',
        \ '+x'
        \ ], "\n")."\n"

  call assert_equal(expected, gitgutter#hunk#fix_file_references(filepath, hunk_diff))
endfunction


function Test_encoding()
  call system('cp ../cp932.txt . && git add cp932.txt')
  edit ++enc=cp932 cp932.txt

  call s:trigger_gitgutter()

  call s:assert_signs([], 'cp932.txt')
endfunction


function Test_empty_file()
  " 0-byte file
  call system('touch empty.txt && git add empty.txt')
  edit empty.txt

  call s:trigger_gitgutter()
  call s:assert_signs([], 'empty.txt')


  " File consisting only of a newline
  call system('echo "" > newline.txt && git add newline.txt')
  edit newline.txt

  call s:trigger_gitgutter()
  call s:assert_signs([], 'newline.txt')


  " 1 line file without newline
  " Vim will force a newline unless we tell it not to.
  call system('echo -n a > oneline.txt && git add oneline.txt')
  set noeol nofixeol
  edit! oneline.txt

  call s:trigger_gitgutter()
  call s:assert_signs([], 'oneline.txt')

  set eol fixeol
endfunction
