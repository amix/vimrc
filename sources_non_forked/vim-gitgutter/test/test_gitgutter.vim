let s:current_dir = expand('%:p:h')
let s:test_repo   = s:current_dir.'/test-repo'
let s:bufnr       = bufnr('')

"
" Helpers
"

" Ignores unexpected keys in actual.
function s:assert_list_of_dicts(expected, actual)
  if empty(a:expected)
    call assert_equal([], a:actual)
    return
  endif

  let expected_keys = keys(a:expected[0])

  for dict in a:actual
    for k in keys(dict)
      if index(expected_keys, k) == -1
        call remove(dict, k)
      endif
    endfor
  endfor

  call assert_equal(a:expected, a:actual)
endfunction

" Ignores unexpected keys.
"
" expected - list of signs
function s:assert_signs(expected, filename)
  let actual = sign_getplaced(a:filename, {'group': 'gitgutter'})[0].signs
  call s:assert_list_of_dicts(a:expected, actual)
endfunction

function s:git_diff(...)
  return split(system('git diff -U0 '.(a:0 ? a:1 : 'fixture.txt')), '\n')
endfunction

function s:git_diff_staged(...)
  return split(system('git diff -U0 --staged '.(a:0 ? a:1 : 'fixture.txt')), '\n')
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
        \ " && cp ../.gitconfig .".
        \ " && cp ../.gitattributes .".
        \ " && cp ../fixture.foo .".
        \ " && cp ../fixture.txt .".
        \ " && cp ../fixture_dos.txt .".
        \ " && cp ../fixture_dos_noeol.txt .".
        \ " && git add . && git commit -m 'initial'".
        \ " && git config diff.mnemonicPrefix false")
  execute ':cd' s:test_repo
  edit! fixture.txt
  call gitgutter#sign#reset()

  " FIXME why won't vim autoload the file?
  execute 'source' '../../autoload/gitgutter/diff_highlight.vim'
  execute 'source' '../../autoload/gitgutter/fold.vim'
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


function Test_filename_with_space()
  call system('touch fix\ ture.txt && git add fix\ ture.txt')
  edit fix\ ture.txt
  normal ggo*
  call s:trigger_gitgutter()

  let expected = [
        \ {'lnum': 1, 'name': 'GitGutterLineAdded'},
        \ {'lnum': 2, 'name': 'GitGutterLineAdded'}
        \ ]
  call s:assert_signs(expected, 'fix\ ture.txt')
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


function Test_file_cmd()
  normal ggo*

  file other.txt

  call s:trigger_gitgutter()
  call assert_equal(1, b:gitgutter.enabled)
  call assert_equal('', b:gitgutter.path)
  call s:assert_signs([], 'other.txt')

  write

  call s:trigger_gitgutter()
  call assert_equal(-2, b:gitgutter.path)
endfunction


function Test_saveas()
  normal ggo*

  saveas other.txt

  call s:trigger_gitgutter()
  call assert_equal(1, b:gitgutter.enabled)
  call assert_equal(-2, b:gitgutter.path)
  call s:assert_signs([], 'other.txt')
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


function Test_file_unknown_in_base()
  let starting_branch = system('git branch --show-current')
  let starting_branch = 'main'
  call system('git checkout -b some-feature')
  let tmp = 'file-on-this-branch-only.tmp'
  call system('echo "hi" > '.tmp.' && git add '.tmp)
  execute 'edit '.tmp
  let g:gitgutter_diff_base = starting_branch
  GitGutter
  let expected = [{'lnum': 1, 'name': 'GitGutterLineAdded', 'group': 'gitgutter', 'priority': 10}]
  call s:assert_signs(expected, tmp)
  let g:gitgutter_diff_base = ''
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


function Test_preview()
  normal 5Gi*
  GitGutterPreviewHunk

  wincmd P
  call assert_equal(2, line('$'))
  call assert_equal('-e', getline(1))
  call assert_equal('+*e', getline(2))
  wincmd p
endfunction


function Test_preview_dos()
  edit! fixture_dos.txt

  normal 5Gi*
  GitGutterPreviewHunk

  wincmd P
  call assert_equal(2, line('$'))
  call assert_equal('-e', getline(1))
  call assert_equal('+*e', getline(2))
  wincmd p
endfunction


function Test_dos_noeol()
  edit! fixture_dos_noeol.txt
  GitGutter

  call s:assert_signs([], 'fixture_dos_noeol.txt')
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
  call assert_equal('e', getline(5))
endfunction


function Test_hunk_undo_dos()
  edit! fixture_dos.txt

  normal 5Gi*
  GitGutterUndoHunk

  call s:assert_signs([], 'fixture_dos.txt')
  call assert_equal([], s:git_diff('fixture_dos.txt'))
  call assert_equal([], s:git_diff_staged('fixture_dos.txt'))
  call assert_equal('e', getline(5))
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
  func! Answer(char)
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
  let sid = matchstr(execute('filter autoload/gitgutter/hunk.vim scriptnames'), '\d\+')
  let FixFileReferences = function("<SNR>".sid."_fix_file_references")

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

  call assert_equal(expected, FixFileReferences(filepath, hunk_diff))

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

  call assert_equal(expected, FixFileReferences(filepath, hunk_diff))

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

  call assert_equal(expected, FixFileReferences(filepath, hunk_diff))
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


function Test_quickfix()
  call setline(5, ['A', 'B'])
  call setline(9, ['C', 'D'])
  write
  let bufnr1 = bufnr('')

  edit fixture_dos.txt
  call setline(2, ['A', 'B'])
  write
  let bufnr2 = bufnr('')

  GitGutterQuickFix

  let expected = [
        \ {'lnum': 5, 'bufnr': bufnr1, 'text': '-e'},
        \ {'lnum': 9, 'bufnr': bufnr1, 'text': '-i'},
        \ {'lnum': 2, 'bufnr': bufnr2, 'text': "-b\r"}
        \ ]

  call s:assert_list_of_dicts(expected, getqflist())

  GitGutterQuickFixCurrentFile

  let expected = [
        \ {'lnum': 2, 'bufnr': bufnr(''), 'text': "-b\r"},
        \ ]

  call s:assert_list_of_dicts(expected, getqflist())
endfunction


function Test_common_prefix()
  let sid = matchstr(execute('filter autoload/gitgutter/diff_highlight.vim scriptnames'), '\d\+')
  let CommonPrefix = function("<SNR>".sid."_common_prefix")

  " zero length
  call assert_equal(-1, CommonPrefix('', 'foo'))
  call assert_equal(-1, CommonPrefix('foo', ''))
  " nothing in common
  call assert_equal(-1, CommonPrefix('-abcde', '+pqrst'))
  call assert_equal(-1, CommonPrefix('abcde', 'pqrst'))
  " something in common
  call assert_equal(-1, CommonPrefix('-abcde', '+abcpq'))
  call assert_equal(2, CommonPrefix('abcde', 'abcpq'))
  call assert_equal(0, CommonPrefix('abc', 'apq'))
  " everything in common
  call assert_equal(-1, CommonPrefix('-abcde', '+abcde'))
  call assert_equal(4, CommonPrefix('abcde', 'abcde'))
  " different lengths
  call assert_equal(-1, CommonPrefix('-abcde', '+abx'))
  call assert_equal(1, CommonPrefix('abcde', 'abx'))
  call assert_equal(-1, CommonPrefix('-abx',   '+abcde'))
  call assert_equal(1, CommonPrefix('abx',   'abcde'))
  call assert_equal(-1, CommonPrefix('-abcde', '+abc'))
  call assert_equal(2, CommonPrefix('abcde', 'abc'))
endfunction


function Test_common_suffix()
  let sid = matchstr(execute('filter autoload/gitgutter/diff_highlight.vim scriptnames'), '\d\+')
  let CommonSuffix = function("<SNR>".sid."_common_suffix")

  " nothing in common
  call assert_equal([6,6], CommonSuffix('-abcde', '+pqrst', 0))
  " something in common
  call assert_equal([3,3], CommonSuffix('-abcde', '+pqcde', 0))
  " everything in common
  call assert_equal([5,5], CommonSuffix('-abcde', '+abcde', 5))
  " different lengths
  call assert_equal([4,2], CommonSuffix('-abcde', '+xde', 0))
  call assert_equal([2,4], CommonSuffix('-xde',   '+abcde', 0))
endfunction


" Note the order of lists within the overall returned list does not matter.
function Test_diff_highlight()
  " Ignores mismatched number of added and removed lines.
  call assert_equal([], gitgutter#diff_highlight#process(['-foo']))
  call assert_equal([], gitgutter#diff_highlight#process(['+foo']))
  call assert_equal([], gitgutter#diff_highlight#process(['-foo','-bar','+baz']))

  " everything changed
  let hunk = ['-foo', '+cat']
  let expected = []
  call assert_equal(expected, gitgutter#diff_highlight#process(hunk))

  " change in middle
  let hunk = ['-foo bar baz', '+foo zip baz']
  let expected = [[1, '-', 6, 8], [2, '+', 6, 8]]
  call assert_equal(expected, gitgutter#diff_highlight#process(hunk))

  " change at start
  let hunk = ['-foo bar baz', '+zip bar baz']
  let expected = [[1, '-', 2, 4], [2, '+', 2, 4]]
  call assert_equal(expected, gitgutter#diff_highlight#process(hunk))

  " change at end
  let hunk = ['-foo bar baz', '+foo bar zip']
  let expected = [[1, '-', 10, 12], [2, '+', 10, 12]]
  call assert_equal(expected, gitgutter#diff_highlight#process(hunk))

  " removed in middle
  let hunk = ['-foo bar baz', '+foo baz']
  let expected = [[1, '-', 8, 11]]
  call assert_equal(expected, gitgutter#diff_highlight#process(hunk))

  " added in middle
  let hunk = ['-foo baz', '+foo bar baz']
  let expected = [[2, '+', 8, 11]]
  call assert_equal(expected, gitgutter#diff_highlight#process(hunk))

  " two insertions at start
  let hunk = ['-foo bar baz', '+(foo) bar baz']
  let expected = [[2, '+', 2, 2], [2, '+', 6, 6]]
  call assert_equal(expected, gitgutter#diff_highlight#process(hunk))

  " two insertions in middle
  let hunk = ['-foo bar baz', '+foo (bar) baz']
  let expected = [[2, '+', 6, 6], [2, '+', 10, 10]]
  call assert_equal(expected, gitgutter#diff_highlight#process(hunk))

  " two insertions at end
  let hunk = ['-foo bar baz', '+foo bar (baz)']
  let expected = [[2, '+', 10, 10], [2, '+', 14, 14]]
  call assert_equal(expected, gitgutter#diff_highlight#process(hunk))

  " singular insertion
  let hunk = ['-The cat in the hat.', '+The furry cat in the hat.']
  call assert_equal([[2, '+', 6, 11]], gitgutter#diff_highlight#process(hunk))

  " singular deletion
  let hunk = ['-The cat in the hat.', '+The cat.']
  call assert_equal([[1, '-', 9, 19]], gitgutter#diff_highlight#process(hunk))

  " two insertions
  let hunk = ['-The cat in the hat.', '+The furry cat in the teal hat.']
  call assert_equal([[2, '+', 6, 11], [2, '+', 22, 26]], gitgutter#diff_highlight#process(hunk))

  " two deletions
  let hunk = ['-The furry cat in the teal hat.', '+The cat in the hat.']
  call assert_equal([[1, '-', 6, 11], [1, '-', 22, 26]], gitgutter#diff_highlight#process(hunk))

  " two edits
  let hunk = ['-The cat in the hat.', '+The ox in the box.']
  call assert_equal([[1, '-', 6, 8], [2, '+', 6, 7], [1, '-', 17, 19], [2, '+', 16, 18]], gitgutter#diff_highlight#process(hunk))

  " Requires s:gap_between_regions = 2 to pass.
  " let hunk = ['-foo: bar.zap', '+foo: quux(bar)']
  " call assert_equal([[2, '+', 7, 11], [1, '-', 10, 13], [2, '+', 15, 15]], gitgutter#diff_highlight#process(hunk))

  let hunk = ['-gross_value: transaction.unexplained_amount', '+gross_value: amount(transaction)']
  call assert_equal([[2, '+', 15, 21], [1, '-', 26, 44], [2, '+', 33, 33]], gitgutter#diff_highlight#process(hunk))

  let hunk = ['-gem "contact_sport", "~> 1.0.2"', '+gem ("contact_sport"), "~> 1.2"']
  call assert_equal([[2, '+', 6, 6], [2, '+', 22, 22], [1, '-', 28, 29]], gitgutter#diff_highlight#process(hunk))
endfunction


function Test_lcs()
  let sid = matchstr(execute('filter autoload/gitgutter/diff_highlight.vim scriptnames'), '\d\+')
  let Lcs = function("<SNR>".sid."_lcs")

  call assert_equal('', Lcs('', 'foo'))
  call assert_equal('', Lcs('foo', ''))
  call assert_equal('bar', Lcs('foobarbaz', 'bbart'))
  call assert_equal('transaction', Lcs('transaction.unexplained_amount', 'amount(transaction)'))
endfunction


function Test_split()
  let sid = matchstr(execute('filter autoload/gitgutter/diff_highlight.vim scriptnames'), '\d\+')
  let Split = function("<SNR>".sid."_split")

  call assert_equal(['foo', 'baz'], Split('foobarbaz', 'bar'))
  call assert_equal(['', 'barbaz'], Split('foobarbaz', 'foo'))
  call assert_equal(['foobar', ''], Split('foobarbaz', 'baz'))
  call assert_equal(['1', '2'], Split('1~2', '~'))
endfunction


function Test_foldtext()
  8d
  call s:trigger_gitgutter()
  call assert_equal(0, gitgutter#fold#is_changed())

  let v:foldstart = 5
  let v:foldend = 9
  call assert_equal(1, gitgutter#fold#is_changed())
  call assert_equal('+-  5 lines (*): e', gitgutter#fold#foldtext())

  let v:foldstart = 1
  let v:foldend = 3
  call assert_equal(0, gitgutter#fold#is_changed())
  call assert_equal('+-  3 lines: a', gitgutter#fold#foldtext())
endfunction


function Test_assume_unchanged()
  call system("git update-index --assume-unchanged fixture.txt")
  unlet b:gitgutter.path  " it was already set when fixture.txt was loaded in SetUp()
  normal ggo*
  call s:trigger_gitgutter()
  call s:assert_signs([], 'fixture.txt')
endfunction


function Test_clean_smudge_filter()
  call system("git config --local include.path ../.gitconfig")
  call system("rm fixture.foo && git checkout fixture.foo")

  func! Answer(char)
    call feedkeys(a:char."\<CR>")
  endfunc

  edit fixture.foo
  call setline(2, ['A'])
  call setline(4, ['B'])
  call s:trigger_gitgutter()
  normal! 2G
  call timer_start(100, {-> Answer('y')} )
  GitGutterStageHunk
  call s:trigger_gitgutter()

  let expected = [
        \ {'lnum': 2, 'id': 23, 'name': 'GitGutterLineModified', 'priority': 10, 'group': 'gitgutter'},
        \ {'lnum': 4, 'id': 24, 'name': 'GitGutterLineModified', 'priority': 10, 'group': 'gitgutter'}
        \ ]
  " call s:assert_signs(expected, 'fixture.foo')
  call s:assert_signs([], 'fixture.foo')
endfunction
