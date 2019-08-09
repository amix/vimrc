function! gitgutter#highlight#line_disable() abort
  let g:gitgutter_highlight_lines = 0
  call s:define_sign_line_highlights()

  if !g:gitgutter_signs
    call gitgutter#sign#clear_signs(bufnr(''))
    call gitgutter#sign#remove_dummy_sign(bufnr(''), 0)
  endif

  redraw!
endfunction

function! gitgutter#highlight#line_enable() abort
  let old_highlight_lines = g:gitgutter_highlight_lines

  let g:gitgutter_highlight_lines = 1
  call s:define_sign_line_highlights()

  if !old_highlight_lines && !g:gitgutter_signs
    call gitgutter#all(1)
  endif

  redraw!
endfunction

function! gitgutter#highlight#line_toggle() abort
  if g:gitgutter_highlight_lines
    call gitgutter#highlight#line_disable()
  else
    call gitgutter#highlight#line_enable()
  endif
endfunction


function! gitgutter#highlight#define_sign_column_highlight() abort
  if g:gitgutter_override_sign_column_highlight
    highlight! link SignColumn LineNr
  else
    highlight default link SignColumn LineNr
  endif
endfunction

function! gitgutter#highlight#define_highlights() abort
  let [guibg, ctermbg] = s:get_background_colors('SignColumn')

  " Highlights used by the signs.

  " When they are invisible.
  execute "highlight GitGutterAddInvisible    guifg=bg guibg=" . guibg . " ctermfg=" . ctermbg . " ctermbg=" . ctermbg
  execute "highlight GitGutterChangeInvisible guifg=bg guibg=" . guibg . " ctermfg=" . ctermbg . " ctermbg=" . ctermbg
  execute "highlight GitGutterDeleteInvisible guifg=bg guibg=" . guibg . " ctermfg=" . ctermbg . " ctermbg=" . ctermbg
  highlight default link GitGutterChangeDeleteInvisible GitGutterChangeInvisible

  " When they are visible.
  " By default use Diff* foreground colors with SignColumn's background.
  for type in ['Add', 'Change', 'Delete']
    let [guifg, ctermfg] = s:get_foreground_colors('Diff'.type)
    execute "highlight GitGutter".type."Default guifg=".guifg." guibg=".guibg." ctermfg=".ctermfg." ctermbg=".ctermbg
    execute "highlight default link GitGutter".type." GitGutter".type."Default"
  endfor
  highlight default link GitGutterChangeDelete GitGutterChange

  " Highlights used for the whole line.

  highlight default link GitGutterAddLine          DiffAdd
  highlight default link GitGutterChangeLine       DiffChange
  highlight default link GitGutterDeleteLine       DiffDelete
  highlight default link GitGutterChangeDeleteLine GitGutterChangeLine
endfunction

function! gitgutter#highlight#define_signs() abort
  sign define GitGutterLineAdded
  sign define GitGutterLineModified
  sign define GitGutterLineRemoved
  sign define GitGutterLineRemovedFirstLine
  sign define GitGutterLineRemovedAboveAndBelow
  sign define GitGutterLineModifiedRemoved
  sign define GitGutterDummy

  call s:define_sign_text()
  call gitgutter#highlight#define_sign_text_highlights()
  call s:define_sign_line_highlights()
endfunction

function! s:define_sign_text() abort
  execute "sign define GitGutterLineAdded                 text=" . g:gitgutter_sign_added
  execute "sign define GitGutterLineModified              text=" . g:gitgutter_sign_modified
  execute "sign define GitGutterLineRemoved               text=" . g:gitgutter_sign_removed
  execute "sign define GitGutterLineRemovedFirstLine      text=" . g:gitgutter_sign_removed_first_line
  execute "sign define GitGutterLineRemovedAboveAndBelow  text=" . g:gitgutter_sign_removed_above_and_below
  execute "sign define GitGutterLineModifiedRemoved       text=" . g:gitgutter_sign_modified_removed
endfunction

function! gitgutter#highlight#define_sign_text_highlights() abort
  " Once a sign's text attribute has been defined, it cannot be undefined or
  " set to an empty value.  So to make signs' text disappear (when toggling
  " off or disabling) we make them invisible by setting their foreground colours
  " to the background's.
  if g:gitgutter_signs
    sign define GitGutterLineAdded                 texthl=GitGutterAdd
    sign define GitGutterLineModified              texthl=GitGutterChange
    sign define GitGutterLineRemoved               texthl=GitGutterDelete
    sign define GitGutterLineRemovedFirstLine      texthl=GitGutterDelete
    sign define GitGutterLineRemovedAboveAndBelow  texthl=GitGutterDelete
    sign define GitGutterLineModifiedRemoved       texthl=GitGutterChangeDelete
  else
    sign define GitGutterLineAdded                 texthl=GitGutterAddInvisible
    sign define GitGutterLineModified              texthl=GitGutterChangeInvisible
    sign define GitGutterLineRemoved               texthl=GitGutterDeleteInvisible
    sign define GitGutterLineRemovedFirstLine      texthl=GitGutterDeleteInvisible
    sign define GitGutterLineRemovedAboveAndBelow  texthl=GitGutterDeleteInvisible
    sign define GitGutterLineModifiedRemoved       texthl=GitGutterChangeDeleteInvisible
  endif
endfunction

function! s:define_sign_line_highlights() abort
  if g:gitgutter_highlight_lines
    sign define GitGutterLineAdded                 linehl=GitGutterAddLine
    sign define GitGutterLineModified              linehl=GitGutterChangeLine
    sign define GitGutterLineRemoved               linehl=GitGutterDeleteLine
    sign define GitGutterLineRemovedFirstLine      linehl=GitGutterDeleteLine
    sign define GitGutterLineRemovedAboveAndBelow  linehl=GitGutterDeleteLine
    sign define GitGutterLineModifiedRemoved       linehl=GitGutterChangeDeleteLine
  else
    sign define GitGutterLineAdded                 linehl=
    sign define GitGutterLineModified              linehl=
    sign define GitGutterLineRemoved               linehl=
    sign define GitGutterLineRemovedFirstLine      linehl=
    sign define GitGutterLineRemovedAboveAndBelow  linehl=
    sign define GitGutterLineModifiedRemoved       linehl=
  endif
endfunction

function! s:get_foreground_colors(group) abort
  redir => highlight
  silent execute 'silent highlight ' . a:group
  redir END

  let link_matches = matchlist(highlight, 'links to \(\S\+\)')
  if len(link_matches) > 0 " follow the link
    return s:get_foreground_colors(link_matches[1])
  endif

  let ctermfg = s:match_highlight(highlight, 'ctermfg=\([0-9A-Za-z]\+\)')
  let guifg   = s:match_highlight(highlight, 'guifg=\([#0-9A-Za-z]\+\)')
  return [guifg, ctermfg]
endfunction

function! s:get_background_colors(group) abort
  redir => highlight
  silent execute 'silent highlight ' . a:group
  redir END

  let link_matches = matchlist(highlight, 'links to \(\S\+\)')
  if len(link_matches) > 0 " follow the link
    return s:get_background_colors(link_matches[1])
  endif

  let ctermbg = s:match_highlight(highlight, 'ctermbg=\([0-9A-Za-z]\+\)')
  let guibg   = s:match_highlight(highlight, 'guibg=\([#0-9A-Za-z]\+\)')
  return [guibg, ctermbg]
endfunction

function! s:match_highlight(highlight, pattern) abort
  let matches = matchlist(a:highlight, a:pattern)
  if len(matches) == 0
    return 'NONE'
  endif
  return matches[1]
endfunction
