function! gitgutter#highlight#line_disable() abort
  let g:gitgutter_highlight_lines = 0
  call s:define_sign_line_highlights()

  if !g:gitgutter_signs
    call gitgutter#sign#clear_signs(bufnr(''))
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


function! gitgutter#highlight#linenr_disable() abort
  let g:gitgutter_highlight_linenrs = 0
  call s:define_sign_linenr_highlights()

  if !g:gitgutter_signs
    call gitgutter#sign#clear_signs(bufnr(''))
  endif

  redraw!
endfunction

function! gitgutter#highlight#linenr_enable() abort
  let old_highlight_linenrs = g:gitgutter_highlight_linenrs

  let g:gitgutter_highlight_linenrs = 1
  call s:define_sign_linenr_highlights()

  if !old_highlight_linenrs && !g:gitgutter_signs
    call gitgutter#all(1)
  endif

  redraw!
endfunction

function! gitgutter#highlight#linenr_toggle() abort
  if g:gitgutter_highlight_linenrs
    call gitgutter#highlight#linenr_disable()
  else
    call gitgutter#highlight#linenr_enable()
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
  for type in ["Add", "Change", "Delete"]
    if hlexists("GitGutter".type) && s:get_foreground_colors("GitGutter".type) != ['NONE', 'NONE']
      if g:gitgutter_set_sign_backgrounds
        execute "highlight GitGutter".type." guibg=".guibg." ctermbg=".ctermbg
      endif
      continue
    elseif s:useful_diff_colours()
      let [guifg, ctermfg] = s:get_foreground_colors('Diff'.type)
    else
      let [guifg, ctermfg] = s:get_foreground_fallback_colors(type)
    endif
    execute "highlight GitGutter".type." guifg=".guifg." guibg=".guibg." ctermfg=".ctermfg." ctermbg=".ctermbg
  endfor

  if hlexists("GitGutterChangeDelete") && g:gitgutter_set_sign_backgrounds
    execute "highlight GitGutterChangeDelete guibg=".guibg." ctermbg=".ctermbg
  endif

  highlight default link GitGutterChangeDelete GitGutterChange

  " Highlights used for the whole line.

  highlight default link GitGutterAddLine          DiffAdd
  highlight default link GitGutterChangeLine       DiffChange
  highlight default link GitGutterDeleteLine       DiffDelete
  highlight default link GitGutterChangeDeleteLine GitGutterChangeLine

  highlight default link GitGutterAddLineNr          CursorLineNr
  highlight default link GitGutterChangeLineNr       CursorLineNr
  highlight default link GitGutterDeleteLineNr       CursorLineNr
  highlight default link GitGutterChangeDeleteLineNr CursorLineNr

  " Highlights used intra line.
  highlight default GitGutterAddIntraLine    gui=reverse cterm=reverse
  highlight default GitGutterDeleteIntraLine gui=reverse cterm=reverse
  " Set diff syntax colours (used in the preview window) - diffAdded,diffChanged,diffRemoved -
  " to match the signs, if not set aleady.
  for [dtype,type] in [['Added','Add'], ['Changed','Change'], ['Removed','Delete']]
    if !hlexists('diff'.dtype)
      let [guifg, ctermfg] = s:get_foreground_colors('GitGutter'.type)
      execute "highlight diff".dtype." guifg=".guifg." ctermfg=".ctermfg." guibg=NONE ctermbg=NONE"
    endif
  endfor
endfunction

function! gitgutter#highlight#define_signs() abort
  sign define GitGutterLineAdded
  sign define GitGutterLineModified
  sign define GitGutterLineRemoved
  sign define GitGutterLineRemovedFirstLine
  sign define GitGutterLineRemovedAboveAndBelow
  sign define GitGutterLineModifiedRemoved

  call s:define_sign_text()
  call gitgutter#highlight#define_sign_text_highlights()
  call s:define_sign_line_highlights()
  call s:define_sign_linenr_highlights()
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

function! s:define_sign_linenr_highlights() abort
  if has('nvim-0.3.2')
    try
      if g:gitgutter_highlight_linenrs
        sign define GitGutterLineAdded                 numhl=GitGutterAddLineNr
        sign define GitGutterLineModified              numhl=GitGutterChangeLineNr
        sign define GitGutterLineRemoved               numhl=GitGutterDeleteLineNr
        sign define GitGutterLineRemovedFirstLine      numhl=GitGutterDeleteLineNr
        sign define GitGutterLineRemovedAboveAndBelow  numhl=GitGutterDeleteLineNr
        sign define GitGutterLineModifiedRemoved       numhl=GitGutterChangeDeleteLineNr
      else
        sign define GitGutterLineAdded                 numhl=
        sign define GitGutterLineModified              numhl=
        sign define GitGutterLineRemoved               numhl=
        sign define GitGutterLineRemovedFirstLine      numhl=
        sign define GitGutterLineRemovedAboveAndBelow  numhl=
        sign define GitGutterLineModifiedRemoved       numhl=
      endif
    catch /E475/
    endtry
  endif
endfunction

function! s:get_hl(group, what, mode) abort
  let r = synIDattr(synIDtrans(hlID(a:group)), a:what, a:mode)
  if empty(r) || r == -1
    return 'NONE'
  endif
  return r
endfunction

function! s:get_foreground_colors(group) abort
  let ctermfg = s:get_hl(a:group, 'fg', 'cterm')
  let guifg = s:get_hl(a:group, 'fg', 'gui')
  return [guifg, ctermfg]
endfunction

function! s:get_background_colors(group) abort
  let ctermbg = s:get_hl(a:group, 'bg', 'cterm')
  let guibg = s:get_hl(a:group, 'bg', 'gui')
  return [guibg, ctermbg]
endfunction

function! s:useful_diff_colours()
  let [guifg_add, ctermfg_add] = s:get_foreground_colors('DiffAdd')
  let [guifg_del, ctermfg_del] = s:get_foreground_colors('DiffDelete')

  return guifg_add != guifg_del && ctermfg_add != ctermfg_del
endfunction

function! s:get_foreground_fallback_colors(type)
  if a:type == 'Add'
    return ['#009900', '2']
  elseif a:type == 'Change'
    return ['#bbbb00', '3']
  elseif a:type == 'Delete'
    return ['#ff2222', '1']
  endif
endfunction
