" Author: Eric Van Dewoestine <ervandew@gmail.com>
"         Original concept and versions up to 0.32 written by
"         Gergely Kontra <kgergely@mcl.hu>
" Version: 2.1
" GetLatestVimScripts: 1643 1 :AutoInstall: supertab.vim
"
" Description: {{{
"   Use your tab key to do all your completion in insert mode!
"   You can cycle forward and backward with the <Tab> and <S-Tab> keys
"   Note: you must press <Tab> once to be able to cycle back
"
"   http://www.vim.org/scripts/script.php?script_id=1643
" }}}
"
" License: {{{
"   Copyright (c) 2002 - 2015
"   All rights reserved.
"
"   Redistribution and use of this software in source and binary forms, with
"   or without modification, are permitted provided that the following
"   conditions are met:
"
"   * Redistributions of source code must retain the above
"     copyright notice, this list of conditions and the
"     following disclaimer.
"
"   * Redistributions in binary form must reproduce the above
"     copyright notice, this list of conditions and the
"     following disclaimer in the documentation and/or other
"     materials provided with the distribution.
"
"   * Neither the name of Gergely Kontra or Eric Van Dewoestine nor the names
"   of its contributors may be used to endorse or promote products derived
"   from this software without specific prior written permission of Gergely
"   Kontra or Eric Van Dewoestine.
"
"   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
"   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
"   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
"   PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
"   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
"   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
"   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
"   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
"   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
"   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
"   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
" }}}
"
" Testing Info: {{{
"   Running vim + supertab with the absolute bare minimum settings:
"     $ vim -u NONE -U NONE -c "set nocp | runtime plugin/supertab.vim"
" }}}

if v:version < 700
  finish
endif

if exists('complType') " Integration with other completion functions.
  finish
endif

if exists("loaded_supertab")
  finish
endif
let loaded_supertab = 1

let s:save_cpo=&cpo
set cpo&vim

" Global Variables {{{

  if !exists("g:SuperTabDefaultCompletionType")
    let g:SuperTabDefaultCompletionType = "<c-p>"
  endif

  if !exists("g:SuperTabContextDefaultCompletionType")
    let g:SuperTabContextDefaultCompletionType = "<c-p>"
  endif

  if !exists("g:SuperTabContextTextMemberPatterns")
    let g:SuperTabContextTextMemberPatterns = ['\.', '>\?::', '->']
  endif

  if !exists("g:SuperTabCompletionContexts")
    let g:SuperTabCompletionContexts = ['s:ContextText']
  endif

  if !exists("g:SuperTabRetainCompletionDuration")
    let g:SuperTabRetainCompletionDuration = 'insert'
  endif

  if !exists("g:SuperTabNoCompleteBefore")
    " retain backwards compatability
    if exists("g:SuperTabMidWordCompletion") && !g:SuperTabMidWordCompletion
      let g:SuperTabNoCompleteBefore = ['\k']
    else
      let g:SuperTabNoCompleteBefore = []
    endif
  endif

  if !exists("g:SuperTabNoCompleteAfter")
    " retain backwards compatability
    if exists("g:SuperTabLeadingSpaceCompletion") && g:SuperTabLeadingSpaceCompletion
      let g:SuperTabNoCompleteAfter = []
    else
      let g:SuperTabNoCompleteAfter = ['^', '\s']
    endif
  endif

  if !exists("g:SuperTabMappingForward")
    let g:SuperTabMappingForward = '<tab>'
  endif
  if !exists("g:SuperTabMappingBackward")
    let g:SuperTabMappingBackward = '<s-tab>'
  endif

  if !exists("g:SuperTabMappingTabLiteral")
    let g:SuperTabMappingTabLiteral = '<c-tab>'
  endif

  if !exists("g:SuperTabLongestEnhanced")
    let g:SuperTabLongestEnhanced = 0
  endif

  if !exists("g:SuperTabLongestHighlight")
    let g:SuperTabLongestHighlight = 0
  endif

  if !exists("g:SuperTabCrMapping")
    let g:SuperTabCrMapping = 0
  endif

  if !exists("g:SuperTabClosePreviewOnPopupClose")
    let g:SuperTabClosePreviewOnPopupClose = 0
  endif

  if !exists("g:SuperTabUndoBreak")
    let g:SuperTabUndoBreak = 0
  endif

  if !exists("g:SuperTabCompleteCase")
    let g:SuperTabCompleteCase = 'inherit'
  endif

" }}}

" Script Variables {{{

  " construct the help text.
  let s:tabHelp =
    \ "Hit <CR> or CTRL-] on the completion type you wish to switch to.\n" .
    \ "Use :help ins-completion for more information.\n" .
    \ "\n" .
    \ "|<c-n>|      - Keywords in 'complete' searching down.\n" .
    \ "|<c-p>|      - Keywords in 'complete' searching up (SuperTab default).\n" .
    \ "|<c-x><c-l>| - Whole lines.\n" .
    \ "|<c-x><c-n>| - Keywords in current file.\n" .
    \ "|<c-x><c-k>| - Keywords in 'dictionary'.\n" .
    \ "|<c-x><c-t>| - Keywords in 'thesaurus', thesaurus-style.\n" .
    \ "|<c-x><c-i>| - Keywords in the current and included files.\n" .
    \ "|<c-x><c-]>| - Tags.\n" .
    \ "|<c-x><c-f>| - File names.\n" .
    \ "|<c-x><c-d>| - Definitions or macros.\n" .
    \ "|<c-x><c-v>| - Vim command-line.\n" .
    \ "|<c-x><c-u>| - User defined completion.\n" .
    \ "|<c-x><c-o>| - Omni completion.\n" .
    \ "|<c-x>s|     - Spelling suggestions."

  " set the available completion types and modes.
  let s:types =
    \ "\<c-e>\<c-y>\<c-l>\<c-n>\<c-k>\<c-t>\<c-i>\<c-]>" .
    \ "\<c-f>\<c-d>\<c-v>\<c-n>\<c-p>\<c-u>\<c-o>\<c-n>\<c-p>s"
  let s:modes = '/^E/^Y/^L/^N/^K/^T/^I/^]/^F/^D/^V/^P/^U/^O/s'
  let s:types = s:types . "np"
  let s:modes = s:modes . '/n/p'

" }}}

function! SuperTabSetDefaultCompletionType(type) " {{{
  " Globally available function that users can use to set the default
  " completion type for the current buffer, like in an ftplugin.

  " don't allow overriding what SuperTabChain has set, otherwise chaining may
  " not work.
  if exists('b:SuperTabChain')
    return
  endif

  " init hack for <c-x><c-v> workaround.
  let b:complCommandLine = 0

  let b:SuperTabDefaultCompletionType = a:type

  " set the current completion type to the default
  call SuperTabSetCompletionType(b:SuperTabDefaultCompletionType)
endfunction " }}}

function! SuperTabSetCompletionType(type) " {{{
  " Globally available function that users can use to create mappings to quickly
  " switch completion modes.  Useful when a user wants to restore the default or
  " switch to another mode without having to kick off a completion of that type
  " or use SuperTabHelp.  Note, this function only changes the current
  " completion type, not the default, meaning that the default will still be
  " restored once the configured retension duration has been met (see
  " g:SuperTabRetainCompletionDuration).  To change the default for the current
  " buffer, use SuperTabDefaultCompletionType(type) instead.  Example mapping to
  " restore SuperTab default:
  "   nmap <F6> :call SetSuperTabCompletionType("<c-p>")<cr>

  " don't allow overriding what SuperTabChain has set, otherwise chaining may
  " not work.
  if exists('b:SuperTabChain')
    return
  endif

  call s:InitBuffer()
  exec "let b:complType = \"" . escape(a:type, '<') . "\""
endfunction " }}}

function! SuperTabAlternateCompletion(type) " {{{
  " Function which can be mapped to a key to kick off an alternate completion
  " other than the default.  For instance, if you have 'context' as the default
  " and want to map ctrl+space to issue keyword completion.
  " Note: due to the way vim expands ctrl characters in mappings, you cannot
  " create the alternate mapping like so:
  "    imap <c-space> <c-r>=SuperTabAlternateCompletion("<c-p>")<cr>
  " instead, you have to use \<lt> to prevent vim from expanding the key
  " when creating the mapping.
  "    gvim:
  "      imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-p>")<cr>
  "    console:
  "      imap <nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-p>")<cr>

  call SuperTabSetCompletionType(a:type)
  " end any current completion before attempting to start the new one.
  " use feedkeys to prevent possible remapping of <c-e> from causing issues.
  "call feedkeys("\<c-e>", 'n')
  " ^ since we can't detect completion mode vs regular insert mode, we force
  " vim into keyword completion mode and end that mode to prevent the regular
  " insert behavior of <c-e> from occurring.
  call feedkeys("\<c-x>\<c-p>\<c-e>", 'n')
  call feedkeys(b:complType, 'n')
  return ''
endfunction " }}}

function! SuperTabLongestHighlight(dir) " {{{
  " When longest highlight is enabled, this function is used to do the actual
  " selection of the completion popup entry.

  if !s:CompletionMode()
    return ''
  endif
  return a:dir == -1 ? "\<up>" : "\<down>"
endfunction " }}}

function! s:Init() " {{{
  " Setup mechanism to restore original completion type upon leaving insert
  " mode if configured to do so
  if g:SuperTabRetainCompletionDuration == 'insert'
    augroup supertab_retain
      autocmd!
      autocmd InsertLeave * call s:SetDefaultCompletionType()
    augroup END
  endif
endfunction " }}}

function! s:InitBuffer() " {{{
  if exists('b:SuperTabNoCompleteBefore')
    return
  endif

  let b:complReset = 0
  let b:complTypeManual = !exists('b:complTypeManual') ? '' : b:complTypeManual
  let b:complTypeContext = ''

  " init hack for <c-x><c-v> workaround.
  let b:complCommandLine = 0

  if !exists('b:SuperTabNoCompleteBefore')
    let b:SuperTabNoCompleteBefore = g:SuperTabNoCompleteBefore
  endif
  if !exists('b:SuperTabNoCompleteAfter')
    let b:SuperTabNoCompleteAfter = g:SuperTabNoCompleteAfter
  endif

  if !exists('b:SuperTabDefaultCompletionType')
    let b:SuperTabDefaultCompletionType = g:SuperTabDefaultCompletionType
  endif

  if !exists('b:SuperTabContextDefaultCompletionType')
    let b:SuperTabContextDefaultCompletionType =
      \ g:SuperTabContextDefaultCompletionType
  endif

  " set the current completion type to the default
  call SuperTabSetCompletionType(b:SuperTabDefaultCompletionType)

  " hack to programatically revert a change to snipmate that breaks supertab
  " but which the new maintainers don't care about:
  " http://github.com/garbas/vim-snipmate/issues/37
  let snipmate = maparg('<tab>', 'i')
  if snipmate =~ '<C-G>u' && g:SuperTabMappingForward =~? '<tab>'
    let snipmate = substitute(snipmate, '<C-G>u', '', '')
    iunmap <tab>
    exec "inoremap <silent> <tab> " . snipmate
  endif
endfunction " }}}

function! s:ManualCompletionEnter() " {{{
  " Handles manual entrance into completion mode.

  if &smd
    echo '' | echohl ModeMsg | echo '-- ^X++ mode (' . s:modes . ')' | echohl None
  endif
  let complType = nr2char(getchar())
  if stridx(s:types, complType) != -1
    if !exists('b:supertab_close_preview')
      let b:supertab_close_preview = !s:IsPreviewOpen()
    endif

    if stridx("\<c-e>\<c-y>", complType) != -1 " no memory, just scroll...
      return "\<c-x>" . complType
    elseif stridx('np', complType) != -1
      let complType = nr2char(char2nr(complType) - 96)
    else
      let complType = "\<c-x>" . complType
    endif

    let b:complTypeManual = complType

    if index(['insert', 'session'], g:SuperTabRetainCompletionDuration) != -1
      let b:complType = complType
    endif

    " Hack to workaround bug when invoking command line completion via <c-r>=
    if complType == "\<c-x>\<c-v>"
      return s:CommandLineCompletion()
    endif

    call s:InitBuffer()

    " optionally enable enhanced longest completion
    if g:SuperTabLongestEnhanced && &completeopt =~ 'longest'
      call s:EnableLongestEnhancement()
    " handle backspacing which triggers g:SuperTabNoCompleteAfter match
    elseif s:IsNoCompleteAfterReset()
      call s:EnableNoCompleteAfterReset()
    endif

    if g:SuperTabLongestHighlight &&
     \ &completeopt =~ 'longest' &&
     \ &completeopt =~ 'menu' &&
     \ !s:CompletionMode()
      let dir = (complType == "\<c-x>\<c-p>") ? -1 : 1
      call feedkeys("\<c-r>=SuperTabLongestHighlight(" . dir . ")\<cr>", 'n')
    endif

    call s:StartCompletionMode()
    return complType
  endif

  echohl "Unknown mode"
  return complType
endfunction " }}}

function! s:SetCompletionType() " {{{
  " Sets the completion type based on what the user has chosen from the help
  " buffer.

  let chosen = substitute(getline('.'), '.*|\(.*\)|.*', '\1', '')
  if chosen != getline('.')
    let winnr = b:winnr
    close
    exec winnr . 'winc w'
    call SuperTabSetCompletionType(chosen)
  endif
endfunction " }}}

function! s:SetDefaultCompletionType() " {{{
  if exists('b:SuperTabDefaultCompletionType') &&
  \ (!exists('b:complCommandLine') || !b:complCommandLine)
    call SuperTabSetCompletionType(b:SuperTabDefaultCompletionType)
  endif
endfunction " }}}

function! SuperTab(command) " {{{
  " Used to perform proper cycle navigation as the user requests the next or
  " previous entry in a completion list, and determines whether or not to simply
  " retain the normal usage of <tab> based on the cursor position.

  if exists('b:SuperTabDisabled') && b:SuperTabDisabled
    if exists('s:Tab')
      return s:Tab()
    endif
    return (
        \ g:SuperTabMappingForward ==? '<tab>' ||
        \ g:SuperTabMappingBackward ==? '<tab>'
      \ ) ? "\<tab>" : ''
  endif

  call s:InitBuffer()

  if s:WillComplete()
    if !exists('b:supertab_close_preview')
      let b:supertab_close_preview = !s:IsPreviewOpen()
    endif

    " optionally enable enhanced longest completion
    if g:SuperTabLongestEnhanced && &completeopt =~ 'longest'
      call s:EnableLongestEnhancement()
    " handle backspacing which triggers g:SuperTabNoCompleteAfter match
    elseif s:IsNoCompleteAfterReset()
      call s:EnableNoCompleteAfterReset()
    endif

    if !s:CompletionMode()
      let b:complTypeManual = ''
    endif

    " exception: if in <c-p> mode, then <c-n> should move up the list, and
    " <c-p> down the list.
    if a:command == 'p' && !b:complReset &&
      \ (b:complType == "\<c-p>" ||
      \   (b:complType == 'context' &&
      \    b:complTypeManual == '' &&
      \    b:complTypeContext == "\<c-p>"))
      return "\<c-n>"

    elseif a:command == 'p' && !b:complReset &&
      \ (b:complType == "\<c-n>" ||
      \   (b:complType == 'context' &&
      \    b:complTypeManual == '' &&
      \    b:complTypeContext == "\<c-n>"))
      return "\<c-p>"

    " already in completion mode and not resetting for longest enhancement, so
    " just scroll to next/previous
    elseif s:CompletionMode() && !b:complReset
      let type = b:complType == 'context' ? b:complTypeContext : b:complType
      if a:command == 'n'
        return type == "\<c-p>" || type == "\<c-x>\<c-p>" ? "\<c-p>" : "\<c-n>"
      endif
      return type == "\<c-p>" || type == "\<c-x>\<c-p>" ? "\<c-n>" : "\<c-p>"
    endif

    " handle 'context' completion.
    if b:complType == 'context'
      let complType = s:ContextCompletion()
      if complType == ''
        exec "let complType = \"" .
          \ escape(b:SuperTabContextDefaultCompletionType, '<') . "\""
      endif
      let b:complTypeContext = complType

    " Hack to workaround bug when invoking command line completion via <c-r>=
    elseif b:complType == "\<c-x>\<c-v>"
      let complType = s:CommandLineCompletion()
    else
      let complType = b:complType
    endif

    " switch <c-x><c-p> / <c-x><c-n> completion in <c-p> mode
    if a:command == 'p'
      if complType == "\<c-x>\<c-p>"
        let complType = "\<c-x>\<c-n>"
      elseif complType == "\<c-x>\<c-n>"
        let complType = "\<c-x>\<c-p>"
      endif
    endif

    " highlight first result if longest enabled
    if g:SuperTabLongestHighlight &&
     \ &completeopt =~ 'longest' &&
     \ &completeopt =~ 'menu' &&
     \ (!s:CompletionMode() || b:complReset)
      let dir = (complType == "\<c-p>") ? -1 : 1
      call feedkeys("\<c-r>=SuperTabLongestHighlight(" . dir . ")\<cr>", 'n')
    endif

    if b:complReset
      let b:complReset = 0
      " not an accurate condition for everyone, but better than sending <c-e>
      " at the wrong time.
      if s:CompletionMode()
        return "\<c-e>" . complType
      endif
    endif

    if g:SuperTabUndoBreak && !s:CompletionMode()
      call s:StartCompletionMode()
      return "\<c-g>u" . complType
    endif

    if g:SuperTabCompleteCase == 'ignore' ||
     \ g:SuperTabCompleteCase == 'match'
      if exists('##CompleteDone')
        let ignorecase = g:SuperTabCompleteCase == 'ignore' ? 1 : 0
        if &ignorecase != ignorecase
          let b:supertab_ignorecase_save = &ignorecase
          let &ignorecase = ignorecase
          augroup supertab_ignorecase
            autocmd CompleteDone <buffer>
              \ let &ignorecase = b:supertab_ignorecase_save |
              \ unlet b:supertab_ignorecase_save |
              \ autocmd! supertab_ignorecase
          augroup END
        endif
      endif
    endif

    call s:StartCompletionMode()
    return complType
  endif

  if (a:command == 'n' && g:SuperTabMappingForward ==? '<tab>') ||
   \ (a:command == 'p' && g:SuperTabMappingBackward ==? '<tab>')

    " trigger our func ref to the smart tabs plugin if present.
    if exists('s:Tab')
      return s:Tab()
    endif

    return "\<tab>"
  endif

  if (a:command == 'n' && g:SuperTabMappingForward ==? '<s-tab>') ||
   \ (a:command == 'p' && g:SuperTabMappingBackward ==? '<s-tab>')
    " support triggering <s-tab> mappings users might have.
    if exists('s:ShiftTab')
      if type(s:ShiftTab) == 2
        return s:ShiftTab()
      else
        call feedkeys(s:ShiftTab, 'n')
      endif
    endif
  endif

  return ''
endfunction " }}}

function! s:SuperTabHelp() " {{{
  " Opens a help window where the user can choose a completion type to enter.

  let winnr = winnr()
  if bufwinnr("SuperTabHelp") == -1
    keepalt botright split SuperTabHelp

    setlocal noswapfile
    setlocal buftype=nowrite
    setlocal bufhidden=wipe

    silent put =s:tabHelp
    call cursor(1, 1)
    silent 1,delete _
    call cursor(4, 1)
    exec "resize " . line('$')

    syntax match Special "|.\{-}|"

    setlocal readonly
    setlocal nomodifiable

    nmap <silent> <buffer> <cr> :call <SID>SetCompletionType()<cr>
    nmap <silent> <buffer> <c-]> :call <SID>SetCompletionType()<cr>
  else
    exec bufwinnr("SuperTabHelp") . "winc w"
  endif
  let b:winnr = winnr
endfunction " }}}

function! s:CompletionMode() " {{{
  return pumvisible() || exists('b:supertab_completion_mode')
endfunction " }}}

function! s:StartCompletionMode() " {{{
  if exists('##CompleteDone')
    let b:supertab_completion_mode = 1
    augroup supertab_completion_mode
      autocmd CompleteDone <buffer>
        \ if exists('b:supertab_completion_mode') |
          \ unlet b:supertab_completion_mode |
        \ endif |
        \ autocmd! supertab_completion_mode
    augroup END
  endif
endfunction " }}}

function! s:WillComplete(...) " {{{
  " Determines if completion should be kicked off at the current location.
  " Optional arg:
  "   col: The column to check at, otherwise use the current column.

  " if an arg was supplied, then we will re-check even if already in
  " completion mode.
  if s:CompletionMode() && !a:0
    return 1
  endif

  let line = getline('.')
  let cnum = a:0 ? a:1 : col('.')

  " honor SuperTabNoCompleteAfter
  let pre = cnum >= 2 ? line[:cnum - 2] : ''
  let complAfterType = type(b:SuperTabNoCompleteAfter)
  if complAfterType == 3
    " the option was provided as a list of patterns
    for pattern in b:SuperTabNoCompleteAfter
      if pre =~ pattern . '$'
        return 0
      endif
    endfor
  elseif complAfterType == 2
    " the option was provided as a funcref
    return !b:SuperTabNoCompleteAfter(pre)
  endif

  " honor SuperTabNoCompleteBefore
  " Within a word, but user does not have mid word completion enabled.
  let post = line[cnum - 1:]
  let complBeforeType = type(b:SuperTabNoCompleteBefore)
  if complBeforeType == 3
    " a list of patterns
    for pattern in b:SuperTabNoCompleteBefore
      if post =~ '^' . pattern
        return 0
      endif
    endfor
  elseif complBeforeType == 2
    " the option was provided as a funcref
    return !b:SuperTabNoCompleteBefore(post)
  endif

  return 1
endfunction " }}}

function! s:EnableLongestEnhancement() " {{{
  augroup supertab_reset
    autocmd!
    autocmd InsertLeave,CursorMovedI <buffer>
      \ call s:ReleaseKeyPresses() | autocmd! supertab_reset
  augroup END
  call s:CaptureKeyPresses()
endfunction " }}}

function! s:IsNoCompleteAfterReset() " {{{
  " if the user has g:SuperTabNoCompleteAfter set, then re-map <bs> so that
  " backspacing to a point where one of the g:SuperTabNoCompleteAfter
  " entries matches will cause completion mode to exit.
  let complAfterType = type(b:SuperTabNoCompleteAfter)
  if complAfterType == 2
    return 1
  endif
  return len(g:SuperTabNoCompleteAfter) && g:SuperTabNoCompleteAfter != ['^', '\s']
endfunction " }}}

function! s:EnableNoCompleteAfterReset() " {{{
  augroup supertab_reset
    autocmd!
    autocmd InsertLeave,CursorMovedI <buffer>
      \ call s:ReleaseKeyPresses() | autocmd! supertab_reset
  augroup END

  " short version of s:CaptureKeyPresses
  if !exists('b:capturing') || !b:capturing
    let b:capturing = 1
    let b:capturing_start = col('.')
    let b:captured = {
        \ '<bs>': s:CaptureKeyMap('<bs>'),
        \ '<c-h>': s:CaptureKeyMap('<c-h>'),
      \ }
    imap <buffer> <bs> <c-r>=<SID>CompletionReset("\<lt>bs>")<cr>
    imap <buffer> <c-h> <c-r>=<SID>CompletionReset("\<lt>c-h>")<cr>
  endif
endfunction " }}}

function! s:CompletionReset(char) " {{{
  let b:complReset = 1

  " handle exiting completion mode if user has g:SuperTabNoCompleteAfter set
  " and they are about to backspace to a point where that maches one of the
  " entries in that var.
  if (a:char == "\<bs>" || a:char == "\<c-h>") && s:IsNoCompleteAfterReset()
    if !s:WillComplete(col('.') - 1)
      " Exit from completion mode then issue the currently requested
      " backspace (mapped).
      call feedkeys("\<space>\<bs>", 'n')
      call s:ReleaseKeyPresses()
      call feedkeys("\<bs>", 'mt')
      return ''
    endif
  endif

  return a:char
endfunction " }}}

function! s:CaptureKeyPresses() " {{{
  if !exists('b:capturing') || !b:capturing
    let b:capturing = 1
    let b:capturing_start = col('.')
    " save any previous mappings
    let b:captured = {
        \ '<bs>': s:CaptureKeyMap('<bs>'),
        \ '<c-h>': s:CaptureKeyMap('<c-h>'),
      \ }
    " TODO: use &keyword to get an accurate list of chars to map
    for c in split('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_', '.\zs')
      let existing = s:CaptureKeyMap(c)
      let b:captured[c] = existing
      exec 'imap <buffer> ' . c . ' <c-r>=<SID>CompletionReset("' . c . '")<cr>'
    endfor
    imap <buffer> <bs> <c-r>=<SID>CompletionReset("\<lt>bs>")<cr>
    imap <buffer> <c-h> <c-r>=<SID>CompletionReset("\<lt>c-h>")<cr>
  endif
endfunction " }}}

function! s:CaptureKeyMap(key) " {{{
  " as of 7.3.032 maparg supports obtaining extended information about the
  " mapping.
  if s:has_dict_maparg
    return maparg(a:key, 'i', 0, 1)
  endif
  return maparg(a:key, 'i')
endfunction " }}}

function! s:IsPreviewOpen() " {{{
  let wins = tabpagewinnr(tabpagenr(), '$')
  let winnr = 1
  while winnr <= wins
    if getwinvar(winnr, '&previewwindow') == 1
      return 1
    endif
    let winnr += 1
  endwhile
  return 0
endfunction " }}}

function! s:ClosePreview() " {{{
  if exists('b:supertab_close_preview') && b:supertab_close_preview
    let preview = 0
    for bufnum in tabpagebuflist()
      if getwinvar(bufwinnr(bufnum), '&previewwindow')
        let preview = 1
        break
      endif
    endfor
    if preview
      pclose
      try
        doautocmd <nomodeline> supertab_preview_closed User <supertab>
      catch /E216/
        " ignore: no autocmds defined
      endtry
    endif
  endif
  silent! unlet b:supertab_close_preview
endfunction " }}}

function! s:ReleaseKeyPresses() " {{{
  if exists('b:capturing') && b:capturing
    let b:capturing = 0
    for c in keys(b:captured)
      exec 'iunmap <buffer> ' . c
    endfor

    " restore any previous mappings
    for [key, mapping] in items(b:captured)
      if !len(mapping)
        continue
      endif

      if type(mapping) == 4
        let restore = mapping.noremap ? "inoremap" : "imap"
        let restore .= " <buffer>"
        if mapping.silent
          let restore .= " <silent>"
        endif
        if mapping.expr
          let restore .= " <expr>"
        endif
        let rhs = substitute(mapping.rhs, '<SID>\c', '<SNR>' . mapping.sid . '_', 'g')
        let restore .= ' ' . key . ' ' . rhs
        exec restore
      elseif type(c) == 1
        let args = substitute(mapping, '.*\(".\{-}"\).*', '\1', '')
        if args != mapping
          let args = substitute(args, '<', '<lt>', 'g')
          let expr = substitute(mapping, '\(.*\)".\{-}"\(.*\)', '\1%s\2', '')
          let mapping = printf(expr, args)
        endif
        exec printf("imap <silent> <buffer> %s %s", key, mapping)
      endif
    endfor
    unlet b:captured

    if mode() == 'i' && &completeopt =~ 'menu' && b:capturing_start != col('.')
      " force full exit from completion mode (don't exit insert mode since
      " that will break repeating with '.')
      call feedkeys("\<space>\<bs>", 'n')
    endif
    unlet b:capturing_start
  endif
endfunction " }}}

function! s:CommandLineCompletion() " {{{
  " Hack needed to account for apparent bug in vim command line mode completion
  " when invoked via <c-r>=

  " This hack will trigger InsertLeave which will then invoke
  " s:SetDefaultCompletionType.  To prevent default completion from being
  " restored prematurely, set an internal flag for s:SetDefaultCompletionType
  " to check for.
  let b:complCommandLine = 1
  return "\<c-\>\<c-o>:call feedkeys('\<c-x>\<c-v>\<c-v>', 'n') | " .
    \ "let b:complCommandLine = 0\<cr>"
endfunction " }}}

function! s:ContextCompletion() " {{{
  let contexts = exists('b:SuperTabCompletionContexts') ?
    \ b:SuperTabCompletionContexts : g:SuperTabCompletionContexts

  for context in contexts
    try
      let Context = function(context)
      let complType = Context()
      unlet Context
      if type(complType) == 1 && complType != ''
        return complType
      endif
    catch /E700/
      echohl Error
      echom 'supertab: no context function "' . context . '" found.'
      echohl None
    endtry
  endfor
  return ''
endfunction " }}}

function! s:ContextDiscover() " {{{
  let discovery = exists('g:SuperTabContextDiscoverDiscovery') ?
    \ g:SuperTabContextDiscoverDiscovery : []

  " loop through discovery list to find the default
  if !empty(discovery)
    for pair in discovery
      let var = substitute(pair, '\(.*\):.*', '\1', '')
      let type = substitute(pair, '.*:\(.*\)', '\1', '')
      exec 'let value = ' . var
      if value !~ '^\s*$' && value != '0'
        exec "let complType = \"" . escape(type, '<') . "\""
        return complType
      endif
    endfor
  endif
endfunction " }}}

function! s:ContextText() " {{{
  let exclusions = exists('g:SuperTabContextTextFileTypeExclusions') ?
    \ g:SuperTabContextTextFileTypeExclusions : []

  if index(exclusions, &ft) == -1
    let curline = getline('.')
    let cnum = col('.')
    let synname = synIDattr(synID(line('.'), cnum - 1, 1), 'name')

    let member_patterns = exists('b:SuperTabContextTextMemberPatterns') ?
      \ b:SuperTabContextTextMemberPatterns : g:SuperTabContextTextMemberPatterns
    let member_pattern = join(member_patterns, '\|')

    " don't kick off file completion if the pattern is '</' (to account for
    " sgml languanges), that's what the following <\@<! pattern is doing.
    if curline =~ '<\@<!/\.\?\w*\%' . cnum . 'c' ||
      \ ((has('win32') || has('win64')) && curline =~ '\\\w*\%' . cnum . 'c')

      return "\<c-x>\<c-f>"

    elseif curline =~ '\(' . member_pattern . '\)\w*\%' . cnum . 'c' &&
      \ synname !~ '\(String\|Comment\)'
      let omniPrecedence = exists('g:SuperTabContextTextOmniPrecedence') ?
        \ g:SuperTabContextTextOmniPrecedence : ['&completefunc', '&omnifunc']
      let omniPrecedence = exists('b:SuperTabContextTextOmniPrecedence') ?
        \ b:SuperTabContextTextOmniPrecedence : omniPrecedence

      for omniFunc in omniPrecedence
        if omniFunc !~ '^&'
          let omniFunc = '&' . omniFunc
        endif
        if getbufvar(bufnr('%'), omniFunc) != ''
          return omniFunc == '&omnifunc' ? "\<c-x>\<c-o>" : "\<c-x>\<c-u>"
        endif
      endfor
    endif
  endif
endfunction " }}}

function! s:ExpandMap(map) " {{{
  let map = a:map
  if map =~ '<Plug>'
    let plug = substitute(map, '.\{-}\(<Plug>\w\+\).*', '\1', '')
    let plug_map = maparg(plug, 'i')
    let map = substitute(map, '.\{-}\(<Plug>\w\+\).*', plug_map, '')
  endif
  return map
endfunction " }}}

function! SuperTabChain(completefunc, completekeys, ...) " {{{
  if a:completefunc != 'SuperTabCodeComplete'
    call s:InitBuffer()
    if (a:0 && a:1) || (!a:0 && b:SuperTabDefaultCompletionType == 'context')
      let b:SuperTabContextTextOmniPrecedence = ['&completefunc', '&omnifunc']
      call SuperTabSetDefaultCompletionType("context")
    else
      call SuperTabSetDefaultCompletionType("<c-x><c-u>")
    endif

    let b:SuperTabChain = [a:completefunc, a:completekeys]
    setlocal completefunc=SuperTabCodeComplete
  endif
endfunction " }}}

function! SuperTabCodeComplete(findstart, base) " {{{
  if !exists('b:SuperTabChain')
    echoe 'No completion chain has been set.'
    return -2
  endif

  if len(b:SuperTabChain) != 2
    echoe 'Completion chain can only be used with 1 completion function ' .
        \ 'and 1 fallback completion key binding.'
    return -2
  endif

  let Func = function(b:SuperTabChain[0])

  if a:findstart
    let start = Func(a:findstart, a:base)
    if start >= 0
      return start
    endif

    return col('.') - 1
  endif

  let results = Func(a:findstart, a:base)
  " Handle dict case, with 'words' and 'refresh' (optional).
  " This is used by YouCompleteMe. (See complete-functions).
  if type(results) == type({}) && has_key(results, 'words')
    if len(results.words)
      return results
    endif
  elseif len(results)
    return results
  endif

  exec 'let keys = "' . escape(b:SuperTabChain[1], '<') . '"'
  " <c-e>: stop completion and go back to the originally typed text.
  call feedkeys("\<c-e>" . keys, 'nt')
  return []
endfunction " }}}

" Autocmds {{{
  if g:SuperTabClosePreviewOnPopupClose
    augroup supertab_close_preview
      autocmd!
      autocmd InsertLeave,CursorMovedI * call s:ClosePreview()
    augroup END
  endif
" }}}

" Key Mappings {{{
  " map a regular tab to ctrl-tab (note: doesn't work in console vim)
  exec 'inoremap ' . g:SuperTabMappingTabLiteral . ' <tab>'

  inoremap <silent> <c-x> <c-r>=<SID>ManualCompletionEnter()<cr>

  imap <script> <Plug>SuperTabForward <c-r>=SuperTab('n')<cr>
  imap <script> <Plug>SuperTabBackward <c-r>=SuperTab('p')<cr>

  let s:has_dict_maparg = v:version > 703 || (v:version == 703 && has('patch32'))

  " support delegating to smart tabs plugin
  if g:SuperTabMappingForward ==? '<tab>' || g:SuperTabMappingBackward ==? '<tab>'
    let existing_tab = maparg('<tab>', 'i')
    if existing_tab =~ '\d\+_InsertSmartTab()$'
      let s:Tab = function(substitute(existing_tab, '()$', '', ''))
    endif
  endif

  " save user's existing <s-tab> mapping if they have one.
  " Note: this could cause more problems than it solves if it picks up <s-tab>
  " mappings from other plugins and misinterprets them, etc, so this block is
  " experimental and could be removed later.
  if g:SuperTabMappingForward ==? '<s-tab>' || g:SuperTabMappingBackward ==? '<s-tab>'
    let stab = maparg('<s-tab>', 'i')
    if s:has_dict_maparg
      let existing_stab = maparg('<s-tab>', 'i', 0, 1)
      if len(existing_stab) && existing_stab.expr
        let stab = substitute(stab, '<SID>\c', '<SNR>' . existing_stab.sid . '_', '')
        let stab = substitute(stab, '()$', '', '')
        let s:ShiftTab = function(stab)
        let stab = ''
      endif
    endif
    if stab != ''
      let stab = substitute(stab, '\(<[-a-zA-Z0-9]\+>\)', '\\\1', 'g')
      exec "let stab = \"" . stab . "\""
      let s:ShiftTab = stab
    endif
  endif

  exec 'imap ' . g:SuperTabMappingForward . ' <Plug>SuperTabForward'
  exec 'imap ' . g:SuperTabMappingBackward . ' <Plug>SuperTabBackward'

  if g:SuperTabCrMapping
    let expr_map = 0
    if s:has_dict_maparg
      let map_dict = maparg('<cr>', 'i', 0, 1)
      let expr_map = has_key(map_dict, 'expr') && map_dict.expr
    else
      let expr_map = maparg('<cr>', 'i') =~? '\<cr>'
    endif

    redir => iabbrevs
    silent iabbrev
    redir END
    let iabbrev_map = iabbrevs =~? '\<cr>'

    if expr_map
      " Not compatible w/ expr mappings. This is most likely a user mapping,
      " typically with the same functionality anyways.
      let g:SuperTabCrMapping = 0
    elseif iabbrev_map
      " Not compatible w/ insert abbreviations containing <cr>
      let g:SuperTabCrMapping = 0
    elseif maparg('<CR>', 'i') =~ '<Plug>delimitMateCR'
      " Not compatible w/ delimitMate since it doesn't play well with others
      " and will always return a <cr> which we don't want when selecting a
      " completion.
      let g:SuperTabCrMapping = 0
    elseif maparg('<CR>', 'i') =~ '<CR>'
      let map = maparg('<cr>', 'i')
      let cr = !(map =~? '\(^\|[^)]\)<cr>' || map =~ 'ExpandCr')
      let map = s:ExpandMap(map)
      exec "inoremap <script> <cr> <c-r>=<SID>SelectCompletion(" . cr . ")<cr>" . map
    else
      inoremap <silent> <cr> <c-r>=<SID>SelectCompletion(1)<cr>
    endif
    function! s:SelectCompletion(cr)
      " selecting a completion
      if s:CompletionMode()
        " ugly hack to let other <cr> mappings for other plugins cooperate
        " with supertab
        let b:supertab_pumwasvisible = 1

        " close the preview window if configured to do so
        if &completeopt =~ 'preview' && g:SuperTabClosePreviewOnPopupClose
          if !exists('b:supertab_close_preview')
            let b:supertab_close_preview = !s:IsPreviewOpen()
          endif
          call s:ClosePreview()
        endif

        return "\<c-y>"
      endif

      " only needed when chained with other mappings and one of them will
      " issue a <cr>.
      if exists('b:supertab_pumwasvisible') && !a:cr
        unlet b:supertab_pumwasvisible
        return ''
      endif

      " not so pleasant hack to keep <cr> working for abbreviations
      let word = substitute(getline('.'), '^.*\s\+\(.*\%' . col('.') . 'c\).*', '\1', '')
      let result = maparg(word, 'i', 1)
      if result != ''
        let bs = ""
        let i = 0
        while i < len(word)
          let bs .= "\<bs>"
          let i += 1
        endwhile
        " escape keys
        let result = substitute(result, '\(<[a-zA-Z][-a-zA-Z]*>\)', '\\\1', 'g')
        " ensure escaped keys are properly recognized
        exec 'let result = "' . escape(result, '"') . '"'
        return bs . result . (a:cr ? "\<cr>" : "")
      endif

      " only return a cr if nothing else is mapped to it since we don't want
      " to duplicate a cr returned by another mapping.
      return a:cr ? "\<cr>" : ""
    endfunction
  endif
" }}}

" Command Mappings {{{
  if !exists(":SuperTabHelp")
    command SuperTabHelp :call <SID>SuperTabHelp()
  endif
" }}}

call s:Init()

function! TestSuperTabCodeComplete(findstart, base) " {{{
  " Test supertab completion chaining w/ a minimal vim environment:
  " $ vim -u NONE -U NONE \
  "   --cmd "set nocp | sy on" \
  "   -c "so ~/.vim/plugin/supertab.vim" \
  "   -c "let g:SuperTabDefaultCompletionType = '<c-x><c-u>'" \
  "   -c "set completefunc=TestSuperTabCodeComplete" \
  "   -c "call SuperTabChain(&completefunc, '<c-p>')"
  if a:findstart
    let line = getline('.')
    let start = col('.') - 1
    if line[start] =~ '\.'
      let start -= 1
    endif
    while start > 0 && line[start - 1] =~ '\w'
      let start -= 1
    endwhile
    return start
  else
    let completions = []
    if getline('.') =~ 'TestC'
      call add(completions, {
          \ 'word': 'test1(',
          \ 'kind': 'm',
          \ 'menu': 'test1(...)',
        \ })
      call add(completions, {
          \ 'word': 'testing2(',
          \ 'kind': 'm',
          \ 'menu': 'testing2(...)',
        \ })
    endif

    return completions
  endif
endfunction " }}}

let &cpo = s:save_cpo

" vim:ft=vim:fdm=marker
