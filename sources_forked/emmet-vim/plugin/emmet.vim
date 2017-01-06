"=============================================================================
" File: emmet.vim
" Author: Yasuhiro Matsumoto <mattn.jp@gmail.com>
" Last Change: 26-Jul-2015.
" Version: 0.86
" WebPage: http://github.com/mattn/emmet-vim
" Description: vim plugins for HTML and CSS hi-speed coding.
" SeeAlso: http://emmet.io/
" Usage:
"
"   This is vim script support expanding abbreviation like emmet.
"   ref: http://emmet.io/
"
"   Type abbreviation
"      +-------------------------------------
"      | html:5_
"      +-------------------------------------
"   "_" is a cursor position. and type "<c-y>," (Ctrl+y and Comma)
"   NOTE: Don't worry about key map. you can change it easily.
"      +-------------------------------------
"      | <!DOCTYPE HTML>
"      | <html lang="en">
"      | <head>
"      |     <title></title>
"      |     <meta charset="UTF-8">
"      | </head>
"      | <body>
"      |      _
"      | </body>
"      | </html>
"      +-------------------------------------
"   Type following
"      +-------------------------------------
"      | div#foo$*2>div.bar
"      +-------------------------------------
"   And type "<c-y>,"
"      +-------------------------------------
"      |<div id="foo1">
"      |    <div class="bar">_</div>
"      |</div>
"      |<div id="foo2">
"      |    <div class="bar"></div>
"      |</div>
"      +-------------------------------------
"
" Tips:
"
"   You can customize behavior of expanding with overriding config.
"   This configuration will be marged at loading plugin.
"
"     let g:user_emmet_settings = {
"     \  'indentation' : '  ',
"     \  'perl' : {
"     \    'aliases' : {
"     \      'req' : 'require '
"     \    },
"     \    'snippets' : {
"     \      'use' : "use strict\nuse warnings\n\n",
"     \      'warn' : "warn \"|\";",
"     \    }
"     \  }
"     \}
"
"   You can set language attribute in html using 'emmet_settings.lang'.
"
" GetLatestVimScripts: 2981 1 :AutoInstall: emmet.vim
" script type: plugin

if &compatible || v:version < 702 || (exists('g:loaded_emmet_vim') && g:loaded_emmet_vim)
  finish
endif
let g:loaded_emmet_vim = 1

let s:save_cpo = &cpoptions
set cpoptions&vim

if !exists('g:emmet_html5')
  let g:emmet_html5 = 1
endif

if !exists('g:emmet_docroot')
  let g:emmet_docroot = {}
endif

if !exists('g:emmet_debug')
  let g:emmet_debug = 0
endif

if !exists('g:emmet_curl_command')
  let g:emmet_curl_command = 'curl -s -L -A Mozilla/5.0'
endif

if !exists('g:user_emmet_leader_key')
  let g:user_emmet_leader_key = '<c-y>'
endif

function! s:install_plugin(mode, buffer)
  let buffer = a:buffer ? '<buffer>' : ''
  let items = [
  \ {'mode': 'i', 'var': 'user_emmet_expandabbr_key', 'key': ',', 'plug': 'emmet-expand-abbr', 'func': '<c-r>=emmet#util#closePopup()<cr><c-r>=emmet#expandAbbr(0,"")<cr>'},
  \ {'mode': 'n', 'var': 'user_emmet_expandabbr_key', 'key': ',', 'plug': 'emmet-expand-abbr', 'func': ':call emmet#expandAbbr(3,"")<cr>'},
  \ {'mode': 'v', 'var': 'user_emmet_expandabbr_key', 'key': ',', 'plug': 'emmet-expand-abbr', 'func': ':call emmet#expandAbbr(2,"")<cr>'},
  \ {'mode': 'i', 'var': 'user_emmet_expandword_key', 'key': ';', 'plug': 'emmet-expand-word', 'func': '<c-r>=emmet#util#closePopup()<cr><c-r>=emmet#expandAbbr(1,"")<cr>'},
  \ {'mode': 'n', 'var': 'user_emmet_expandword_key', 'key': ';', 'plug': 'emmet-expand-word', 'func': ':call emmet#expandAbbr(1,"")<cr>'},
  \ {'mode': 'i', 'var': 'user_emmet_update_tag', 'key': 'u', 'plug': 'emmet-update-tag', 'func': '<c-r>=emmet#util#closePopup()<cr><c-r>=emmet#updateTag()<cr>'},
  \ {'mode': 'n', 'var': 'user_emmet_update_tag', 'key': 'u', 'plug': 'emmet-update-tag', 'func': ':call emmet#updateTag()<cr>'},
  \ {'mode': 'i', 'var': 'user_emmet_balancetaginward_key', 'key': 'd', 'plug': 'emmet-balance-tag-inward', 'func': '<esc>:call emmet#balanceTag(1)<cr>'},
  \ {'mode': 'n', 'var': 'user_emmet_balancetaginward_key', 'key': 'd', 'plug': 'emmet-balance-tag-inward', 'func': ':call emmet#balanceTag(1)<cr>'},
  \ {'mode': 'v', 'var': 'user_emmet_balancetaginward_key', 'key': 'd', 'plug': 'emmet-balance-tag-inward', 'func': ':call emmet#balanceTag(2)<cr>'},
  \ {'mode': 'i', 'var': 'user_emmet_balancetagoutward_key', 'key': 'D', 'plug': 'emmet-balance-tag-outword', 'func': '<esc>:call emmet#balanceTag(-1)<cr>'},
  \ {'mode': 'n', 'var': 'user_emmet_balancetagoutward_key', 'key': 'D', 'plug': 'emmet-balance-tag-outword', 'func': ':call emmet#balanceTag(-1)<cr>'},
  \ {'mode': 'v', 'var': 'user_emmet_balancetagoutward_key', 'key': 'D', 'plug': 'emmet-balance-tag-outword', 'func': ':call emmet#balanceTag(-2)<cr>'},
  \ {'mode': 'i', 'var': 'user_emmet_next_key', 'key': 'n', 'plug': 'emmet-move-next', 'func': '<esc>:call emmet#moveNextPrev(0)<cr>'},
  \ {'mode': 'n', 'var': 'user_emmet_next_key', 'key': 'n', 'plug': 'emmet-move-next', 'func': ':call emmet#moveNextPrev(0)<cr>'},
  \ {'mode': 'i', 'var': 'user_emmet_prev_key', 'key': 'N', 'plug': 'emmet-move-prev', 'func': '<esc>:call emmet#moveNextPrev(1)<cr>'},
  \ {'mode': 'n', 'var': 'user_emmet_prev_key', 'key': 'N', 'plug': 'emmet-move-prev', 'func': ':call emmet#moveNextPrev(1)<cr>'},
  \ {'mode': 'i', 'var': '', 'key': '', 'plug': 'emmet-move-next-item', 'func': '<esc>:call emmet#moveNextPrevItem(0)<cr>'},
  \ {'mode': 'n', 'var': '', 'key': '', 'plug': 'emmet-move-next-item', 'func': ':call emmet#moveNextPrevItem(0)<cr>'},
  \ {'mode': 'i', 'var': '', 'key': '', 'plug': 'emmet-move-prev-item', 'func': '<esc>:call emmet#moveNextPrevItem(1)<cr>'},
  \ {'mode': 'n', 'var': '', 'key': '', 'plug': 'emmet-move-prev-item', 'func': ':call emmet#moveNextPrevItem(1)<cr>'},
  \ {'mode': 'i', 'var': 'user_emmet_imagesize_key', 'key': 'i', 'plug': 'emmet-image-size', 'func': '<c-r>=emmet#util#closePopup()<cr><c-r>=emmet#imageSize()<cr>'},
  \ {'mode': 'n', 'var': 'user_emmet_imagesize_key', 'key': 'i', 'plug': 'emmet-image-size', 'func': ':call emmet#imageSize()<cr>'},
  \ {'mode': 'i', 'var': 'user_emmet_togglecomment_key', 'key': '/', 'plug': 'emmet-toggle-comment', 'func': '<c-r>=emmet#util#closePopup()<cr><c-r>=emmet#toggleComment()<cr>'},
  \ {'mode': 'n', 'var': 'user_emmet_togglecomment_key', 'key': '/', 'plug': 'emmet-toggle-comment', 'func': ':call emmet#toggleComment()<cr>'},
  \ {'mode': 'i', 'var': 'user_emmet_splitjointag_key', 'key': 'j', 'plug': 'emmet-split-join-tag', 'func': '<esc>:call emmet#splitJoinTag()<cr>'},
  \ {'mode': 'n', 'var': 'user_emmet_splitjointag_key', 'key': 'j', 'plug': 'emmet-split-join-tag', 'func': ':call emmet#splitJoinTag()<cr>'},
  \ {'mode': 'i', 'var': 'user_emmet_removetag_key', 'key': 'k', 'plug': 'emmet-remove-tag', 'func': '<c-r>=emmet#util#closePopup()<cr><c-r>=emmet#removeTag()<cr>'},
  \ {'mode': 'n', 'var': 'user_emmet_removetag_key', 'key': 'k', 'plug': 'emmet-remove-tag', 'func': ':call emmet#removeTag()<cr>'},
  \ {'mode': 'i', 'var': 'user_emmet_anchorizeurl_key', 'key': 'a', 'plug': 'emmet-anchorize-url', 'func': '<c-r>=emmet#util#closePopup()<cr><c-r>=emmet#anchorizeURL(0)<cr>'},
  \ {'mode': 'n', 'var': 'user_emmet_anchorizeurl_key', 'key': 'a', 'plug': 'emmet-anchorize-url', 'func': ':call emmet#anchorizeURL(0)<cr>'},
  \ {'mode': 'i', 'var': 'user_emmet_anchorizesummary_key', 'key': 'A', 'plug': 'emmet-anchorize-summary', 'func': '<c-r>=emmet#util#closePopup()<cr><c-r>=emmet#anchorizeURL(1)<cr>'},
  \ {'mode': 'n', 'var': 'user_emmet_anchorizesummary_key', 'key': 'A', 'plug': 'emmet-anchorize-summary', 'func': ':call emmet#anchorizeURL(1)<cr>'},
  \ {'mode': 'v', 'var': 'user_emmet_mergelines_key', 'key': 'm', 'plug': 'emmet-merge-lines', 'func': ':call emmet#mergeLines()<cr>'},
  \ {'mode': 'v', 'var': 'user_emmet_codepretty_key', 'key': 'c', 'plug': 'emmet-code-pretty', 'func': ':call emmet#codePretty()<cr>'},
  \]

  let only_plug = get(g:, 'emmet_install_only_plug', 0)
  for item in items
    if a:mode !=# 'a' && stridx(a:mode, item.mode) == -1
      continue
    endif
    exe item.mode . 'noremap '. buffer .' <plug>(' . item.plug . ') ' . item.func
    if item.var != '' && !only_plug
      if exists('g:' . item.var)
        let key = eval('g:' . item.var)
      else
        let key = g:user_emmet_leader_key . item.key
      endif
      if !hasmapto('<plug>(' . item.plug . ')', item.mode) && !len(maparg(key, item.mode))
        exe item.mode . 'map ' . buffer . ' <unique> ' . key . ' <plug>(' . item.plug . ')'
      endif
    endif
  endfor

  if exists('g:user_emmet_complete_tag') && g:user_emmet_complete_tag
    if get(g:, 'user_emmet_install_global', 1)
      set omnifunc=emmet#completeTag
    else
      setlocal omnifunc=emmet#completeTag
    endif
  endif
endfunction

command! -nargs=0 -bar EmmetInstall call <SID>install_plugin(get(g:, 'user_emmet_mode', 'a'), 1)

if get(g:, 'user_emmet_install_global', 1)
  call s:install_plugin(get(g:, 'user_emmet_mode', 'a'), 0)
endif

if get(g:, 'user_emmet_install_command', 1)
  command! -nargs=1 Emmet call emmet#expandAbbr(4, <q-args>)
endif

let &cpoptions = s:save_cpo
unlet s:save_cpo

" vim:set et:
