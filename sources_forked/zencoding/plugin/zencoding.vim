"=============================================================================
" File: zencoding.vim
" Author: Yasuhiro Matsumoto <mattn.jp@gmail.com>
" Last Change: 13-Feb-2013.
" Version: 0.75
" WebPage: http://github.com/mattn/zencoding-vim
" Description: vim plugins for HTML and CSS hi-speed coding.
" SeeAlso: http://code.google.com/p/zen-coding/
" Usage:
"
"   This is vim script support expanding abbreviation like zen-coding.
"   ref: http://code.google.com/p/zen-coding/
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
"     let g:user_zen_settings = {
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
"   You can set language attribute in html using 'zen_settings.lang'.
"
" GetLatestVimScripts: 2981 1 :AutoInstall: zencoding.vim
" script type: plugin

if &cp || (exists('g:loaded_zencoding_vim') && g:loaded_zencoding_vim)
  finish
endif
let g:loaded_zencoding_vim = 1

let s:save_cpo = &cpo
set cpo&vim

if !exists('g:zencoding_debug')
  let g:zencoding_debug = 0
endif

if !exists('g:zencoding_curl_command')
  let g:zencoding_curl_command = 'curl -s -L -A Mozilla/5.0'
endif

if exists('g:use_zen_complete_tag') && g:use_zen_complete_tag
  setlocal omnifunc=zencoding#CompleteTag
endif

if !exists('g:user_zen_leader_key')
  let g:user_zen_leader_key = '<c-y>'
endif

function! s:install_plugin_i()
  for item in [
  \ {'mode': 'i', 'var': 'user_zen_expandabbr_key', 'key': ',', 'plug': 'ZenCodingExpandAbbr', 'func': '<c-g>u<esc>:call zencoding#expandAbbr(0,"")<cr>a'},
  \ {'mode': 'i', 'var': 'user_zen_expandword_key', 'key': ';', 'plug': 'ZenCodingExpandWord', 'func': '<c-g>u<esc>:call zencoding#expandAbbr(1,"")<cr>a'},
  \ {'mode': 'i', 'var': 'user_zen_balancetaginward_key', 'key': 'd', 'plug': 'ZenCodingBalanceTagInwardInsert', 'func': '<esc>:call zencoding#balanceTag(1)<cr>'},
  \ {'mode': 'i', 'var': 'user_zen_balancetagoutward_key', 'key': 'D', 'plug': 'ZenCodingBalanceTagOutwardInsert', 'func': '<esc>:call zencoding#balanceTag(-1)<cr>'},
  \ {'mode': 'i', 'var': 'user_zen_next_key', 'key': 'n', 'plug': 'ZenCodingNext', 'func': '<esc>:call zencoding#moveNextPrev(0)<cr>'},
  \ {'mode': 'i', 'var': 'user_zen_prev_key', 'key': 'N', 'plug': 'ZenCodingPrev', 'func': '<esc>:call zencoding#moveNextPrev(1)<cr>'},
  \ {'mode': 'i', 'var': 'user_zen_imagesize_key', 'key': 'i', 'plug': 'ZenCodingImageSize', 'func': '<esc>:call zencoding#imageSize()<cr>a'},
  \ {'mode': 'i', 'var': 'user_zen_togglecomment_key', 'key': '/', 'plug': 'ZenCodingToggleComment', 'func': '<esc>:call zencoding#toggleComment()<cr>a'},
  \ {'mode': 'i', 'var': 'user_zen_splitjointag_key', 'key': 'j', 'plug': 'ZenCodingSplitJoinTagInsert', 'func': '<esc>:call zencoding#splitJoinTag()<cr>'},
  \ {'mode': 'i', 'var': 'user_zen_removetag_key', 'key': 'k', 'plug': 'ZenCodingRemoveTag', 'func': '<esc>:call zencoding#removeTag()<cr>a'},
  \ {'mode': 'i', 'var': 'user_zen_anchorizeurl_key', 'key': 'a', 'plug': 'ZenCodingAnchorizeURL', 'func': '<esc>:call zencoding#anchorizeURL(0)<cr>a'},
  \ {'mode': 'i', 'var': 'user_zen_anchorizesummary_key', 'key': 'A', 'plug': 'ZenCodingAnchorizeSummary', 'func': '<esc>:call zencoding#anchorizeURL(1)<cr>a'},
  \]

    if !hasmapto('<plug>'.item.plug, item.mode)
      exe item.mode . 'noremap <plug>' . item.plug . ' ' . item.func
    endif
    if !exists('g:' . item.var)
    endif
    if exists('g:' . item.var)
      let key = eval('g:' . item.var)
    else
      let key = g:user_zen_leader_key . item.key
    endif
    if len(maparg(key, item.mode)) == 0
      exe item.mode . 'map <unique> ' . key . ' <plug>' . item.plug
    endif
  endfor
endfunction

function! s:install_plugin_n()
  for item in [
  \ {'mode': 'n', 'var': 'user_zen_expandabbr_key', 'key': ',', 'plug': 'ZenCodingExpandNormal', 'func': ':call zencoding#expandAbbr(3,"")<cr>'},
  \ {'mode': 'n', 'var': 'user_zen_expandword_key', 'key': ',', 'plug': 'ZenCodingExpandWord', 'func': ':call zencoding#expandAbbr(1,"")<cr>'},
  \ {'mode': 'n', 'var': 'user_zen_balancetaginward_key', 'key': 'd', 'plug': 'ZenCodingBalanceTagInwardNormal', 'func': ':call zencoding#balanceTag(1)<cr>'},
  \ {'mode': 'n', 'var': 'user_zen_balancetagoutward_key', 'key': 'D', 'plug': 'ZenCodingBalanceTagOutwardNormal', 'func': ':call zencoding#balanceTag(-1)<cr>'},
  \ {'mode': 'n', 'var': 'user_zen_next_key', 'key': 'n', 'plug': 'ZenCodingNext', 'func': ':call zencoding#moveNextPrev(0)<cr>'},
  \ {'mode': 'n', 'var': 'user_zen_prev_key', 'key': 'N', 'plug': 'ZenCodingPrev', 'func': ':call zencoding#moveNextPrev(1)<cr>'},
  \ {'mode': 'n', 'var': 'user_zen_imagesize_key', 'key': 'i', 'plug': 'ZenCodingImageSize', 'func': ':call zencoding#imageSize()<cr>'},
  \ {'mode': 'n', 'var': 'user_zen_togglecomment_key', 'key': '/', 'plug': 'ZenCodingToggleComment', 'func': ':call zencoding#toggleComment()<cr>'},
  \ {'mode': 'n', 'var': 'user_zen_splitjointag_key', 'key': 'j', 'plug': 'ZenCodingSplitJoinTagNormal', 'func': ':call zencoding#splitJoinTag()<cr>'},
  \ {'mode': 'n', 'var': 'user_zen_removetag_key', 'key': 'k', 'plug': 'ZenCodingRemoveTag', 'func': ':call zencoding#removeTag()<cr>'},
  \ {'mode': 'n', 'var': 'user_zen_anchorizeurl_key', 'key': 'a', 'plug': 'ZenCodingAnchorizeURL', 'func': ':call zencoding#anchorizeURL(0)<cr>'},
  \ {'mode': 'n', 'var': 'user_zen_anchorizesummary_key', 'key': 'A', 'plug': 'ZenCodingAnchorizeSummary', 'func': ':call zencoding#anchorizeURL(1)<cr>'},
  \]

    if !hasmapto('<plug>'.item.plug, item.mode)
      exe item.mode . 'noremap <plug>' . item.plug . ' ' . item.func
    endif
    if !exists('g:' . item.var)
    endif
    if exists('g:' . item.var)
      let key = eval('g:' . item.var)
    else
      let key = g:user_zen_leader_key . item.key
    endif
    if len(maparg(key, item.mode)) == 0
      exe item.mode . 'map <unique> ' . key . ' <plug>' . item.plug
    endif
  endfor
endfunction

function! s:install_plugin_v()
  for item in [
  \ {'mode': 'v', 'var': 'user_zen_expandabbr_key', 'key': ',', 'plug': 'ZenCodingExpandVisual', 'func': ':call zencoding#expandAbbr(2,"")<cr>'},
  \ {'mode': 'v', 'var': 'user_zen_balancetaginward_key', 'key': 'd', 'plug': 'ZenCodingBalanceTagInwardVisual', 'func': ':call zencoding#balanceTag(2)<cr>'},
  \ {'mode': 'v', 'var': 'user_zen_balancetagoutward_key', 'key': 'D', 'plug': 'ZenCodingBalanceTagOutwardVisual', 'func': ':call zencoding#balanceTag(-2)<cr>'},
  \ {'mode': 'v', 'var': 'user_zen_mergelines_key', 'key': 'm', 'plug': 'ZenCodingMergeLines', 'func': ':call zencoding#mergeLines()<cr>'},
  \ {'mode': 'v', 'var': 'user_zen_codepretty_key', 'key': 'c', 'plug': 'ZenCodingCodePretty', 'func': ':call zencoding#codePretty()<cr>'},
  \]

    if !hasmapto('<plug>'.item.plug, item.mode)
      exe item.mode . 'noremap <plug>' . item.plug . ' ' . item.func
    endif
    if !exists('g:' . item.var)
    endif
    if exists('g:' . item.var)
      let key = eval('g:' . item.var)
    else
      let key = g:user_zen_leader_key . item.key
    endif
    if len(maparg(key, item.mode)) == 0
      exe item.mode . 'map <unique> ' . key . ' <plug>' . item.plug
    endif
  endfor
endfunction


if exists('g:user_zen_mode') 
    let imode = matchstr(g:user_zen_mode, '[ai]')
    let nmode = matchstr(g:user_zen_mode, '[an]')
    let vmode = matchstr(g:user_zen_mode, '[av]')

    if !empty(imode)
        call s:install_plugin_i()
    endif

    if !empty(nmode)
        call s:install_plugin_n()
    endif

    if !empty(vmode)
        call s:install_plugin_v()
    endif
else
    call s:install_plugin_i()
    call s:install_plugin_n()
    call s:install_plugin_v()
endif


delfunction s:install_plugin_i
delfunction s:install_plugin_n
delfunction s:install_plugin_v

command! -nargs=1 Zen call zencoding#expandAbbr(4, <q-args>)

let &cpo = s:save_cpo
unlet s:save_cpo

" vim:set et:
