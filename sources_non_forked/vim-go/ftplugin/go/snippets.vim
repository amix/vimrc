" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

if exists("g:go_loaded_gosnippets")
  finish
endif
let g:go_loaded_gosnippets = 1

function! s:GoUltiSnips() abort
  if get(g:, 'did_plugin_ultisnips') isnot 1
    return
  endif

  if !exists("g:UltiSnipsSnippetDirectories")
    let g:UltiSnipsSnippetDirectories = ["gosnippets/UltiSnips"]
  else
    let g:UltiSnipsSnippetDirectories += ["gosnippets/UltiSnips"]
  endif
endfunction

function! s:GoNeosnippet() abort
  if get(g:, 'loaded_neosnippet') isnot 1
    return
  endif

  let g:neosnippet#enable_snipmate_compatibility = 1

  let l:gosnippets_dir = globpath(&rtp, 'gosnippets/snippets')
  if type(g:neosnippet#snippets_directory) == type([])
    let g:neosnippet#snippets_directory += [l:gosnippets_dir]
  elseif type(g:neosnippet#snippets_directory) == type("")
    if strlen(g:neosnippet#snippets_directory) > 0
      let g:neosnippet#snippets_directory = g:neosnippet#snippets_directory . "," . l:gosnippets_dir
    else
      let g:neosnippet#snippets_directory = l:gosnippets_dir
    endif
  endif
endfunction

function! s:GoMinisnip() abort
  if get(g:, 'loaded_minisnip') isnot 1
    return
  endif

  if exists('g:minisnip_dir')
    let g:minisnip_dir .= go#util#PathListSep() . globpath(&rtp, 'gosnippets/minisnip')
  else
    let g:minisnip_dir = globpath(&rtp, 'gosnippets/minisnip')
  endif
endfunction


let s:engine = go#config#SnippetEngine()
if s:engine is? "automatic"
  if get(g:, 'did_plugin_ultisnips') is 1
    call s:GoUltiSnips()
  elseif get(g:, 'loaded_neosnippet') is 1
    call s:GoNeosnippet()
  elseif get(g:, 'loaded_minisnip') is 1
    call s:GoMinisnip()
  endif
elseif s:engine is? "ultisnips"
  call s:GoUltiSnips()
elseif s:engine is? "neosnippet"
  call s:GoNeosnippet()
elseif s:engine is? "minisnip"
  call s:GoMinisnip()
endif

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
