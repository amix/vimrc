if !exists("g:go_list_type")
  let g:go_list_type = ""
endif

if !exists("g:go_list_type_commands")
  let g:go_list_type_commands = {}
endif

" Window opens the list with the given height up to 10 lines maximum.
" Otherwise g:go_loclist_height is used.
"
" If no or zero height is given it closes the window by default.
" To prevent this, set g:go_list_autoclose = 0
function! go#list#Window(listtype, ...) abort
  " we don't use lwindow to close the location list as we need also the
  " ability to resize the window. So, we are going to use lopen and lclose
  " for a better user experience. If the number of errors in a current
  " location list increases/decreases, cwindow will not resize when a new
  " updated height is passed. lopen in the other hand resizes the screen.
  if !a:0 || a:1 == 0
    let autoclose_window = get(g:, 'go_list_autoclose', 1)
    if autoclose_window
      if a:listtype == "locationlist"
        lclose
      else
        cclose
      endif
    endif
    return
  endif

  let height = get(g:, "go_list_height", 0)
  if height == 0
    " prevent creating a large location height for a large set of numbers
    if a:1 > 10
      let height = 10
    else
      let height = a:1
    endif
  endif

  if a:listtype == "locationlist"
    exe 'lopen ' . height
  else
    exe 'copen ' . height
  endif
endfunction


" Get returns the current items from the list
function! go#list#Get(listtype) abort
  if a:listtype == "locationlist"
    return getloclist(0)
  else
    return getqflist()
  endif
endfunction

" Populate populate the list with the given items
function! go#list#Populate(listtype, items, title) abort
  if a:listtype == "locationlist"
    call setloclist(0, a:items, 'r')

    " The last argument ({what}) is introduced with 7.4.2200:
    " https://github.com/vim/vim/commit/d823fa910cca43fec3c31c030ee908a14c272640
    if has("patch-7.4.2200") | call setloclist(0, [], 'a', {'title': a:title}) | endif
  else
    call setqflist(a:items, 'r')
    if has("patch-7.4.2200") | call setqflist([], 'a', {'title': a:title}) | endif
  endif
endfunction

" Parse parses the given items based on the specified errorformat and
" populates the list.
function! go#list#ParseFormat(listtype, errformat, items, title) abort
  " backup users errorformat, will be restored once we are finished
  let old_errorformat = &errorformat

  " parse and populate the location list
  let &errorformat = a:errformat
  try
    if a:listtype == "locationlist"
      lgetexpr a:items
      if has("patch-7.4.2200") | call setloclist(0, [], 'a', {'title': a:title}) | endif
    else
      cgetexpr a:items
      if has("patch-7.4.2200") | call setqflist([], 'a', {'title': a:title}) | endif
    endif
  finally
    "restore back
    let &errorformat = old_errorformat
  endtry
endfunction

" Parse parses the given items based on the global errorformat and
" populates the list.
function! go#list#Parse(listtype, items) abort
  if a:listtype == "locationlist"
    lgetexpr a:items
  else
    cgetexpr a:items
  endif
endfunction

" JumpToFirst jumps to the first item in the location list
function! go#list#JumpToFirst(listtype) abort
  if a:listtype == "locationlist"
    ll 1
  else
    cc 1
  endif
endfunction

" Clean cleans the location list
function! go#list#Clean(listtype) abort
  if a:listtype == "locationlist"
    lex []
  else
    cex []
  endif
endfunction

function! s:listtype(listtype) abort
  if g:go_list_type == "locationlist"
    return "locationlist"
  elseif g:go_list_type == "quickfix"
    return "quickfix"
  endif

  return a:listtype
endfunction

" s:default_list_type_commands is the defaults that will be used for each of
" the supported commands (see documentation for g:go_list_type_commands). When
" defining a default, quickfix should be used if the command operates on
" multiple files, while locationlist should be used if the command operates on a
" single file or buffer. Keys that begin with an underscore are not supported
" in g:go_list_type_commands.
let s:default_list_type_commands = {
      \ "GoBuild":              "quickfix",
      \ "GoErrCheck":           "quickfix",
      \ "GoFmt":                "locationlist",
      \ "GoGenerate":           "quickfix",
      \ "GoInstall":            "quickfix",
      \ "GoLint":               "quickfix",
      \ "GoMetaLinter":         "quickfix",
      \ "GoMetaLinterAutoSave": "locationlist",
      \ "GoModifyTags":         "locationlist",
      \ "GoRename":             "quickfix",
      \ "GoRun":                "quickfix",
      \ "GoTest":               "quickfix",
      \ "GoVet":                "quickfix",
      \ "_guru":                "locationlist",
      \ "_term":                "locationlist",
      \ "_job":                 "locationlist",
  \ }

function! go#list#Type(for) abort
  let l:listtype = s:listtype(get(s:default_list_type_commands, a:for))
  if l:listtype == "0"
    call go#util#EchoError(printf(
          \ "unknown list type command value found ('%s'). Please open a bug report in the vim-go repo.",
          \ a:for))
    let l:listtype = "quickfix"
  endif

  return get(g:go_list_type_commands, a:for, l:listtype)
endfunction

" vim: sw=2 ts=2 et
