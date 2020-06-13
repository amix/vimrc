" File              : google.vim
" Date              : 28.02.2020
" Last Modified Date: 28.02.2020
source /usr/share/vim/google/google.vim


" Piper plugin mappings
Glug piper plugin[mappings]

" g4 plugin
" :G4Edit :G4Revert :G4Add
" :G4Pending
" :help g4
Glug g4



" BlazeDeps
" Use :BlazeDepsUpdate to update the BUILD file dependency
Glug blazedeps

" code format for build file
Glug codefmt
Glug codefmt-google auto_filetypes+=python,cc,proto,ts,html
autocmd FileType bzl AutoFormatBuffer buildifier

Glug relatedfiles plugin[mappings]=',2'

" Codesearch 
" <leader>cs code_search_query
" <leader>cf open current file bn cs
Glug corpweb plugin[mappings]


" Kythe LSP
au User lsp_setup call lsp#register_server({
    \ 'name': 'Kythe Language Server',
    \ 'cmd': {server_info->['/google/bin/releases/grok/tools/kythe_languageserver', '--google3']},
    \ 'whitelist': ['python', 'go', 'java', 'cpp', 'proto'],
    \})

" CiderLSP
au User lsp_setup call lsp#register_server({
    \ 'name': 'CiderLSP',
    \ 'cmd': {server_info->[
    \   '/google/bin/releases/editor-devtools/ciderlsp',
    \   '--tooltag=vim-lsp',
    \   '--noforward_sync_responses',
    \ ]},
    \ 'whitelist': ['c', 'cpp', 'proto', 'textproto', 'go'],
    \})
let g:asyncomplete_auto_popup = 0
let g:lsp_signs_error = {'text': '✗'}
let g:lsp_highlight_references_enabled = 1

let g:lsp_async_completion = 1

" Enable UI for diagnostics
let g:lsp_signs_enabled = 1           " enable diagnostics signs in the gutter
let g:lsp_diagnostics_echo_cursor = 1 " enable echo under cursor when in normal mode

" Enabling fuzzy completion
let g:asyncomplete_smart_completion = 1
let g:asyncomplete_auto_popup = 1

" asyncomplete
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr>    pumvisible() ? "\<C-y>" : "\<cr>"


" Clang include fixer
" Normal mode.
function HeaderFix()
  let g:clang_include_fixer_query_mode=0
  pyf /usr/lib/clang-include-fixer/clang-include-fixer.py
endfunction
function HeaderQuery()
  let g:clang_include_fixer_query_mode=1
  pyf /usr/lib/clang-include-fixer/clang-include-fixer.py
endfunction
command Headerfix call HeaderFix()
command Headerquery call HeaderQuery()


let localleader=","
nnoremap <localleader>hf :Headerfix<CR>
nnoremap <localleader>hq :Headerquery<CR>

" Show diff of the current file in a new pane.
nnoremap <localleader>d :SignifyDiff!<CR>

" Get the Citc path without google3/
function GetCitCPath() 
  let l:cwd = getcwd(1)
  let l:pathmatch = matchlist(l:cwd, '\v(.*)/google3.*$')
  if len(l:pathmatch) > 0
    return l:pathmatch[1]
  else
    return ''
  endif
endfunction

" If the file path start with //, treat it as from the client root.
function GetGoogle3Path(filepath, clientpath) 
  if strlen(a:clientpath)  == 0
    return a:filepath
  endif

  let l:dst_matches = matchlist(a:filepath, '\v//(.*)$')
  if len(l:dst_matches) > 0
    return a:clientpath.'/google3/'.l:dst_matches[1]
  else
    " If not starting with //, return the original path 
    return a:filepath
  endif 
endfunction

function GoogleECompletion(ArgLead, CmdLine, CursorPos)
  let l:citc = GetCitCPath()
  let l:filepath = GetGoogle3Path(a:ArgLead, l:citc)
  let l:head = fnamemodify(l:filepath, ':h')
  let l:tail = fnamemodify(l:filepath, ':t')
  let l:myList = split(globpath(l:head, l:tail.'*'), "\n")
  echom l:myList
  " length +1 for the /
  " Slicing the results
  return map(l:myList, 'v:val[strlen(l:head)+1:-1]')
endfunction

" An edit function that can recognize google3 path.
" The filepath is relative the google3/
" eg. you can go to //ads/video.txt wherever you are inside
" citc by :e //ads/video.txt.
function GoogleE(...) 
  " if the number of argument is not 0
  if a:0 
    let l:filepath = a:1
    let l:citc = GetCitCPath()
    let l:dst_filepath = GetGoogle3Path(l:filepath, l:citc)
    execute 'e '. l:dst_filepath
  else
    execute 'e'
  endif
endfunction

" command -nargs=+ -complete=customlist,GoogleECompletion GoogleE call GoogleE(<f-args>)
" help command-complete
command -nargs=* -complete=file GoogleE call GoogleE(<f-args>)

" Function to defind a built-in (lowercase) command.
function! CommandCabbr(abbreviation, expansion)
  " The replacement only happen when the command is leading by a ';'
  execute 'cabbr ' . a:abbreviation . ' <c-r>=getcmdpos() == 1 && getcmdtype() == ":" ? "' . a:expansion . '" : "' . a:abbreviation . '"<CR>'
endfunction
command! -nargs=+ CommandCabbr call CommandCabbr(<f-args>)
" Use it on itself to define a simpler abbreviation for itself.
execute 'CommandCabbr ccab CommandCabbr'
execute 'CommandCabbr e GoogleE'



" Class Access Operator toggle
" Only work if current char under the cursor is either . or ->
function ClassAccessOperatorToggle()
    " Col number starts at 1 while string index starts at 0.
    let cur_char = matchstr(getline('.'), '\%' . col('.') . 'c.')
    " == does not match case, but it is sufficient here. ==# match cases. For more :help expr-<
    " To do replace . with -> or replace -> with .
    let line = line(".")
    if cur_char == '.'
      normal dwi->
    endif
    if cur_char == '-' 
      normal dwi.
    endif
    if cur_char == '>'
      normal hdwi. 
    endif
endfunction 

command ClassAccessOperatorToggle call ClassAccessOperatorToggle()
nnoremap ,p :ClassAccessOperatorToggle<CR>

" Copy the buildable command of the current file to clipboard.
" If the current file is a/b/c/d/k.h
" Copy 'blaze build a/b/c/d:k' to clipboard.
function! GetCurrentFileBuildCmd()
  " citc:filepath
  let path = GetSmartFilePath() 
  let subs = split(path, ':')

  let file = expand('%:p:t')
  " Need to escapte the dot.
  let file_subs = split(file, '\.')
  if len(subs) > 1 && len(file_subs) > 0
      let output = 'blaze build /' . subs[1] . ':' . file_subs[0]
      " Copy to secondary clipboard, ctrl+c/v
      echom l:output
      let @+ = l:output
  endif
endfunction 

command GetCurrentFileBuildCmd call GetCurrentFileBuildCmd()
nmap <script> <silent> <unique> ,bb :GetCurrentFileBuildCmd<CR>


function CopyFilePathGoogle() 
  let path = expand('%:p')
  let l:dst_matches = matchlist(path, '\v(.*)/google3/(.*)$')
  if len(l:dst_matches) > 0
    let @* =  l:dst_matches[2]
  endif 
endfunction

command CopyFilePathGoogle call CopyFilePathGoogle()

Glug youcompleteme-google
nnoremap <silent> <C-]> :YcmCompleter GoTo<CR>
let g:ycm_always_populate_location_list = 1

" Disable template
let g:templates_no_autocmd = 1
