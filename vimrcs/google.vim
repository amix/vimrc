source /usr/share/vim/google/google.vim


" Piper plugin mappings
Glug piper plugin[mappings]

" g4 plugin
" :G4Edit :G4Revert :G4Add
" :G4Pending
" :help g4
Glug g4

Glug youcompleteme-google
let g:ycm_always_populate_location_list = 1

" BlazeDeps
" Use :BlazeDepsUpdate to update the BUILD file dependency
Glug blazedeps

" code format for build file
Glug codefmt
Glug codefmt-google
autocmd FileType bzl AutoFormatBuffer buildifier

Glug relatedfiles plugin[mappings]=',2'

" Codesearch 
" <leader>cs code_search_query
" <leader>cf open current file bn cs
Glug corpweb plugin[mappings]


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

" If the file path start with //, treat it as from the client root.
function GetGoogle3Path(filepath, clientpath) 
  let l:dst_matches = matchlist(a:filepath, '\v//(.*)$')
  if len(l:dst_matches) > 0
    return clientpath.'/'.l:dst_matches[1]
  else
    " If not starting with //, return the original path 
    return a:filepath
  endif 
endfunction

" An edit function that can recognize google3 path.
function GoogleE(filepath) 
  let l:cwd = getcwd(1)
  echom l:cwd
  let l:cur_matches = matchlist(l:cwd, '\v(.*google3).*$') 
  if len(l:cur_matches) > 0
    let l:dst_filepath = GetGoogle3Path(a:filepath, l:cur_matches[1])
    execute 'e '. l:dst_filepath
  else
    execute 'e '. a:filepath
  endif
endfunction

command -nargs=+ -complete=file GoogleE call GoogleE(<f-args>)

" Function to defind a built-in (lowercase) command.
function! CommandCabbr(abbreviation, expansion)
  " The replacement only happen when the command is leading by a ';'
  execute 'cabbr ' . a:abbreviation . ' <c-r>=getcmdpos() == 1 && getcmdtype() == ":" ? "' . a:expansion . '" : "' . a:abbreviation . '"<CR>'
endfunction
command! -nargs=+ CommandCabbr call CommandCabbr(<f-args>)
" Use it on itself to define a simpler abbreviation for itself.
execute 'CommandCabbr ccab CommandCabbr'
execute 'CommandCabbr e GoogleE'

