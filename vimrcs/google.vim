source /usr/share/vim/google/google.vim


" Piper plugin mappings
Glug piper plugin[mappings]

" g4 plugin
Glug g4

Glug youcompleteme-google
let g:ycm_always_populate_location_list = 1

" BlazeDeps
" Use :BlazeDepsUpdate to update the BUILD file dependency
Glug blazedeps

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

