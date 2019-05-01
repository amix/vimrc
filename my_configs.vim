"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Cheat.sh
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:CheatDoNotReplaceKeywordPrg=1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colorscheme
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Neovim
colorscheme gruvbox

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vizardry
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:VizardryGitMethod="submodule add"
let g:VizardryGitBaseDir="~\.vim_runtime\my_plugins"
let g:VizardryCommitMsgs={'Invoke': "[Vizardry] Invoked vim submodule:",
      \'Banish': "[Vizardry] Banished vim submodule:",
      \'Vanish': "[Vizardry] Vanished vim submodule:",
      \'Evolve': "[Vizardry] Evolved vim submodule:",
      \}

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Windows Clipboard
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
noremap <Leader>y "*y
noremap <Leader>p "*p
noremap <Leader>y "+y
noremap <Leader>p "+p

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => lightline
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:lightline = {
      \ 'colorscheme': 'gruvbox',
      \ 'active': {
      \   'left': [ ['mode', 'paste'],
      \             ['fugitive', 'gitbranch', 'readonly', 'filename', 'modified'] ],
      \   'right': [ [ 'lineinfo' ], ['percent'] ]
      \ },
      \ 'component': {
      \   'readonly': '%{&filetype=="help"?"":&readonly?"🔒":""}',
      \   'modified': '%{&filetype=="help"?"":&modified?"+":&modifiable?"":"-"}',
      \   'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
      \ },
      \ 'component_visible_condition': {
      \   'readonly': '(&filetype!="help"&& &readonly)',
      \   'modified': '(&filetype!="help"&&(&modified||!&modifiable))',
      \   'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())'
      \ },
      \ 'separator': { 'left': ' ', 'right': ' ' },
      \ 'subseparator': { 'left': ' ', 'right': ' ' }
      \ }

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Todo.txt
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Use todo#Complete as the omni complete function for todo files
au filetype todo setlocal omnifunc=todo#Complete

