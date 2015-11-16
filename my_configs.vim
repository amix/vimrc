" No Uganda message on startup"
set shortmess=I

" Mode shown in the statusline all the time"
set noshowmode

"If we want backups, then put them under vim_runitime"
set backupdir=~\.vim_runtime\backupdir
" Block can move outside of bounds"
set virtualedit=block

" Turn spelling on by default, toggle with <F4>"
if has("spell")
  set spell
  " toggle spelling with F4 key
  map <F4> :set spell!<CR><Bar>:echo "Spell Check: " . strpart("OffOn", 3 * &spell, 3)<CR>
  " they were using white on white
  highlight PmenuSel ctermfg=black ctermbg=lightgray
  " limit it to just the top 10 items
  set sps=best,10
endif

set guifont=Source\ Code\ Pro:h11

nmap <leader>rb :call DeleteTrailingWS()<cr>

" Filtering through vimgrep results using regular expressions (amix on github)
map <leader>cc :botright cope<cr>
map <leader>co ggVGy:tabnew<cr>:set syntax=qf<cr>pgg
