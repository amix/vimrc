"" This file contains some option I like

" line number
set nu

" the color I used to
"colorscheme desert

" change current directory when switching files
set autochdir

""""""""""""""""""""""""
" [my plugin setup]
"
" ctags
set tags=tags;
set tags+=~/.systags

" cscope
if has("cscope")
	set csprg=/usr/bin/cscope
	set csto=1
	set cst 
	if filereadable("cscope.out")
		cs add cscope.out .
	elseif filereadable("../cscope.out")
		cs add ../cscope.out ..
	elseif filereadable("../../cscope.out")
		cs add ../../cscope.out ../..
	elseif filereadable("../../../cscope.out")
		cs add ../../../cscope.out ../../..
	elseif filereadable("../../../../cscope.out")
		cs add ../../../../cscope.out ../../../..
	elseif filereadable("../../../../../cscope.out")
		cs add ../../../../../cscope.out ../../../../..
	elseif filereadable("../../../../../../cscope.out")
		cs add ../../../../../../cscope.out ../../../../../..
	endif
	set csverb
endif

nmap <leader>css :cs find s <C-R>=expand("<cword>")<CR><CR>
nmap <leader>csg :cs find g <C-R>=expand("<cword>")<CR><CR>
nmap <leader>csc :cs find c <C-R>=expand("<cword>")<CR><CR>
nmap <leader>cst :cs find t <C-R>=expand("<cword>")<CR><CR>
nmap <leader>cse :cs find e <C-R>=expand("<cword>")<CR><CR>
nmap <leader>csf :cs find f <C-R>=expand("<cword>")<CR><CR>
nmap <leader>csi :cs find i ^<C-R>=expand("<cword>")<CR>$<CR>
nmap <leader>csd :cs find d <C-R>=expand("<cword>")<CR><CR>

#let g:miniBufExplMapCTabSwitchBufs = 1
#let g:miniBufExplMapWindowNavVim = 1
#let g:miniBufExplMapWindowNavArrows = 1

let tmpcolumn=$COLUMNS
execute "set columns=".tmpcolumn

