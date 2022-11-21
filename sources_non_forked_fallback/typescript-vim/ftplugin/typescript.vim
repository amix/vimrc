if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

let s:cpo_save = &cpo
set cpo-=C

compiler typescript
setlocal commentstring=//\ %s

" Set 'formatoptions' to break comment lines but not other lines,
" " and insert the comment leader when hitting <CR> or using "o".
setlocal formatoptions-=t formatoptions+=croql

setlocal suffixesadd+=.ts,.tsx

let b:undo_ftplugin = "setl fo< ofu< com< cms<"

let &cpo = s:cpo_save
unlet s:cpo_save
