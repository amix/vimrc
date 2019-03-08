" asm.vim: Vim filetype plugin for Go assembler.

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

let b:undo_ftplugin = "setl fo< com< cms<
      \ | exe 'au! vim-go-asm-buffer * <buffer>'"

setlocal formatoptions-=t

setlocal comments=s1:/*,mb:*,ex:*/,://
setlocal commentstring=//\ %s

setlocal noexpandtab

command! -nargs=0 AsmFmt call go#asmfmt#Format()

" Autocommands
" ============================================================================

augroup vim-go-asm-buffer
  autocmd! * <buffer>

  autocmd BufWritePre <buffer> call go#auto#asmfmt_autosave()
augroup end

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
