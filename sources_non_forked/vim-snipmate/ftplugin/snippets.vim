" Vim filetype plugin for SnipMate snippets (.snippets and .snippet files)

if exists("b:did_ftplugin")
    finish
endif
let b:did_ftplugin = 1

let b:undo_ftplugin = "setl et< sts< cms< fdm< fde<"

" Use hard tabs
setlocal noexpandtab softtabstop=0

setlocal foldmethod=expr foldexpr=getline(v:lnum)!~'^\\t\\\\|^$'?'>1':1

setlocal commentstring=#\ %s
setlocal nospell

command! -buffer -range=% RetabSnip
            \ echom "This command is deprecated. Use :retab and = instead. Doing that now."
            \ | <line1>,<line2>retab! | <line1>,<line2>normal =
