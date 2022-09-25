### Issue

<!--
Please describe the issue here.

If you are not using jedi-vim from Git (but e.g. from a distribution's package,
please try it with jedi-vim's Git master, too).
-->

### Steps to reproduce

<!--
Include if relevant.

Please provide steps to reproduce it here, preferably based on a minimal Vim
configuration.

You can use the following template (save it as `minimal.vimrc` in the directory
where jedi-vim is installed, `cd` into that directory, and run Vim with
`vim -u minimal.vimrc`):

```
set nocompatible

let script_dir = fnamemodify(expand('<sfile>'), ':h')
let &runtimepath .= ','.script_dir.','.script_dir.'/after'

" Put your config changes here.
" let g:jedi#show_call_signatures=1

syntax on
filetype plugin indent on
```

Please provide the `minimal.vimrc` you have used here, too.
-->

### Output of “:verbose JediDebugInfo”

<!--
Please execute `:redir @+> | silent verb JediDebugInfo | redir END` in a
Python buffer to copy debug information into your clipboard.
Then paste it here.
-->
