fun! s:DetectScala()
    if getline(1) =~# '^#!\(/usr\)\?/bin/env\s\+scalas\?'
        set filetype=scala
    endif
endfun

au BufRead,BufNewFile *.scala set filetype=scala
au BufRead,BufNewFile * call s:DetectScala()

" Install vim-sbt for additional syntax highlighting.
au BufRead,BufNewFile *.sbt setfiletype sbt.scala
