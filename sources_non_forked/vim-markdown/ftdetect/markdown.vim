if !has('patch-7.4.480')
    " Before this patch, vim used modula2 for .md.
    au! filetypedetect BufRead,BufNewFile *.md
endif

" markdown filetype file
au BufRead,BufNewFile *.{md,mdown,mkd,mkdn,markdown,mdwn} setfiletype markdown
au BufRead,BufNewFile *.{md,mdown,mkd,mkdn,markdown,mdwn}.{des3,des,bf,bfa,aes,idea,cast,rc2,rc4,rc5,desx} setfiletype markdown
