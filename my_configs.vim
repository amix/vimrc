set nu
set mouse+=a
nmap <leader>a :exec &mouse!=""? "set mouse=" : "set mouse+=a"<cr>
