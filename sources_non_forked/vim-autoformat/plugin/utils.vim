
" Simple python-based random number generator
function! g:RandomInt()
if has("python3")
python3 << EOF
import random
result = random.randrange(1, 1000000)
vim.command('return ' + str(result))
EOF
else
python << EOF
import random
result = random.randrange(1, 1000000)
vim.command('return ' + str(result))
EOF
endif
endfunction

" Put the uncopyable messages text into the buffer
command! PutMessages redir @" | messages | redir END | put
