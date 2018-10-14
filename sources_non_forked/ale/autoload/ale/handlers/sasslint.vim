" Author: KabbAmine - https://github.com/KabbAmine,
"   Ben Falconer <ben@falconers.me.uk>

function! ale#handlers#sasslint#GetCommand(buffer) abort
    return ale#path#BufferCdString(a:buffer)
    \   . ale#Escape('sass-lint')
    \   . ' -v -q -f compact %t'
endfunction
