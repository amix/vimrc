" Measure how long it takes to unplace signs.
"
" Source this file with `:source %` or `vim -S unplace.vim`


let num = 500
sign define Foo text=*

new

call append(0, range(1, num))

for i in range(1, num)
  execute "sign place ".i." line=".i." name=Foo buffer=".bufnr('')
endfor

let start = reltime()
for i in range(1, num)
  execute "sign unplace ".i
endfor
let elapsed = reltime(start)

bdelete!

echom split(reltimestr(elapsed))[0]."s to remove ".num." signs"
echom string(reltimefloat(elapsed) * 1000 / num).' ms/sign'
echom string(float2nr(num / reltimefloat(elapsed))).' sign/s'
