" normal.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2008-10-06.
" @Last Change: 2010-09-22.
" @Revision:    28

let s:save_cpo = &cpo
set cpo&vim


" :display: tlib#normal#WithRegister(cmd, ?register='t', ?norm_cmd='norm!')
" Execute a normal command while maintaining all registers.
function! tlib#normal#WithRegister(cmd, ...) "{{{3
    TVarArg ['register', 't'], ['norm_cmd', 'norm!']
    let registers = {}
    for reg in split('123456789'. register, '\zs')
        exec 'let registers[reg] = @'. reg
    endfor
    exec 'let reg = @'. register
    try
        exec norm_cmd .' '. a:cmd
        exec 'return @'. register
    finally
        for [reg, value] in items(registers)
            exec 'let @'. reg .' = value'
        endfor
    endtry
endf


let &cpo = s:save_cpo
unlet s:save_cpo
