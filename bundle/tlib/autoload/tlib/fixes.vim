" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Last Change: 2013-02-22.
" @Revision:    3


function! tlib#fixes#Winpos() "{{{3
    if has('gui_win32')
        return 'winpos '. getwinposx() .' '. getwinposy()
    else
        return ''
    endif
endf

