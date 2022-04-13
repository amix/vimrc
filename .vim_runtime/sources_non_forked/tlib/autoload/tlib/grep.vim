" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Last Change: 2013-10-16.
" @Revision:    31


function! tlib#grep#Do(cmd, rx, files) "{{{3
    " TLogVAR a:cmd, a:rx, a:files
    let files = join(map(copy(a:files), 'tlib#arg#Ex(v:val, "")'), ' ')
    let rx = '/'. escape(a:rx, '/') .'/j'
    " TLogVAR rx, files
    silent exec a:cmd rx files
endf


function! tlib#grep#LocList(rx, files) "{{{3
    return tlib#grep#Do('noautocmd lvimgrep', a:rx, a:files)
endf


function! tlib#grep#QuickFixList(rx, files) "{{{3
    return tlib#grep#Do('noautocmd vimgrep', a:rx, a:files)
endf


function! tlib#grep#List(rx, files) "{{{3
    call setqflist([])
    call tlib#grep#Do('noautocmd vimgrepadd', a:rx, a:files)
    let qfl = getqflist()
    " TLogVAR qfl
    " try
        silent! colder
    " catch
    "     call setqflist([], 'r')
    " endtry
    return qfl
endf

