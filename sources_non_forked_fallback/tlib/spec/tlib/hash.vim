" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    31


SpecBegin 'title': 'tlib#hash'


It should calculate CRC32B checksums.

let g:tlib_hash_use_crc32 = g:tlib#hash#use_crc32

let g:tlib#hash#use_crc32 = 'ruby'
Should be equal tlib#hash#CRC32B('The quick brown fox jumps over the lazy dog'), '414FA339'
Should be equal tlib#hash#CRC32B('foo'), '8C736521'
Should be equal tlib#hash#CRC32B('f'), '76D32BE0'

let g:tlib#hash#use_crc32 = 'vim'
Should be equal tlib#hash#CRC32B('The quick brown fox jumps over the lazy dog'), '414FA339'
Should be equal tlib#hash#CRC32B('foo'), '8C736521'
Should be equal tlib#hash#CRC32B('f'), '76D32BE0'


function! s:CompareHash(text) "{{{3
    if !empty(a:text)
        exec 'It should calculate the crc32b checksum for:' a:text
        let crc32ruby = tlib#hash#CRC32B_ruby(a:text)
        let crc32vim = tlib#hash#CRC32B_vim(a:text)
        exec 'Should be equal' string(crc32ruby) ',' string(crc32vim)
        exec 'It should calculate the adler32 checksum for:' a:text
        let adler32tlib = tlib#hash#Adler32_tlib(a:text)
        let adler32vim = tlib#hash#Adler32_vim(a:text)
        exec 'Should be equal' string(adler32tlib) ',' string(adler32vim)
    endif
endf

redir => s:scriptnames
silent scriptnames
redir END
for s:script in split(s:scriptnames, '\n')
    let s:scriptfile = matchstr(s:script, '^\s*\d\+:\s\+\zs.*$')
    call s:CompareHash(s:scriptfile)
    try
        let s:scriptlines = readfile(s:scriptfile)
        call s:CompareHash(join(s:scriptlines, "\n"))
        for s:scriptline in s:scriptlines
            call s:CompareHash(s:scriptline)
        endfor
    catch /^Vim\%((\a\+)\)\=:E484/
    endtry
endfor
unlet s:scriptnames, :script, s:scriptfile, s:scriptlines, s:scriptline
delf s:CompareHash


let g:tlib#hash#use_crc32 = g:tlib_hash_use_crc32

