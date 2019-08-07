"=============================================================================
" FILE: t/smartsign_spec.vim
" AUTHOR: haya14busa
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

" Test for `smartsign` feature for find motions

" Avoid source test files {{{
if expand("%:p") ==# expand("<sfile>:p")
  finish
endif
"}}}

" Setup {{{
let s:root_dir = matchstr(system('git rev-parse --show-cdup'), '[^\n]\+')
execute 'set' 'rtp +=./'.s:root_dir
runtime! plugin/EasyMotion.vim
"}}}

" Functions for Test {{{
function! AddLine(str)
    put! =a:str
endfunction

function! CursorPos()
    return [line('.'), col('.'), getline('.')[col('.')-1]]
endfunction
"}}}

" Smartsign configulation {{{
describe 'Smartsign configulation'
    it 'provide default dictionary'
        let smartdict_us = g:EasyMotion#sticky_table#us
        let smartdict_jp = g:EasyMotion#sticky_table#jp
        Expect smartdict_us !=# {}
        Expect smartdict_jp !=# {}
    end
end
"}}}

" Basic Smartsign feature with 1-key findmotions with US layout {{{
describe 'Basic Smartsign feature with 1-key findmotions with US layout'
    before
        new
        let g:EasyMotion_keys = '123456789'
        let g:EasyMotion_use_smartsign_us = 1
        map s <Plug>(easymotion-s)
        call EasyMotion#init()
        call AddLine(' -_ =+ ;: [{ ]} `~ ''" \|')
        call AddLine(' 1! 2@ 3# 4$ 5% 6^ 7& 8* 9( 0)')
        call AddLine(' ,< .> /?')
        "             123456789012345678901234567890
        "                      1         2         3
        "
        " ',' : '<', '.' : '>', '/' : '?',
        " '1' : '!', '2' : '@', '3' : '#', '4' : '$', '5' : '%',
        " '6' : '^', '7' : '&', '8' : '*', '9' : '(', '0' : ')', '-' : '_', '=' : '+',
        " ';' : ':', '[' : '{', ']' : '}', '`' : '~', "'" : "\"", '\' : '|',
    end

    after
        close!
    end

    it 'works well for all sign as a target char'
        " Default position
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,' ']

        " ,<
        normal s,1
        Expect CursorPos() == [l,2,',']
        normal! 0
        normal s,2
        Expect CursorPos() == [l,3,'<']
        normal! 0
        normal s<1
        Expect CursorPos() == [l,3,'<']
        normal! 0
        normal s,3
        Expect CursorPos() == [l,1,' ']
        normal! 0

        " .>
        normal s.1
        Expect CursorPos() == [l,5,'.']
        normal! 0
        normal s.2
        Expect CursorPos() == [l,6,'>']
        normal! 0
        normal s>1
        Expect CursorPos() == [l,6,'>']
        normal! 0
        normal s.3
        Expect CursorPos() == [l,1,' ']
        normal! 0

        " /?
        normal s/1
        Expect CursorPos() == [l,8,'/']
        normal! 0
        normal s/2
        Expect CursorPos() == [l,9,'?']
        normal! 0
        normal s?1
        Expect CursorPos() == [l,9,'?']
        normal! 0
        normal s/3
        Expect CursorPos() == [l,1,' ']
        normal! 0

        " 1!
        normal s11
        Expect CursorPos() == [l+1,2,'1']
        normal! 0
        normal s12
        Expect CursorPos() == [l+1,3,'!']
        normal! 0
        normal s!1
        Expect CursorPos() == [l+1,3,'!']
        normal! 0
        normal s13
        Expect CursorPos() == [l+1,1,' ']
        normal! 0

        " 2@
        normal s21
        Expect CursorPos() == [l+1,5,'2']
        normal! 0
        normal s22
        Expect CursorPos() == [l+1,6,'@']
        normal! 0
        normal s@1
        Expect CursorPos() == [l+1,6,'@']
        normal! 0
        normal s23
        Expect CursorPos() == [l+1,1,' ']
        normal! 0

        " 3#
        normal s31
        Expect CursorPos() == [l+1,8,'3']
        normal! 0
        normal s32
        Expect CursorPos() == [l+1,9,'#']
        normal! 0
        normal s#1
        Expect CursorPos() == [l+1,9,'#']
        normal! 0
        normal s33
        Expect CursorPos() == [l+1,1,' ']
        normal! 0

        " 4$
        normal s41
        Expect CursorPos() == [l+1,11,'4']
        normal! 0
        normal s42
        Expect CursorPos() == [l+1,12,'$']
        normal! 0
        normal s$1
        Expect CursorPos() == [l+1,12,'$']
        normal! 0
        normal s43
        Expect CursorPos() == [l+1,1,' ']
        normal! 0

        " 5%
        normal s51
        Expect CursorPos() == [l+1,14,'5']
        normal! 0
        normal s52
        Expect CursorPos() == [l+1,15,'%']
        normal! 0
        normal s%1
        Expect CursorPos() == [l+1,15,'%']
        normal! 0
        normal s53
        Expect CursorPos() == [l+1,1,' ']
        normal! 0

        " 6^
        normal s61
        Expect CursorPos() == [l+1,17,'6']
        normal! 0
        normal s62
        Expect CursorPos() == [l+1,18,'^']
        normal! 0
        normal s^1
        Expect CursorPos() == [l+1,18,'^']
        normal! 0
        normal s63
        Expect CursorPos() == [l+1,1,' ']
        normal! 0

        " 7&
        normal s71
        Expect CursorPos() == [l+1,20,'7']
        normal! 0
        normal s72
        Expect CursorPos() == [l+1,21,'&']
        normal! 0
        normal s&1
        Expect CursorPos() == [l+1,21,'&']
        normal! 0
        normal s73
        Expect CursorPos() == [l+1,1,' ']
        normal! 0

        " 8*
        normal s81
        Expect CursorPos() == [l+1,23,'8']
        normal! 0
        normal s82
        Expect CursorPos() == [l+1,24,'*']
        normal! 0
        normal s*1
        Expect CursorPos() == [l+1,24,'*']
        normal! 0
        normal s83
        Expect CursorPos() == [l+1,1,' ']
        normal! 0

        " 9(
        normal s91
        Expect CursorPos() == [l+1,26,'9']
        normal! 0
        normal s92
        Expect CursorPos() == [l+1,27,'(']
        normal! 0
        normal s(1
        Expect CursorPos() == [l+1,27,'(']
        normal! 0
        normal s93
        Expect CursorPos() == [l+1,1,' ']
        normal! 0

        " 0)
        normal s01
        Expect CursorPos() == [l+1,29,'0']
        normal! 0
        normal s02
        Expect CursorPos() == [l+1,30,')']
        normal! 0
        normal s)1
        Expect CursorPos() == [l+1,30,')']
        normal! 0
        normal s03
        Expect CursorPos() == [l+1,1,' ']
        normal! 0

        " -_
        normal s-1
        Expect CursorPos() == [l+2,2,'-']
        normal! 0
        normal s-2
        Expect CursorPos() == [l+2,3,'_']
        normal! 0
        normal s_1
        Expect CursorPos() == [l+2,3,'_']
        normal! 0
        normal s-3
        Expect CursorPos() == [l+2,1,' ']
        normal! 0

        " =+
        normal s=1
        Expect CursorPos() == [l+2,5,'=']
        normal! 0
        normal s=2
        Expect CursorPos() == [l+2,6,'+']
        normal! 0
        normal s+1
        Expect CursorPos() == [l+2,6,'+']
        normal! 0
        normal s=3
        Expect CursorPos() == [l+2,1,' ']
        normal! 0

        " ;:
        normal s;1
        Expect CursorPos() == [l+2,8,';']
        normal! 0
        normal s;2
        Expect CursorPos() == [l+2,9,':']
        normal! 0
        normal s:1
        Expect CursorPos() == [l+2,9,':']
        normal! 0
        normal s;3
        Expect CursorPos() == [l+2,1,' ']
        normal! 0

        " [{
        normal s[1
        Expect CursorPos() == [l+2,11,'[']
        normal! 0
        normal s[2
        Expect CursorPos() == [l+2,12,'{']
        normal! 0
        normal s{1
        Expect CursorPos() == [l+2,12,'{']
        normal! 0
        normal s[3
        Expect CursorPos() == [l+2,1,' ']
        normal! 0

        " ]}
        normal s]1
        Expect CursorPos() == [l+2,14,']']
        normal! 0
        normal s]2
        Expect CursorPos() == [l+2,15,'}']
        normal! 0
        normal s}1
        Expect CursorPos() == [l+2,15,'}']
        normal! 0
        normal s]3
        Expect CursorPos() == [l+2,1,' ']
        normal! 0

        " `~
        normal s`1
        Expect CursorPos() == [l+2,17,'`']
        normal! 0
        normal s`2
        Expect CursorPos() == [l+2,18,'~']
        normal! 0
        normal s~1
        Expect CursorPos() == [l+2,18,'~']
        normal! 0
        normal s`3
        Expect CursorPos() == [l+2,1,' ']
        normal! 0

        " '"
        normal s'1
        Expect CursorPos() == [l+2,20,'''']
        normal! 0
        normal s'2
        Expect CursorPos() == [l+2,21,'"']
        normal! 0
        normal s"1
        Expect CursorPos() == [l+2,21,'"']
        normal! 0
        normal s'3
        Expect CursorPos() == [l+2,1,' ']
        normal! 0

        " \|
        normal s\1
        Expect CursorPos() == [l+2,23,'\']
        normal! 0
        normal s\2
        Expect CursorPos() == [l+2,24,'|']
        normal! 0
        normal s|1
        Expect CursorPos() == [l+2,24,'|']
        normal! 0
        normal s\3
        Expect CursorPos() == [l+2,1,' ']
        normal! 0
    end
end
"}}}

" Smartsign with 2-key find motions with US layout {{{
describe 'Smartsign with 2-key find motions with US layout'
    before
        new
        let g:EasyMotion_keys = '123456789'
        let g:EasyMotion_use_smartsign_us = 1
        map s <Plug>(easymotion-s2)
        call EasyMotion#init()
        call AddLine(' -_ =+ ;: [{ ]} `~ ''" \|')
        call AddLine(' 1! 2@ 3# 4$ 5% 6^ 7& 8* 9( 0)')
        call AddLine(' ,< .> /?')
        call AddLine(' -_ =+ ;: [{ ]} `~ ''" \|')
        call AddLine(' 1! 2@ 3# 4$ 5% 6^ 7& 8* 9( 0)')
        call AddLine(' ,< .> /?')
        "             123456789012345678901234567890
        "                      1         2         3
    end

    after
        close!
    end

    it 'works well'
        " Default position
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,' ']

        " ,<
        normal s,,1
        Expect CursorPos() == [l,2,',']
        normal! 0
        Expect CursorPos() == [l,1,' ']
        normal s,,3
        Expect CursorPos() == [l,1,' ']
        normal! 0
        normal s, 1
        Expect CursorPos() == [l,3,'<']
        normal! 0
        normal s<<1
        Expect CursorPos() == [l,1,' ']
        normal! 0
        normal s,<1
        Expect CursorPos() == [l,2,',']
        normal! 0
        normal s<,1
        Expect CursorPos() == [l,1,' ']
        normal! 0
    end
    it ': s,,3'
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,' ']
        normal s,,3
        Expect CursorPos() == [l,1,' ']
        normal! 0
    end

    it 'escape * asterisc #151'
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,' ']
        normal s1*22
        Expect CursorPos() == [l,1,' ']
        normal! 0
        normal s8*1
        Expect CursorPos() == [l+1,23,'8']
        normal! 0
        normal s881
        Expect CursorPos() == [l+1,23,'8']
        normal! 0
        normal s**1
        Expect CursorPos() == [l+1,1,' ']
        normal! 0
        normal s*81
        Expect CursorPos() == [l+1,1,' ']
        normal! 0
    end
end
"}}}

" Smartsign with 2-key find motions with JP layout {{{
describe 'Smartsign with 2-key find motions with JP layout'
    before
        new
        let g:EasyMotion_keys = '123456789'
        let g:EasyMotion_use_smartsign_jp = 1
        map s <Plug>(easymotion-s2)
        call EasyMotion#init()
        call AddLine(' -= ^~ ;+ :* [{ ]} @` \|')
        call AddLine(' 1! 2" 3# 4$ 5% 6& 7'' 8( 9) 0_')
        call AddLine(' ,< .> /?')
        call AddLine(' -= ^~ ;+ :* [{ ]} @` \|')
        call AddLine(' 1! 2" 3# 4$ 5% 6& 7'' 8( 9) 0_')
        call AddLine(' ,< .> /?')
        "             123456789012345678901234567890
        "                      1         2         3
        "
        "',' : '<', '.' : '>', '/' : '?',
        "'1' : '!', '2' : '"', '3' : '#', '4' : '$', '5' : '%',
        "'6' : '&', '7' : "'", '8' : '(', '9' : ')', '0' : '_', '-' : '=', '^' : '~',
        "';' : '+', ':' : '*', '[' : '{', ']' : '}', '@' : '`', '\' : '|',
        "
    end

    after
        close!
    end

    it 'works well'
        " Default position
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,' ']

        " ,<
        normal s,,1
        Expect CursorPos() == [l,2,',']
        normal! 0
        Expect CursorPos() == [l,1,' ']
        normal s,,3
        Expect CursorPos() == [l,1,' ']
        normal! 0
        normal s, 1
        Expect CursorPos() == [l,3,'<']
        normal! 0
        normal s<<1
        Expect CursorPos() == [l,1,' ']
        normal! 0
        normal s,<1
        Expect CursorPos() == [l,2,',']
        normal! 0
        normal s<,1
        Expect CursorPos() == [l,1,' ']
        normal! 0
    end
    it ': s,,3'
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,' ']
        normal s,,3
        Expect CursorPos() == [l,1,' ']
        normal! 0
    end
end
"}}}

" Smartsign with n-key find search motions {{{
describe 'Smartsign with n-key find search motions'
    before
        new
        let g:EasyMotion_keys = '123456789'
        let g:EasyMotion_use_smartsign_jp = 1
        map / <Plug>(easymotion-sn)
        call EasyMotion#init()
        call AddLine(' -= ^~ ;+ :* [{ ]} @` \|')
        call AddLine(' 1! 2" 3# 4$ 5% 6& 7'' 8( 9) 0_')
        call AddLine(' ,< .> /?')
        call AddLine(' -= ^~ ;+ :* [{ ]} @` \|')
        call AddLine(' 1! 2" 3# 4$ 5% 6& 7'' 8( 9) 0_')
        call AddLine(' ,< .> /?')
    end

    after
        close!
    end

    it 'do not work'
        " Default position
        normal! 0
        let l = line('.')
        Expect CursorPos() == [l,1,' ']

        " ,<
        normal /,,1
        Expect CursorPos() == [l,1,' ']
        normal! 0
        normal /,<1
        Expect CursorPos() == [l,1,' ']
        normal! 0
    end
end
"}}}

" __END__  {{{
" vim: expandtab softtabstop=4 shiftwidth=4
" vim: foldmethod=marker
" }}}
