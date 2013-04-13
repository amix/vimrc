" Filter_cnf.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2008-11-25.
" @Last Change: 2012-09-26.
" @Revision:    0.0.90

let s:prototype = tlib#Object#New({'_class': ['Filter_cnf'], 'name': 'cnf'}) "{{{2
let s:prototype.highlight = g:tlib_inputlist_higroup

" The search pattern for |tlib#input#List()| is in conjunctive normal 
" form: (P1 OR P2 ...) AND (P3 OR P4 ...) ...
" The pattern is a '/\V' very no-'/magic' regexp pattern.
"
" Pressing <space> joins two patterns with AND.
" Pressing | joins two patterns with OR.
" I.e. In order to get "lala AND (foo OR bar)", you type 
" "lala foo|bar".
"
" This is also the base class for other filters.
function! tlib#Filter_cnf#New(...) "{{{3
    let object = s:prototype.New(a:0 >= 1 ? a:1 : {})
    return object
endf


" :nodoc:
function! s:prototype.Init(world) dict "{{{3
endf


" :nodoc:
function! s:prototype.Help(world) dict "{{{3
    call a:world.PushHelp(
                \ printf('"%s", "%s", "%sWORD"', g:tlib_inputlist_and, g:tlib_inputlist_or, g:tlib_inputlist_not),
                \ 'AND, OR, NOT')
endf


" :nodoc:
function! s:prototype.AssessName(world, name) dict "{{{3
    let xa  = 0
    let prefix = self.FilterRxPrefix()
    for flt in a:world.filter_pos
        " let flt = prefix . a:world.GetRx(fltl)
        " if flt =~# '\u' && a:name =~# flt
        "     let xa += 5
        " endif

        if a:name =~ '\^'. flt
            let xa += 4
        elseif a:name =~ '\<'. flt
            let xa += 3
        " elseif a:name =~ '[[:punct:][:space:][:digit:]]'. flt
        "     let xa += 2
        elseif a:name =~ '\A'. flt .'\|'. flt .'\A'
            let xa += 1
        endif

        " if a:name =~ '\^'. flt .'\|'. flt .'\$'
        "     let xa += 4
        " elseif a:name =~ '\<'. flt .'\|'. flt .'\>'
        "     let xa += 3
        " " elseif a:name =~ flt .'\>'
        " "     let xa += 2
        " elseif a:name =~ '\A'. flt .'\|'. flt .'\A'
        "     let xa += 1
        " endif
        " if flt[0] =~# '\u' && matchstr(a:name, '\V\.\ze'. flt) =~# '\U'
        "     let xa += 1
        " endif
        " if flt[0] =~# '\U' && matchstr(a:name, '\V\.\ze'. flt) =~# '\u'
        "     let xa += 1
        " endif
        " if flt[-1] =~# '\u' && matchstr(a:name, '\V'. flt .'\zs\.') =~# '\U'
        "     let xa += 1
        " endif
        " if flt[-1] =~# '\U' && matchstr(a:name, '\V'. flt .'\zs\.') =~# '\u'
        "     let xa += 1
        " endif
    endfor
    " TLogVAR a:name, xa
    return xa
endf


" :nodoc:
function! s:prototype.Match(world, text) dict "{{{3
    " TLogVAR a:text
    " let sc = &smartcase
    " let ic = &ignorecase
    " if &ignorecase
    "     set smartcase
    " endif
    " try
    if !empty(a:world.filter_neg)
        for rx in a:world.filter_neg
            " TLogVAR rx
            if a:text =~ rx
                return 0
            endif
        endfor
    endif
    if !empty(a:world.filter_pos)
        for rx in a:world.filter_pos
            " TLogVAR rx
            if a:text !~ rx
                return 0
            endif
        endfor
    endif
    " finally
    "     let &smartcase = sc
    "     let &ignorecase = ic
    " endtry
    return 1
endf


" :nodoc:
function! s:prototype.DisplayFilter(filter) dict "{{{3
    let filter1 = deepcopy(a:filter)
    call map(filter1, '"(". join(reverse(self.Pretty(v:val)), " OR ") .")"')
    return join(reverse(filter1), ' AND ')
endf


function! s:prototype.Pretty(filter) dict "{{{3
    " call map(a:filter, 'substitute(v:val, ''\\\.\\{-}'', ''=>'', ''g'')')
    call map(a:filter, 'self.CleanFilter(v:val)')
    return a:filter
endf


" :nodoc:
function! s:prototype.SetFrontFilter(world, pattern) dict "{{{3
    let a:world.filter[0] = reverse(split(a:pattern, '\s*|\s*')) + a:world.filter[0][1 : -1]
endf


" :nodoc:
function! s:prototype.PushFrontFilter(world, char) dict "{{{3
    let a:world.filter[0][0] .= nr2char(a:char)
endf


" :nodoc:
function! s:prototype.ReduceFrontFilter(world) dict "{{{3
    let a:world.filter[0][0] = a:world.filter[0][0][0:-2]
endf


" :nodoc:
function! s:prototype.FilterRxPrefix() dict "{{{3
    return '\V'
endf


" :nodoc:
function! s:prototype.CleanFilter(filter) dict "{{{3
    return a:filter
endf

