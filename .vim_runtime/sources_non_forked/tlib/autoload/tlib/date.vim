" date.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-03-25.
" @Last Change: 2015-11-23.
" @Revision:    21.0.34


if !exists('g:tlib#date#ShortDatePrefix') | let g:tlib#date#ShortDatePrefix = '20' | endif "{{{2
if !exists('g:tlib#date#TimeZoneShift')   | let g:tlib#date#TimeZoneShift = 0      | endif "{{{2

let g:tlib#date#dayshift = 60 * 60 * 24
" let g:tlib#date#date_rx = '\<\(\d\{4}\)-\(\d\d\)-\(\d\d\)\%(\s\+\(\(\d\d\):\(\d\d\)\)\)\?\>'
let g:tlib#date#date_rx = '\<\(\d\{4}\)-\(\d\d\)-\(\d\d\)\>'
let g:tlib#date#date_format = '%Y-%m-%d'


function! tlib#date#IsDate(text) abort "{{{3
    return a:text =~# '^'. g:tlib#date#date_rx .'$'
endf


function! tlib#date#Format(secs1970) abort "{{{3
    return strftime(g:tlib#date#date_format, a:secs1970)
endf


" :display: tlib#date#DiffInDays(date1, ?date2=localtime(), ?allow_zero=0)
function! tlib#date#DiffInDays(date, ...)
    let allow_zero = a:0 >= 2 ? a:2 : 0
    let s0 = tlib#date#SecondsSince1970(a:date, 0, allow_zero)
    let s1 = a:0 >= 1 ? tlib#date#SecondsSince1970(a:1, 0, allow_zero) : localtime()
    let dd = (s0 - s1) / g:tlib#date#dayshift
    " TLogVAR dd
    return dd
endf


" :display: tlib#date#Parse(date, ?allow_zero=0) "{{{3
function! tlib#date#Parse(date, ...) "{{{3
    let min = a:0 >= 1 && a:1 ? 0 : 1
    " TLogVAR a:date, min
    let m = matchlist(a:date, '^\(\d\{2}\|\d\{4}\)-\(\d\{1,2}\)-\(\d\{1,2}\)$')
    if !empty(m)
        let year = m[1]
        let month = m[2]
        let days = m[3]
    else
        let m = matchlist(a:date, '^\(\d\+\)/\(\d\{1,2}\)/\(\d\{1,2}\)$')
        if !empty(m)
            let year = m[1]
            let month = m[3]
            let days = m[2]
        else
            let m = matchlist(a:date, '^\(\d\{1,2}\)\.\s*\(\d\{1,2}\)\.\s*\(\d\d\{2}\|\d\{4}\)$')
            if !empty(m)
                let year = m[3]
                let month = m[2]
                let days = m[1]
            endif
        endif
    endif
    if empty(m) || year == '' || month == '' || days == '' || 
                \ month < min || month > 12 || days < min || days > 31
        echoerr 'TLib: Invalid date: '. a:date
        return []
    endif
    if strlen(year) == 2
        let year = g:tlib#date#ShortDatePrefix . year
    endif
    return [0 + year, 0 + month, 0 + days]
endf


" tlib#date#SecondsSince1970(date, ?daysshift=0, ?allow_zero=0)
function! tlib#date#SecondsSince1970(date, ...) "{{{3
    let allow_zero = a:0 >= 2 ? a:2 : 0
    " TLogVAR a:date, allow_zero
    let date = tlib#date#Parse(a:date, allow_zero)
    if empty(date)
        return 0
    endif
    let [year, month, days] = date
    if a:0 >= 1 && a:1 > 0
        let days = days + a:1
    end
    let days_passed = days
    let i = 1970
    while i < year
        let days_passed = days_passed + 365
        if i % 4 == 0 || i == 2000
            let days_passed = days_passed + 1
        endif
        let i = i + 1
    endwh
    let i = 1
    while i < month
        if i == 1
            let days_passed = days_passed + 31
        elseif i == 2
            let days_passed = days_passed + 28
            if year % 4 == 0 || year == 2000
                let days_passed = days_passed + 1
            endif
        elseif i == 3
            let days_passed = days_passed + 31
        elseif i == 4
            let days_passed = days_passed + 30
        elseif i == 5
            let days_passed = days_passed + 31
        elseif i == 6
            let days_passed = days_passed + 30
        elseif i == 7
            let days_passed = days_passed + 31
        elseif i == 8
            let days_passed = days_passed + 31
        elseif i == 9
            let days_passed = days_passed + 30
        elseif i == 10
            let days_passed = days_passed + 31
        elseif i == 11
            let days_passed = days_passed + 30
        endif
        let i = i + 1
    endwh
    let seconds = (days_passed - 1) * 24 * 60 * 60
    let seconds = seconds + (strftime('%H') + g:tlib#date#TimeZoneShift) * 60 * 60
    let seconds = seconds + strftime('%M') * 60
    let seconds = seconds + strftime('%S')
    return seconds
endf


function! tlib#date#Shift(date, shift) abort "{{{3
    let n = str2nr(matchstr(a:shift, '\d\+'))
    let ml = matchlist(a:date, g:tlib#date#date_rx)
    " TLogVAR a:date, a:shift, n, ml
    if a:shift =~ 'd$'
        let secs = tlib#date#SecondsSince1970(a:date) + g:tlib#date#dayshift * n
        " TLogVAR secs
        let date = tlib#date#Format(secs)
    elseif a:shift =~ 'w$'
        let secs = tlib#date#SecondsSince1970(a:date) + g:tlib#date#dayshift * n * 7
        let date = tlib#date#Format(secs)
    elseif a:shift =~ 'm$'
        let d = str2nr(ml[3])
        let ms = str2nr(ml[2]) + n
        let m = (ms - 1) % 12 + 1
        let yr = str2nr(ml[1]) + ms / 12
        let date = printf('%04d-%02d-%02d', yr, m, d)
        " TLogVAR d, ms, m, yr, date
    elseif a:shift =~ 'y$'
        let yr = str2nr(ml[1]) + n
        let date = substitute(a:date, '^\d\{4}', yr, '')
    endif
    " if !empty(ml[4]) && date !~ '\s'. ml[4] .'$'
    "     let date .= ' '. ml[4]
    " endif
    " TLogVAR date
    return date
endf

