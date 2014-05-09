" date.vim
" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2010-03-25.
" @Last Change: 2010-09-17.
" @Revision:    0.0.34


if !exists('g:tlib#date#ShortDatePrefix') | let g:tlib#date#ShortDatePrefix = '20' | endif "{{{2
if !exists('g:tlib#date#TimeZoneShift')   | let g:tlib#date#TimeZoneShift = 0      | endif "{{{2

let g:tlib#date#dayshift = 60 * 60 * 24


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

