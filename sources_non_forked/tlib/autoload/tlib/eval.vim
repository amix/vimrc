" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    56


function! tlib#eval#FormatValue(value, ...) "{{{3
    TVarArg ['indent', 0]
    " TLogVAR a:value, indent
    let indent1 = indent + 1
    let indenti = repeat(' ', &sw)
    let type = type(a:value)
    let acc = []
    if type == 0 || type == 1 || type == 2
        " TLogDBG 'Use string() for type='. type
        call add(acc, string(a:value))
    elseif type == 3 "List
        " TLogDBG 'List'
        call add(acc, '[')
        for e in a:value
            call add(acc, printf('%s%s,', indenti, tlib#eval#FormatValue(e, indent1)))
            unlet e
        endfor
        call add(acc, ']')
    elseif type == 4 "Dictionary
        " TLogDBG 'Dictionary'
        call add(acc, '{')
        let indent1 = indent + 1
        for [k, v] in items(a:value)
            call add(acc, printf("%s%s: %s,", indenti, string(k), tlib#eval#FormatValue(v, indent1)))
            unlet k v
        endfor
        call add(acc, '}')
    else
        " TLogDBG 'Unknown type: '. string(a:value)
        call add(acc, string(a:value))
    endif
    if indent > 0
        let is = repeat(' ', indent * &sw)
        for i in range(1,len(acc) - 1)
            let acc[i] = is . acc[i]
        endfor
    endif
    return join(acc, "\n")
endf


function! tlib#eval#Extend(a, b, ...) abort "{{{3
    let mode = a:0 >= 1 ? a:1 : 'force'
    if type(a:a) != type(a:b)
        throw 'tlib#eval#Extend: Incompatible types: a='. string(a:a) .' b='. string(a:b)
    elseif type(a:a) == 3 " list
        return extend(a:a, a:b, mode)
    elseif type(a:a) == 4 " dict
        for k in keys(a:b)
            if has_key(a:a, k)
                if mode == 'force'
                    let a:a[k] = tlib#eval#Extend(copy(a:a[k]), a:b[k], mode)
                elseif mode == 'error'
                    throw 'tlib#eval#Extend: Key already exists: '. k
                endif
            else
                let a:a[k] = a:b[k]
            endif
            unlet! k
        endfor
        return a:a
    else
        return a:b
    endif
endf

