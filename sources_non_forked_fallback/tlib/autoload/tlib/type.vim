" @Author:      Tom Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-09-30.
" @Last Change: 2017-02-22.
" @Revision:    57


let g:tlib#type#nil = []


" Enable type assertiona via |:Tlibtype|.
function! tlib#type#Enable() abort "{{{3
    " :nodoc:
    command! -nargs=+ Tlibtype call tlib#type#Check(expand('<sfile>'), [<f-args>], [<args>])
endf


" Disable type assertiona via |:Tlibtype|.
function! tlib#type#Disable() abort "{{{3
    " :nodoc:
    command! -nargs=+ Tlibtype :
endf


function! tlib#type#IsNil(expr) abort "{{{3
    return tlib#type#Is(a:expr, v:t_none) || a:expr is g:tlib#type#nil
endf


function! tlib#type#IsNumber(expr)
    return tlib#type#Is(a:expr, 0)
endf


function! tlib#type#IsString(expr)
    return tlib#type#Is(a:expr, 1)
endf


function! tlib#type#IsFuncref(expr)
    return tlib#type#Is(a:expr, 2)
endf


function! tlib#type#IsList(expr)
    return tlib#type#Is(a:expr, 3)
endf


function! tlib#type#IsDictionary(expr)
    return tlib#type#Is(a:expr, 4)
endf


function! tlib#type#Is(val, type) abort "{{{3
    if has_key(s:schemas, a:type)
        return tlib#type#Has(a:val, a:type)
    else
        if type(a:type) == 0
            let type = a:type
        elseif a:type =~? '^b\%[oolean]$'
            let type = v:t_bool
        elseif a:type =~? '^c\%[hannel]$'
            let type = v:t_channel
        elseif a:type =~? '^d\%[ictionary]$'
            let type = v:t_dict
        elseif a:type =~? '^fl\%[oat]$'
            let type = v:t_float
        elseif a:type =~? '^fu\%[ncref]$'
            let type = v:t_func
        elseif a:type =~? '^j\%[ob]$'
            let type = v:t_job
        elseif a:type =~? '^l\%[ist]$'
            let type = v:t_list
        elseif a:type =~? '^\%(nil\|null\|none\)$'
            let type = v:t_none
        elseif a:type =~? '^n\%[umber]$'
            let type = v:t_number
        elseif a:type =~? '^s\%[tring]$'
            let type = v:t_string
        else
            throw 'tlib#type#Is: Unknown type: ' a:type
        endif
        Tlibtrace 'tlib', a:val, a:type, type, type(a:val), type(a:val) == a:type
        return type(a:val) == type
    endif
endf


function! tlib#type#Are(vals, type) abort "{{{3
    return tlib#assert#Map(a:vals, 'tlib#type#Is(v:val,'. string(a:type) .')')
endf


let s:schemas = {}


function! tlib#type#Define(name, schema) abort "{{{3
    let s:schemas[a:name] = deepcopy(a:schema)
endf


function! tlib#type#Has(val, schema) abort "{{{3
    Tlibtrace 'tlib', type(a:val), type(a:schema)
    if !tlib#type#IsDictionary(a:val)
        Tlibtrace 'tlib', 'not a dictionary', a:val
        return 0
    endif
    if tlib#type#IsString(a:schema)
        Tlibtrace 'tlib', a:schema
        let schema = copy(s:schemas[a:schema])
    else
        let schema = copy(a:schema)
    endif
    if tlib#type#IsDictionary(schema)
        return tlib#assert#All(map(schema, 'has_key(a:val, v:key) && tlib#type#Is(a:val[v:key], v:val)'))
    else
        Tlibtrace 'tlib', keys(a:val), schema
        return tlib#assert#All(map(schema, 'has_key(a:val, v:val)'))
    endif
endf


function! tlib#type#Have(vals, schema) abort "{{{3
    return tlib#assert#Map(a:vals, 'tlib#type#Has(v:val,'. string(a:schema) .')')
endf


function! tlib#type#Check(caller, names, vals) abort "{{{3
    Tlibtrace 'tlib', a:names, a:vals, len(a:names)
    for i in range(0, len(a:names) - 1, 2)
        let val = a:vals[i]
        let type = a:vals[i + 1]
        Tlibtrace 'tlib', i, val, type
        if !tlib#type#Is(val, type)
            let name = matchstr(a:names[i], '^''\zs.\{-}\ze'',\?$')
            throw 'tlib#type#Check: Type mismatch: '. name .':'. a:vals[i + 1]
        endif
    endfor
endf

