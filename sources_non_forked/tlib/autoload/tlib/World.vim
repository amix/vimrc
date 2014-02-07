" World.vim -- The World prototype for tlib#input#List()
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-05-01.
" @Last Change: 2013-12-03.
" @Revision:    0.1.1310

" :filedoc:
" A prototype used by |tlib#input#List|.
" Inherits from |tlib#Object#New|.


" Size of the input list window (in percent) from the main size (of &lines).
" See |tlib#input#List()|.
TLet g:tlib_inputlist_pct = 50

" Size of filename columns when listing filenames.
" See |tlib#input#List()|.
TLet g:tlib_inputlist_width_filename = '&co / 3'
" TLet g:tlib_inputlist_width_filename = 25

" If true, |tlib#input#List()| will show some indicators about the 
" status of a filename (e.g. buflisted(), bufloaded() etc.).
" This is disabled by default because vim checks also for the file on 
" disk when doing this.
TLet g:tlib_inputlist_filename_indicators = 0

" If not null, display only a short info about the filter.
TLet g:tlib_inputlist_shortmessage = 0



" Known keys & values:
"   scratch_split ... See |tlib#scratch#UseScratch()|
let s:prototype = tlib#Object#New({
            \ '_class': 'World',
            \ 'name': 'world',
            \ 'allow_suspend': 1,
            \ 'base': [], 
            \ 'bufnr': -1,
            \ 'buffer_local': 1,
            \ 'cache_var': '',
            \ 'display_format': '',
            \ 'fileencoding': &fileencoding,
            \ 'fmt_display': {},
            \ 'fmt_filter': {},
            \ 'fmt_options': {},
            \ 'filetype': '',
            \ 'filter': [['']],
            \ 'filter_format': '',
            \ 'filter_options': '',
            \ 'follow_cursor': '',
            \ 'has_menu': 0,
            \ 'help_extra': [],
            \ 'index_table': [],
            \ 'initial_filter': [['']],
            \ 'initial_index': 1,
            \ 'initial_display': 1,
            \ 'initialized': 0,
            \ 'key_handlers': [],
            \ 'list': [],
            \ 'matcher': {},
            \ 'next_agent': '',
            \ 'next_eval': '',
            \ 'next_state': '',
            \ 'numeric_chars': g:tlib#input#numeric_chars,
            \ 'offset': 1,
            \ 'offset_horizontal': 0,
            \ 'on_leave': [],
            \ 'pick_last_item': tlib#var#Get('tlib#input#pick_last_item', 'bg'),
            \ 'post_handlers': [],
            \ 'query': '',
            \ 'resize': 0,
            \ 'resize_vertical': 0,
            \ 'restore_from_cache': [],
            \ 'filtered_items': [],
            \ 'retrieve_eval': '',
            \ 'return_agent': '',
            \ 'rv': '',
            \ 'scratch': '__InputList__',
            \ 'scratch_filetype': 'tlibInputList',
            \ 'scratch_hidden': g:tlib#scratch#hidden,
            \ 'scratch_vertical': 0,
            \ 'scratch_split': 1,
            \ 'sel_idx': [],
            \ 'show_empty': 0,
            \ 'state': 'display', 
            \ 'state_handlers': [],
            \ 'sticky': 0,
            \ 'temp_lines': [],
            \ 'temp_prompt': [],
            \ 'timeout': 0,
            \ 'timeout_resolution': 2,
            \ 'type': '', 
            \ 'win_wnr': -1,
            \ 'win_height': -1,
            \ 'win_width': -1,
            \ 'win_pct': 25,
            \ })
            " \ 'handlers': [],
            " \ 'filter_options': '\c',

function! tlib#World#New(...)
    let object = s:prototype.New(a:0 >= 1 ? a:1 : {})
    call object.SetMatchMode(tlib#var#Get('tlib#input#filter_mode', 'g', 'cnf'))
    return object
endf


" :nodoc:
function! s:prototype.Set_display_format(value) dict "{{{3
    if a:value == 'filename'
        call self.Set_highlight_filename()
        let self.display_format = 'world.FormatFilename(%s)'
    else
        let self.display_format = a:value
    endif
endf


" :nodoc:
function! s:prototype.Set_highlight_filename() dict "{{{3
    let self.tlib_UseInputListScratch = 'call world.Highlight_filename()'
endf


if g:tlib#input#format_filename == 'r'

    " :nodoc:
    function! s:prototype.Highlight_filename() dict "{{{3
        syntax match TLibDir /\s\+\zs.\{-}[\/]\ze[^\/]\+$/
        hi def link TLibDir Directory
        syntax match TLibFilename /[^\/]\+$/
        hi def link TLibFilename Normal
    endf

    " :nodoc:
    function! s:prototype.FormatFilename(file) dict "{{{3
        if !has_key(self.fmt_options, 'maxlen')
            let maxco = &co - len(len(self.base)) - eval(g:tlib#input#filename_padding_r)
            let maxfi = max(map(copy(self.base), 'strwidth(v:val)'))
            let self.fmt_options.maxlen = min([maxco, maxfi])
            " TLogVAR maxco, maxfi, self.fmt_options.maxlen
        endif
        let max = self.fmt_options.maxlen
        if len(a:file) > max
            let filename = '...' . strpart(a:file, len(a:file) - max + 3)
        else
            let filename = printf('% '. max .'s', a:file)
        endif
        return filename
    endf

else

    " :nodoc:
    function! s:prototype.Highlight_filename() dict "{{{3
        " let self.width_filename = 1 + eval(g:tlib_inputlist_width_filename)
        " TLogVAR self.base
        let self.width_filename = min([
                    \ get(self, 'width_filename', &co),
                    \ empty(g:tlib#input#filename_max_width) ? &co : eval(g:tlib#input#filename_max_width),
                    \ max(map(copy(self.base), 'strwidth(matchstr(v:val, "[^\\/]*$"))'))
                    \ ])
       "  TLogVAR self.width_filename
         exec 'syntax match TLibDir /\%>'. (1 + self.width_filename) .'c \(|\|\[[^]]*\]\) \zs\(\(\a:\|\.\.\|\.\.\..\{-}\)\?[\/][^&<>*|]\{-}\)\?[^\/]\+$/ contained containedin=TLibMarker contains=TLibFilename'
         exec 'syntax match TLibMarker /\%>'. (1 + self.width_filename) .'c \(|\|\[[^]]*\]\) \S.*$/ contains=TLibDir'
       "  exec 'syntax match TLibDir /\(|\|\[.\{-}\]\) \zs\(\(\a:\|\.\.\|\.\.\..\{-}\)\?[\/][^&<>*|]\{-}\)\?[^\/]\+$/ contained containedin=TLibMarker contains=TLibFilename'
       "  exec 'syntax match TLibMarker /\(|\|\[.\{-}\]\) \S.*$/ contains=TLibDir'
        exec 'syntax match TLibFilename /[^\/]\+$/ contained containedin=TLibDir'
        hi def link TLibMarker Special
        hi def link TLibDir Directory
        hi def link TLibFilename NonText
        " :nodoc:
        function! self.Highlighter(rx) dict
            let rx = '/\c\%>'. (1 + self.width_filename) .'c \(|\|\[[^]]*\]\) .\{-}\zs'. escape(a:rx, '/') .'/'
            exec 'match' self.matcher.highlight rx
        endf
    endf


    " :nodoc:
    function! s:prototype.FormatFilename(file) dict "{{{3
        " TLogVAR a:file
        let width = self.width_filename
        let split = match(a:file, '[/\\]\zs[^/\\]\+$')
        if split == -1
            let fname = a:file
            let dname = a:file
        else
            let fname = strpart(a:file, split)
            " let dname = strpart(a:file, 0, split - 1)
            let dname = a:file
        endif
        if strwidth(fname) > width
            let fname = strpart(fname, 0, width - 3) .'...'
        endif
        let dnmax = &co - max([width, strwidth(fname)]) - 10 - self.index_width - &fdc
        if g:tlib_inputlist_filename_indicators
            let dnmax -= 2
        endif
        if strwidth(dname) > dnmax
            let dname = '...'. strpart(dname, len(dname) - dnmax)
        endif
        let marker = []
        let use_indicators = g:tlib_inputlist_filename_indicators || has_key(self, 'filename_indicators')
        " TLogVAR use_indicators
        if use_indicators
            call insert(marker, '[')
            if g:tlib_inputlist_filename_indicators
                let bnr = bufnr(a:file)
                TLogVAR a:file, bnr, self.bufnr
                if bnr != -1
                    if bnr == self.bufnr
                        call add(marker, '%')
                    else
                        call add(marker, bnr)
                    endif
                    if getbufvar(bnr, '&modified')
                        call add(marker, '+')
                    endif
                    if getbufvar(bnr, '&bufhidden') == 'hide'
                        call add(marker, 'h')
                    endif
                    " if !buflisted(bnr)
                    "     call add(marker, 'u')
                    " endif
                    " echom "DBG" a:file string(get(self,'filename_indicators'))
                endif
            endif
            if has_key(self, 'filename_indicators') && has_key(self.filename_indicators, a:file)
                if len(marker) > 1
                    call add(marker, '|')
                endif
                call add(marker, self.filename_indicators[a:file])
            endif
            if len(marker) <= 1
                call add(marker, ' ')
            endif
            call add(marker, ']')
        else
            call add(marker, '|')
        endif
        return printf("%-*s %s %s",
                    \ self.width_filename + len(fname) - strwidth(fname),
                    \ fname, join(marker, ''), dname)
    endf

endif


" :nodoc:
function! s:prototype.GetSelectedItems(current) dict "{{{3
    " TLogVAR a:current
    if stridx(self.type, 'i') != -1
        let rv = copy(self.sel_idx)
    else
        let rv = map(copy(self.sel_idx), 'self.GetBaseItem(v:val)')
    endif
    if !empty(a:current)
        " TLogVAR a:current, rv, type(a:current)
        if tlib#type#IsNumber(a:current) || tlib#type#IsString(a:current)
            call s:InsertSelectedItems(rv, a:current)
        elseif tlib#type#IsList(a:current)
            for item in a:current
                call s:InsertSelectedItems(rv, item)
            endfor
        elseif tlib#type#IsDictionary(a:current)
            for [inum, item] in items(a:current)
                call s:InsertSelectedItems(rv, item)
            endfor
        endif
    endif
    " TAssert empty(rv) || rv[0] == a:current
    if stridx(self.type, 'i') != -1
        if !empty(self.index_table)
            " TLogVAR rv, self.index_table
            call map(rv, 'self.index_table[v:val - 1]')
            " TLogVAR rv
        endif
    endif
    return rv
endf


function! s:InsertSelectedItems(rv, current) "{{{3
    let ci = index(a:rv, a:current)
    if ci != -1
        call remove(a:rv, ci)
    endif
    call insert(a:rv, a:current)
endf


" :nodoc:
function! s:prototype.SelectItemsByNames(mode, items) dict "{{{3
    for item in a:items
        let bi = index(self.base, item) + 1
        " TLogVAR item, bi
        if bi > 0
            let si = index(self.sel_idx, bi)
            " TLogVAR self.sel_idx
            " TLogVAR si
            if si == -1
                call add(self.sel_idx, bi)
            elseif a:mode == 'toggle'
                call remove(self.sel_idx, si)
            endif
        endif
    endfor
    return 1
endf


" :nodoc:
function! s:prototype.SelectItem(mode, index) dict "{{{3
    let bi = self.GetBaseIdx(a:index)
    " if self.RespondTo('MaySelectItem')
    "     if !self.MaySelectItem(bi)
    "         return 0
    "     endif
    " endif
    " TLogVAR bi
    let si = index(self.sel_idx, bi)
    " TLogVAR self.sel_idx
    " TLogVAR si
    if si == -1
        call add(self.sel_idx, bi)
    elseif a:mode == 'toggle'
        call remove(self.sel_idx, si)
    endif
    return 1
endf


" :nodoc:
function! s:prototype.FormatArgs(format_string, arg) dict "{{{3
    let nargs = len(substitute(a:format_string, '%%\|[^%]', '', 'g'))
    return [a:format_string] + repeat([string(a:arg)], nargs)
endf


" :nodoc:
function! s:prototype.GetRx(filter) dict "{{{3
    return '\('. join(filter(copy(a:filter), 'v:val[0] != "!"'), '\|') .'\)' 
endf


" :nodoc:
function! s:prototype.GetRx0(...) dict "{{{3
    exec tlib#arg#Let(['negative'])
    let rx0 = []
    for filter in self.filter
        " TLogVAR filter
        let rx = join(reverse(filter(copy(filter), '!empty(v:val)')), '\|')
        " TLogVAR rx
        if !empty(rx) && (negative ? rx[0] == g:tlib#input#not : rx[0] != g:tlib#input#not)
            call add(rx0, rx)
        endif
    endfor
    let rx0s = join(rx0, '\|')
    if empty(rx0s)
        return ''
    else
        return self.FilterRxPrefix() .'\('. rx0s .'\)'
    endif
endf


" :nodoc:
function! s:prototype.FormatName(cache, format, value) dict "{{{3
    " TLogVAR a:format, a:value
    " TLogDBG has_key(self.fmt_display, a:value)
    if has_key(a:cache, a:value)
        " TLogDBG "cached"
        return a:cache[a:value]
    else
        let world = self
        let ftpl = self.FormatArgs(a:format, a:value)
        let fn = call(function("printf"), ftpl)
        let fmt = eval(fn)
        " TLogVAR ftpl, fn, fmt
        let a:cache[a:value] = fmt
        return fmt
    endif
endf


" :nodoc:
function! s:prototype.GetItem(idx) dict "{{{3
    return self.list[a:idx - 1]
endf


" :nodoc:
function! s:prototype.GetListIdx(baseidx) dict "{{{3
    " if empty(self.index_table)
        let baseidx = a:baseidx
    " else
    "     let baseidx = 0 + self.index_table[a:baseidx - 1]
    "     " TLogVAR a:baseidx, baseidx, self.index_table 
    " endif
    let rv = index(self.table, baseidx)
    " TLogVAR rv, self.table
    return rv
endf


" :nodoc:
" The first index is 1.
function! s:prototype.GetBaseIdx(idx) dict "{{{3
    " TLogVAR a:idx, self.table, self.index_table
    if !empty(self.table) && a:idx > 0 && a:idx <= len(self.table)
        return self.table[a:idx - 1]
    else
        return 0
    endif
endf


" :nodoc:
function! s:prototype.GetBaseIdx0(idx) dict "{{{3
    let idx0 = self.GetBaseIdx(a:idx) - 1
    if idx0 < 0
        call tlib#notify#Echo('TLIB: Internal Error: GetBaseIdx0: idx0 < 0', 'WarningMsg')
    endif
    return idx0
endf


" :nodoc:
function! s:prototype.GetBaseItem(idx) dict "{{{3
    return self.base[a:idx - 1]
endf


" :nodoc:
function! s:prototype.SetBaseItem(idx, item) dict "{{{3
    let self.base[a:idx - 1] = a:item
endf


" :nodoc:
function! s:prototype.GetLineIdx(lnum) dict "{{{3
    let line = getline(a:lnum)
    let prefidx = substitute(matchstr(line, '^\d\+\ze[*:]'), '^0\+', '', '')
    return prefidx
endf


" :nodoc:
function! s:prototype.SetPrefIdx() dict "{{{3
    " let pref = sort(range(1, self.llen), 'self.SortPrefs')
    " let self.prefidx = get(pref, 0, self.initial_index)
    let pref_idx = -1
    let pref_weight = -1
    " TLogVAR self.filter_pos, self.filter_neg
    for idx in range(1, self.llen)
        let item = self.GetItem(idx)
        let weight = self.matcher.AssessName(self, item)
        " TLogVAR item, weight
        if weight > pref_weight
            let pref_idx = idx
            let pref_weight = weight
        endif
    endfor
    " TLogVAR pref_idx
    " TLogDBG self.GetItem(pref_idx)
    if pref_idx == -1
        let self.prefidx = self.initial_index
    else
        let self.prefidx = pref_idx
    endif
endf


" " :nodoc:
" function! s:prototype.GetCurrentItem() dict "{{{3
"     let idx = self.prefidx
"     " TLogVAR idx
"     if stridx(self.type, 'i') != -1
"         return idx
"     elseif !empty(self.list)
"         if len(self.list) >= idx
"             let idx1 = idx - 1
"             let rv = self.list[idx - 1]
"             " TLogVAR idx, idx1, rv, self.list
"             return rv
"         endif
"     else
"         return ''
"     endif
" endf


" :nodoc:
function! s:prototype.CurrentItem() dict "{{{3
    if stridx(self.type, 'i') != -1
        return self.GetBaseIdx(self.llen == 1 ? 1 : self.prefidx)
    else
        if self.llen == 1
            " TLogVAR self.llen
            return self.list[0]
        elseif self.prefidx > 0
            " TLogVAR self.prefidx
            " return self.GetCurrentItem()
            if len(self.list) >= self.prefidx
                let rv = self.list[self.prefidx - 1]
                " TLogVAR idx, rv, self.list
                return rv
            endif
        else
            return ''
        endif
    endif
endf


" :nodoc:
function! s:prototype.FilterRxPrefix() dict "{{{3
    return self.matcher.FilterRxPrefix()
endf


" :nodoc:
function! s:prototype.SetFilter() dict "{{{3
    " let mrx = '\V'. (a:0 >= 1 && a:1 ? '\C' : '')
    let mrx = self.FilterRxPrefix() . self.filter_options
    let self.filter_pos = []
    let self.filter_neg = []
    " TLogVAR mrx, self.filter
    for filter in self.filter
        " TLogVAR filter
        let rx = join(reverse(filter(copy(filter), '!empty(v:val)')), '\|')
        " TLogVAR rx
        if !empty(rx)
            if rx =~ '\u'
                let mrx1 = mrx .'\C'
            else
                let mrx1 = mrx
            endif
            " TLogVAR rx
            if rx[0] == g:tlib#input#not
                if len(rx) > 1
                    call add(self.filter_neg, mrx1 .'\('. rx[1:-1] .'\)')
                endif
            else
                call add(self.filter_pos, mrx1 .'\('. rx .'\)')
            endif
        endif
    endfor
    " TLogVAR self.filter_pos, self.filter_neg
endf


" :nodoc:
function! s:prototype.IsValidFilter() dict "{{{3
    let last = self.FilterRxPrefix() .'\('. self.filter[0][0] .'\)'
    " TLogVAR last
    try
        let a = match("", last)
        return 1
    catch
        return 0
    endtry
endf


" :nodoc:
function! s:prototype.SetMatchMode(match_mode) dict "{{{3
    " TLogVAR a:match_mode
    if !empty(a:match_mode)
        unlet self.matcher
        try
            let self.matcher = tlib#Filter_{a:match_mode}#New()
            call self.matcher.Init(self)
        catch /^Vim\%((\a\+)\)\=:E117/
            throw 'tlib: Unknown mode for tlib#input#filter_mode: '. a:match_mode
        endtry
    endif
endf


" function! s:prototype.Match(text) dict "{{{3
"     return self.matcher.Match(self, text)
" endf


" :nodoc:
function! s:prototype.MatchBaseIdx(idx) dict "{{{3
    let text = self.GetBaseItem(a:idx)
    if !empty(self.filter_format)
        let text = self.FormatName(self.fmt_filter, self.filter_format, text)
    endif
    " TLogVAR text
    " return self.Match(text)
    return self.matcher.Match(self, text)
endf


" :nodoc:
function! s:prototype.BuildTableList() dict "{{{3
    " let time0 = str2float(reltimestr(reltime()))  " DBG
    " TLogVAR time0
    call self.SetFilter()
    " TLogVAR self.filter_neg, self.filter_pos
    let self.table = range(1, len(self.base))
    " TLogVAR self.filtered_items
    let copy_base = 1
    if !empty(self.filtered_items)
        let self.table = filter(self.table, 'index(self.filtered_items, v:val) != -1')
        let copy_base = 0
    endif
    if !empty(self.filter_pos) || !empty(self.filter_neg)
        let self.table = filter(self.table, 'self.MatchBaseIdx(v:val)')
        let copy_base = 0
    endif
    if copy_base
        let self.list = copy(self.base)
    else
        let self.list  = map(copy(self.table), 'self.GetBaseItem(v:val)')
    endif
endf


" :nodoc:
function! s:prototype.ReduceFilter() dict "{{{3
    " TLogVAR self.filter
    if self.filter[0] == [''] && len(self.filter) > 1
        call remove(self.filter, 0)
    elseif empty(self.filter[0][0]) && len(self.filter[0]) > 1
        call remove(self.filter[0], 0)
    else
        call self.matcher.ReduceFrontFilter(self)
    endif
endf


" :nodoc:
" filter is either a string or a list of list of strings.
function! s:prototype.SetInitialFilter(filter) dict "{{{3
    " let self.initial_filter = [[''], [a:filter]]
    if type(a:filter) == 3
        let self.initial_filter = copy(a:filter)
    else
        let self.initial_filter = [[a:filter]]
    endif
endf


" :nodoc:
function! s:prototype.PopFilter() dict "{{{3
    " TLogVAR self.filter
    if len(self.filter[0]) > 1
        call remove(self.filter[0], 0)
    elseif len(self.filter) > 1
        call remove(self.filter, 0)
    else
        let self.filter[0] = ['']
    endif
endf


" :nodoc:
function! s:prototype.FilterIsEmpty() dict "{{{3
    " TLogVAR self.filter
    return self.filter == copy(self.initial_filter)
endf


" :nodoc:
function! s:prototype.DisplayFilter() dict "{{{3
    let filter1 = copy(self.filter)
    call filter(filter1, 'v:val != [""]')
    " TLogVAR self.matcher['_class']
    let rv = self.matcher.DisplayFilter(filter1)
    let rv = self.CleanFilter(rv)
    return rv
endf


" :nodoc:
function! s:prototype.SetFrontFilter(pattern) dict "{{{3
    call self.matcher.SetFrontFilter(self, a:pattern)
endf


" :nodoc:
function! s:prototype.PushFrontFilter(char) dict "{{{3
    call self.matcher.PushFrontFilter(self, a:char)
endf


" :nodoc:
function! s:prototype.CleanFilter(filter) dict "{{{3
    return self.matcher.CleanFilter(a:filter)
endf


" :nodoc:
function! s:prototype.UseScratch() dict "{{{3
    " if type(self.scratch) != 0 && get(self, 'buffer_local', 1)
    "     if self.scratch != fnamemodify(self.scratch, ':p')
    "         let self.scratch = tlib#file#Join([expand('%:p:h'), self.scratch])
    "         " TLogVAR self.scratch
    "     endif
    "     " let self.scratch_hidden = 'wipe'
    " endif
    keepjumps keepalt let rv = tlib#scratch#UseScratch(self)
    " if expand('%:t') == self.scratch
        let b:tlib_world = self
    " endif
    return rv
endf


" :nodoc:
function! s:prototype.CloseScratch(...) dict "{{{3
    TVarArg ['reset_scratch', 0]
    " TVarArg ['reset_scratch', 1]
    " TLogVAR reset_scratch
    if self.sticky
        return 0
    else
        let rv = tlib#scratch#CloseScratch(self, reset_scratch)
        " TLogVAR rv
        if rv
            call self.SwitchWindow('win')
        endif
        return rv
    endif
endf


" :nodoc:
function! s:prototype.Initialize() dict "{{{3
    let self.initialized = 1
    call self.SetOrigin(1)
    call self.Reset(1)
    if !empty(self.cache_var) && exists(self.cache_var)
        for prop in self.restore_from_cache
            exec 'let self[prop] = get('. self.cache_var .', prop, self[prop])'
        endfor
        exec 'unlet '. self.cache_var
    endif
endf


" :nodoc:
function! s:prototype.Leave() dict "{{{3
    if !empty(self.cache_var)
        exec 'let '. self.cache_var .' = self'
    endif
    for handler in self.on_leave
        call call(handler, [self])
    endfor
endf


" :nodoc:
function! s:prototype.UseInputListScratch() dict "{{{3
    let scratch = self.UseScratch()
    if !exists('b:tlib_list_init')
        call tlib#autocmdgroup#Init()
        autocmd TLib VimResized <buffer> call feedkeys("\<c-j>", 't')
        let b:tlib_list_init = 1
    endif
    if !exists('w:tlib_list_init')
        " TLogVAR scratch
        syntax match InputlListIndex /^\d\+:/
        syntax match InputlListCursor /^\d\+\* .*$/ contains=InputlListIndex
        syntax match InputlListSelected /^\d\+# .*$/ contains=InputlListIndex
        hi def link InputlListIndex Constant
        hi def link InputlListCursor Search
        hi def link InputlListSelected IncSearch
        setlocal nowrap
        " hi def link InputlListIndex Special
        " let b:tlibDisplayListMarks = {}
        let b:tlibDisplayListMarks = []
        let b:tlibDisplayListWorld = self
        call tlib#hook#Run('tlib_UseInputListScratch', self)
        let w:tlib_list_init = 1
    endif
    return scratch
endf


" s:prototype.Reset(?initial=0)
" :nodoc:
function! s:prototype.Reset(...) dict "{{{3
    TVarArg ['initial', 0]
    " TLogVAR initial
    let self.state     = 'display'
    let self.offset    = 1
    let self.filter    = deepcopy(self.initial_filter)
    let self.idx       = ''
    let self.prefidx   = 0
    let self.initial_display = 1
    let self.fmt_display = {}
    let self.fmt_filter = {}
    call self.UseInputListScratch()
    call self.ResetSelected()
    call self.Retrieve(!initial)
    return self
endf


" :nodoc:
function! s:prototype.ResetSelected() dict "{{{3
    let self.sel_idx   = []
endf


" :nodoc:
function! s:prototype.Retrieve(anyway) dict "{{{3
    " TLogVAR a:anyway, self.base
    " TLogDBG (a:anyway || empty(self.base))
    if (a:anyway || empty(self.base))
        let ra = self.retrieve_eval
        " TLogVAR ra
        if !empty(ra)
            let back  = self.SwitchWindow('win')
            let world = self
            let self.base = eval(ra)
            " TLogVAR self.base
            exec back
            return 1
        endif
    endif
    return 0
endf


function! s:FormatHelp(help) "{{{3
    " TLogVAR a:help
    let max = [0, 0]
    for item in a:help
        " TLogVAR item
        if type(item) == 3
            let itemlen = map(copy(item), 'strwidth(v:val)')
            " TLogVAR itemlen
            let max = map(range(2), 'max[v:val] >= itemlen[v:val] ? max[v:val] : itemlen[v:val]')
        endif
        unlet item
    endfor
    " TLogVAR max
    let cols = float2nr((winwidth(0) - &foldcolumn - 1) / (max[0] + max[1] + 2))
    if cols < 1
        let cols = 1
    endif
    let fmt = printf('%%%ds: %%-%ds', max[0], max[1])
    " TLogVAR cols, fmt
    let help = []
    let idx = -1
    let maxidx = len(a:help)
    while idx < maxidx
        let push_item = 0
        let accum = []
        for i in range(cols)
            let idx += 1
            if idx >= maxidx
                break
            endif
            let item = a:help[idx]
            if type(item) == 3
                call add(accum, item)
            else
                let push_item = 1
                break
            endif
            unlet item
        endfor
        if !empty(accum)
            call add(help, s:FormatHelpItem(accum, fmt))
        endif
        if push_item
            call add(help, a:help[idx])
        endif
    endwh
    " TLogVAR help
    return help
endf


function! s:FormatHelpItem(item, fmt) "{{{3
    let args = [join(repeat([a:fmt], len(a:item)), '  ')]
    for item in a:item
        " TLogVAR item
        let args += item
    endfor
    " TLogVAR args
    return call('printf', args)
endf


" :nodoc:
function! s:prototype.InitHelp() dict "{{{3
    return []
endf


" :nodoc:
function! s:prototype.PushHelp(...) dict "{{{3
    " TLogVAR a:000
    if a:0 == 1
        if type(a:1) == 3
            let self.temp_lines += a:1
        else
            call add(self.temp_lines, a:1)
        endif
    elseif a:0 == 2
        call add(self.temp_lines, a:000)
    else
        throw "TLIB: PushHelp: Wrong number of arguments: ". string(a:000)
    endif
    " TLogVAR helpstring
endf


" :nodoc:
function! s:prototype.DisplayHelp() dict "{{{3
    let self.temp_lines = self.InitHelp()
    call self.PushHelp('<Esc>', self.key_mode == 'default' ? 'Abort' : 'Reset keymap')
    call self.PushHelp('Enter, <cr>', 'Pick the current item')
    call self.PushHelp('<M-Number>',  'Pick an item')
    call self.PushHelp('Mouse', 'L: Pick item, R: Show menu')
    call self.PushHelp('<BS>, <C-BS>', 'Reduce filter')
    call self.PushHelp('<S-Esc>, <F10>', 'Enter command')

    if self.key_mode == 'default'
        call self.PushHelp('<C|M-r>',     'Reset the display')
        call self.PushHelp('Up/Down',      'Next/previous item')
        call self.PushHelp('<C|M-q>',     'Edit top filter string')
        call self.PushHelp('Page Up/Down', 'Scroll')
        if self.allow_suspend
            call self.PushHelp('<C|M-z>', 'Suspend/Resume')
            call self.PushHelp('<C-o>', 'Switch to origin')
        endif
        if stridx(self.type, 'm') != -1
            call self.PushHelp('<S-Up/Down>', '(Un)Select items')
            call self.PushHelp('#, <C-Space>', '(Un)Select the current item')
            call self.PushHelp('<C|M-a>', '(Un)Select all items')
            call self.PushHelp('<F9>', '(Un)Restrict view to selection')
            " \ '<c-\>        ... Show only selected',
        endif
    endif

    " TLogVAR len(self.temp_lines)
    call self.matcher.Help(self)

    " TLogVAR self.key_mode
    for handler in values(self.key_map[self.key_mode])
        " TLogVAR handler
        let key = get(handler, 'key_name', '')
        " TLogVAR key
        if !empty(key)
            let desc = get(handler, 'help', '')
            if empty(desc)
                let desc = get(handler, 'agent', '')
            endif
            call self.PushHelp(key, desc)
        endif
    endfor

    if !has_key(self.key_map[self.key_mode], 'unknown_key')
        call self.PushHelp('Letter', 'Filter the list')
    endif

    if self.key_mode == 'default' && !empty(self.help_extra)
        call self.PushHelp(self.help_extra)
    endif

    " TLogVAR len(self.temp_lines)
    call self.PushHelp([
                \ '',
                \ 'Matches at word boundaries are prioritized.',
                \ ])
    let self.temp_lines = s:FormatHelp(self.temp_lines)
    call self.PrintLines()
endf


function! s:prototype.PrintLines() dict "{{{3
    let self.temp_prompt = ['Press any key to continue.', 'Question']
    call tlib#buffer#DeleteRange('1', '$')
    call append(0, self.temp_lines)
    call tlib#buffer#DeleteRange('$', '$')
    1
    call self.Resize(len(self.temp_lines), 0)
    let self.temp_lines = []
endf


" :nodoc:
function! s:prototype.Resize(hsize, vsize) dict "{{{3
    " TLogVAR self.scratch_vertical, a:hsize, a:vsize
    let world_resize = ''
    let winpos = ''
    let scratch_split = get(self, 'scratch_split', 1)
    " TLogVAR scratch_split
    if scratch_split > 0
        if self.scratch_vertical
            if a:vsize
                let world_resize = 'vert resize '. a:vsize
                let winpos = tlib#fixes#Winpos()
                " let w:winresize = {'v': a:vsize}
                setlocal winfixwidth
            endif
        else
            if a:hsize
                let world_resize = 'resize '. a:hsize
                " let w:winresize = {'h': a:hsize}
                setlocal winfixheight
            endif
        endif
    endif
    if !empty(world_resize)
        " TLogVAR world_resize, winpos
        exec world_resize
        if !empty(winpos)
            exec winpos
        endif
        " redraw!
    endif
endf


" :nodoc:
function! s:prototype.GetResize(size) dict "{{{3
    let resize0 = get(self, 'resize', 0)
    let resize = empty(resize0) ? 0 : eval(resize0)
    " TLogVAR resize0, resize
    let resize = resize == 0 ? a:size : min([a:size, resize])
    " let min = self.scratch_vertical ? &cols : &lines
    let min1 = (self.scratch_vertical ? self.win_width : self.win_height) * g:tlib_inputlist_pct
    let min2 = (self.scratch_vertical ? &columns : &lines) * self.win_pct
    let min = max([min1, min2])
    let resize = min([resize, (min / 100)])
    " TLogVAR resize, a:size, min, min1, min2
    return resize
endf


" function! s:prototype.DisplayList(?query=self.Query(), ?list=[])
" :nodoc:
function! s:prototype.DisplayList(...) dict "{{{3
    " TLogVAR self.state
    let query = a:0 >= 1 ? a:1 : self.Query()
    let list = a:0 >= 2 ? a:2 : []
    " TLogVAR query, len(list)
    " TLogDBG 'len(list) = '. len(list)
    call self.UseScratch()
    " TLogVAR self.scratch
    " TAssert IsNotEmpty(self.scratch)
    if self.state == 'scroll'
        call self.ScrollToOffset()
    elseif self.state == 'help'
        call self.DisplayHelp()
        call self.SetStatusline(query)
    elseif self.state == 'printlines'
        call self.PrintLines()
        call self.SetStatusline(query)
    else
        " TLogVAR query
        " let ll = len(list)
        let ll = self.llen
        " let x  = len(ll) + 1
        let x  = self.index_width + 1
        " TLogVAR ll
        if self.state =~ '\<display\>'
            call self.Resize(self.GetResize(ll), eval(get(self, 'resize_vertical', 0)))
            call tlib#normal#WithRegister('gg"tdG', 't')
            let w = winwidth(0) - &fdc
            " let w = winwidth(0) - &fdc - 1
            let lines = copy(list)
            let lines = map(lines, 'printf("%-'. w .'.'. w .'s", substitute(v:val, ''[[:cntrl:][:space:]]'', " ", "g"))')
            " TLogVAR lines
            call append(0, lines)
            call tlib#normal#WithRegister('G"tddgg', 't')
        endif
        " TLogVAR self.prefidx
        let base_pref = self.GetBaseIdx(self.prefidx)
        " TLogVAR base_pref
        if self.state =~ '\<redisplay\>'
            call filter(b:tlibDisplayListMarks, 'index(self.sel_idx, v:val) == -1 && v:val != base_pref')
            " TLogVAR b:tlibDisplayListMarks
            call map(b:tlibDisplayListMarks, 'self.DisplayListMark(x, v:val, ":")')
            " let b:tlibDisplayListMarks = map(copy(self.sel_idx), 'self.DisplayListMark(x, v:val, "#")')
            " call add(b:tlibDisplayListMarks, self.prefidx)
            " call self.DisplayListMark(x, self.GetBaseIdx(self.prefidx), '*')
        endif
        let b:tlibDisplayListMarks = map(copy(self.sel_idx), 'self.DisplayListMark(x, v:val, "#")')
        call add(b:tlibDisplayListMarks, base_pref)
        call self.DisplayListMark(x, base_pref, '*')
        call self.SetOffset()
        call self.SetStatusline(query)
        " TLogVAR self.offset
        call self.ScrollToOffset()
        let rx0 = self.GetRx0()
        " TLogVAR rx0
        if !empty(self.matcher.highlight)
            if empty(rx0)
                match none
            elseif self.IsValidFilter()
                if has_key(self, 'Highlighter')
                    call self.Highlighter(rx0)
                else
                    exec 'match '. self.matcher.highlight .' /\c'. escape(rx0, '/') .'/'
                endif
            endif
        endif
    endif
    redraw
endf


" :nodoc:
function! s:prototype.SetStatusline(query) dict "{{{3
    " TLogVAR a:query
    if !empty(self.temp_prompt)
        let echo = get(self.temp_prompt, 0, '')
        let hl = get(self.temp_prompt, 1, 'Normal')
        let self.temp_prompt = []
    else
        let hl = 'Normal'
        let query   = a:query
        let options = [self.matcher.name]
        if self.sticky
            call add(options, '#')
        endif
        if self.key_mode != 'default'
            call add(options, 'map:'. self.key_mode)
        endif
        if !empty(self.filtered_items)
            if g:tlib_inputlist_shortmessage
                call add(options, 'R')
            else
                call add(options, 'restricted')
            endif
        endif
        if !empty(options)
            let sopts = printf('[%s]', join(options, ', '))
            " let echo  = query . repeat(' ', &columns - len(sopts) - len(query) - 20) . sopts
            let echo  = query . '  ' . sopts
            " let query .= '%%='. sopts .' '
        endif
        " TLogVAR &l:statusline, query
        " let &l:statusline = query
    endif
    echo
    if hl != 'Normal'
        exec 'echohl' hl
        echo echo
        echohl None
    else
        echo echo
    endif
endf


" :nodoc:
function! s:prototype.Query() dict "{{{3
    if g:tlib_inputlist_shortmessage
        let query = 'Filter: '. self.DisplayFilter()
    else
        let query = self.query .' (filter: '. self.DisplayFilter() .'; press "?" for help)'
    endif
    return query
endf


" :nodoc:
function! s:prototype.ScrollToOffset() dict "{{{3
    " TLogVAR self.scratch_vertical, self.llen, winheight(0)
    exec 'norm! '. self.offset .'zt'
endf


" :nodoc:
function! s:prototype.SetOffset() dict "{{{3
    " TLogVAR self.prefidx, self.offset
    " TLogDBG winheight(0)
    " TLogDBG self.prefidx > self.offset + winheight(0) - 1
    let listtop = len(self.list) - winheight(0) + 1
    if listtop < 1
        let listtop = 1
    endif
    if self.prefidx > listtop
        let self.offset = listtop
    elseif self.prefidx > self.offset + winheight(0) - 1
        let listoff = self.prefidx - winheight(0) + 1
        let self.offset = min([listtop, listoff])
    "     TLogVAR self.prefidx
    "     TLogDBG len(self.list)
    "     TLogDBG winheight(0)
    "     TLogVAR listtop, listoff, self.offset
    elseif self.prefidx < self.offset
        let self.offset = self.prefidx
    endif
    " TLogVAR self.offset
endf


" :nodoc:
function! s:prototype.ClearAllMarks() dict "{{{3
    let x = self.index_width + 1
    call map(range(1, line('$')), 'self.DisplayListMark(x, v:val, ":")')
endf


" :nodoc:
function! s:prototype.MarkCurrent(y) dict "{{{3
    let x = self.index_width + 1
    call self.DisplayListMark(x, a:y, '*')
endf


" :nodoc:
function! s:prototype.DisplayListMark(x, y, mark) dict "{{{3
    " TLogVAR a:y, a:mark
    if a:x > 0 && a:y >= 0
        " TLogDBG a:x .'x'. a:y .' '. a:mark
        let sy = self.GetListIdx(a:y) + 1
        " TLogVAR sy
        if sy >= 1
            call setpos('.', [0, sy, a:x, 0])
            exec 'norm! r'. a:mark
            " exec 'norm! '. a:y .'gg'. a:x .'|r'. a:mark
        endif
    endif
    return a:y
endf


" :nodoc:
function! s:prototype.SwitchWindow(where) dict "{{{3
    " TLogDBG string(tlib#win#List())
    let wnr = get(self, a:where.'_wnr')
    " TLogVAR self, wnr
    return tlib#win#Set(wnr)
endf


" :nodoc:
function! s:prototype.FollowCursor() dict "{{{3
    if !empty(self.follow_cursor)
        let back = self.SwitchWindow('win')
        " TLogVAR back
        " TLogDBG winnr()
        try
            call call(self.follow_cursor, [self, [self.CurrentItem()]])
        finally
            exec back
        endtry
    endif
endf


" :nodoc:
function! s:prototype.SetOrigin(...) dict "{{{3
    TVarArg ['winview', 0]
    " TLogVAR self.win_wnr, self.bufnr
    " TLogDBG bufname('%')
    " TLogDBG winnr()
    " TLogDBG winnr('$')
    let self.win_wnr = winnr()
    let self.win_height = winheight(self.win_wnr)
    let self.win_width = winwidth(self.win_wnr)
    " TLogVAR self.win_wnr, self.win_height, self.win_width
    let self.bufnr   = bufnr('%')
    let self.tabpagenr = tabpagenr()
    let self.cursor  = getpos('.')
    if winview
        let self.winview = tlib#win#GetLayout()
    endif
    " TLogVAR self.win_wnr, self.bufnr, self.winview
    return self
endf


" :nodoc:
function! s:prototype.RestoreOrigin(...) dict "{{{3
    TVarArg ['winview', 0]
    if winview
        " TLogVAR winview
        call tlib#win#SetLayout(self.winview)
    endif
    " TLogVAR self.win_wnr, self.bufnr, self.cursor, &splitbelow
    " TLogDBG "RestoreOrigin0 ". string(tlib#win#List())
    " If &splitbelow or &splitright is false, we cannot rely on 
    " self.win_wnr to be our source buffer since, e.g, opening a buffer 
    " in a split window changes the whole layout.
    " Possible solutions:
    " - Restrict buffer switching to cases when the number of windows 
    "   hasn't changed.
    " - Guess the right window, which we try to do here.
    if &splitbelow == 0 || &splitright == 0
        let wn = bufwinnr(self.bufnr)
        " TLogVAR wn
        if wn == -1
            let wn = 1
        end
    else
        let wn = self.win_wnr
    endif
    if wn != winnr()
        exec wn .'wincmd w'
    endif
    exec 'buffer! '. self.bufnr
    call setpos('.', self.cursor)
    " TLogDBG "RestoreOrigin1 ". string(tlib#win#List())
endf

