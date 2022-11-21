" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Revision:    1482


" :filedoc:
" A prototype used by |tlib#input#List|.
" Inherits from |tlib#Object#New|.


" Size of the input list window (in percent) from the main size (of &lines).
" See |tlib#input#List()|.
TLet g:tlib_inputlist_pct = 50

" Max height for a horizontal list.
TLet g:tlib_inputlist_max_lines = -1

" Max width for a vertical list.
TLet g:tlib_inputlist_max_cols = -1

" Size of filename columns when listing filenames.
" See |tlib#input#List()|.
TLet g:tlib_inputlist_width_filename = '&columns / 3'
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
            \ 'resume_state': '', 
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
            \ 'tabpagenr': -1,
            \ 'type': '', 
            \ 'win_id': g:tlib#win#null_id,
            \ 'win_height': -1,
            \ 'win_width': -1,
            \ 'win_pct': 25,
            \ })
            " \ 'handlers': [],
            " \ 'filter_options': '\c',

function! tlib#World#New(...) abort
    let object = s:prototype.New(a:0 >= 1 ? a:1 : {})
    call object.SetMatchMode(tlib#var#Get('tlib#input#filter_mode', 'g', 'cnf'))
    return object
endf


" :nodoc:
function! s:prototype.Set_display_format(value) dict abort "{{{3
    if a:value ==# 'filename'
        call self.Set_highlight_filename()
        let self.display_format = 'world.FormatFilename(%s)'
    else
        let self.display_format = a:value
    endif
endf


" :nodoc:
function! s:prototype.DisplayFormat(list) dict abort "{{{3
    let display_format = self.display_format
    if !empty(display_format)
        if has_key(self, 'InitFormatName')
            call self.InitFormatName()
        endif
        let cache = self.fmt_display
        Tlibtrace 'tlib', display_format
        return map(copy(a:list), 'self.FormatName(cache, display_format, v:val)')
    else
        return a:list
    endif
endf


" :nodoc:
function! s:prototype.Set_highlight_filename() dict abort "{{{3
    let self.tlib_UseInputListScratch = 'call world.Highlight_filename()'
endf


if g:tlib#input#format_filename ==# 'r'

    " :nodoc:
    function! s:prototype.Highlight_filename() dict abort "{{{3
        syntax match TLibDir /\s\+\zs.\{-}[\/]\ze[^\/]\+$/
        hi def link TLibDir Directory
        syntax match TLibFilename /[^\/]\+$/
        hi def link TLibFilename Normal
    endf

    " :nodoc:
    function! s:prototype.FormatFilename(file) dict abort "{{{3
        if !has_key(self.fmt_options, 'maxlen')
            let maxco = &columns - len(len(self.base)) - eval(g:tlib#input#filename_padding_r)
            let maxfi = max(map(copy(self.base), 'strwidth(v:val)'))
            let self.fmt_options.maxlen = min([maxco, maxfi])
            Tlibtrace 'tlib', maxco, maxfi, self.fmt_options.maxlen
        endif
        let max = self.fmt_options.maxlen
        if len(a:file) > max
            let filename = '...' . tlib#string#Strcharpart(a:file, len(a:file) - max + 3)
        else
            let filename = printf('% '. max .'s', a:file)
        endif
        return filename
    endf

else

    " :nodoc:
    function! s:prototype.Highlight_filename() dict abort "{{{3
        " let self.width_filename = 1 + eval(g:tlib_inputlist_width_filename)
        Tlibtrace 'tlib', self.base
        let self.width_filename = min([
                    \ get(self, 'width_filename', &columns),
                    \ empty(g:tlib#input#filename_max_width) ? &columns : eval(g:tlib#input#filename_max_width),
                    \ max(map(copy(self.base), 'strwidth(matchstr(v:val, "[^\\/]*$"))'))
                    \ ])
       "  TLogVAR self.width_filename
         " exec 'syntax match TLibDir /\%>'. (1 + self.width_filename) .'c \(|\|\[[^]]*\]\) \zs\(\(\a:\|\.\.\|\.\.\..\{-}\)\?[\/][^&<>*|]\{-}\)\?[^\/]\+$/ contained containedin=TLibMarker contains=TLibFilename'
         exec 'syntax match TLibDir /\%>'. (1 + self.width_filename) .'c \(|\|\[[^]]*\]\) \zs[^&<>*|]*$/ contained containedin=TLibMarker contains=TLibFilename'
         exec 'syntax match TLibMarker /\%>'. (1 + self.width_filename) .'c \(|\|\[[^]]*\]\) \S.*$/ contains=TLibDir'
       "  exec 'syntax match TLibDir /\(|\|\[.\{-}\]\) \zs\(\(\a:\|\.\.\|\.\.\..\{-}\)\?[\/][^&<>*|]\{-}\)\?[^\/]\+$/ contained containedin=TLibMarker contains=TLibFilename'
       "  exec 'syntax match TLibMarker /\(|\|\[.\{-}\]\) \S.*$/ contains=TLibDir'
        exec 'syntax match TLibFilename /[^\/]\+$/ contained containedin=TLibDir'
        hi def link TLibMarker Special
        hi def link TLibDir Directory
        hi def link TLibFilename NonText
        " :nodoc:
        function! self.Highlighter(rx) dict abort
            let rx = '/\c\%>'. (1 + self.width_filename) .'c \(|\|\[[^]]*\]\) .\{-}\zs'. escape(a:rx, '/') .'/'
            exec 'match' self.matcher.highlight rx
        endf
    endf


    " :nodoc:
    function! s:prototype.UseFilenameIndicators() dict abort "{{{3
        return g:tlib_inputlist_filename_indicators || has_key(self, 'filename_indicators')
    endf


    " :nodoc:
    function! s:prototype.InitFormatName() dict abort "{{{3 
        if self.UseFilenameIndicators()
            let self._buffers = {}
            for bufnr in range(1, bufnr('$'))
                let filename = fnamemodify(bufname(bufnr), ':p')
                Tlibtrace 'tlib', filename
                let bufdef = {
                            \ 'bufnr': bufnr,
                            \ }
                " '&buflisted'
                for opt in ['&modified', '&bufhidden']
                    let bufdef[opt] = getbufvar(bufnr, opt)
                endfor
                let self._buffers[filename] = bufdef
            endfor
        endif
    endf


    " :nodoc:
    function! s:prototype.FormatFilename(file) dict abort "{{{3
        Tlibtrace 'tlib', a:file
        let width = self.width_filename
        let split = match(a:file, '[/\\]\zs[^/\\]\+$')
        if split == -1
            let fname = a:file
            let dname = a:file
        else
            let fname = strpart(a:file, split)
            " let dname = tlib#string#Strcharpart(a:file, 0, split - 1)
            let dname = a:file
        endif
        if strwidth(fname) > width
            let fname = tlib#string#Strcharpart(fname, 0, width - 3) .'...'
        endif
        let dnmax = &columns - max([width, strwidth(fname)]) - 8 - self.index_width - &foldcolumn
        let use_indicators = self.UseFilenameIndicators()
        Tlibtrace 'tlib', use_indicators
        let marker = []
        if use_indicators
            call insert(marker, '[')
            if g:tlib_inputlist_filename_indicators
                let bufdef = get(self._buffers, a:file, {})
                " let bnr = bufnr(a:file)
                let bnr = get(bufdef, 'bufnr', -1)
                Tlibtrace 'tlib', a:file, bnr, self.bufnr
                if bnr != -1
                    if bnr == self.bufnr
                        call add(marker, '%')
                    else
                        call add(marker, bnr)
                    endif
                    if get(bufdef, '&modified', 0)
                        call add(marker, '+')
                    endif
                    if get(bufdef, '&bufhidden', '') ==# 'hide'
                        call add(marker, 'h')
                    endif
                    " if !get(bufdef, '&buflisted', 1)
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
        let markers = join(marker, '')
        if !empty(markers)
            let dnmax -= len(markers)
        endif
        if strwidth(dname) > dnmax
            let dname = '...'. tlib#string#Strcharpart(dname, len(dname) - dnmax)
        endif
        return printf('%-*s %s %s',
                    \ self.width_filename + len(fname) - strwidth(fname),
                    \ fname, markers, dname)
    endf

endif


" :nodoc:
function! s:prototype.GetSelectedItems(current) dict abort "{{{3
    Tlibtrace 'tlib', a:current
    if stridx(self.type, 'i') != -1
        let rv = copy(self.sel_idx)
    else
        let rv = map(copy(self.sel_idx), 'self.GetBaseItem(v:val)')
    endif
    if !empty(a:current)
        Tlibtrace 'tlib', a:current, rv, type(a:current)
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
            Tlibtrace 'tlib', rv, self.index_table
            call map(rv, 'self.index_table[v:val - 1]')
            Tlibtrace 'tlib', rv
        endif
    endif
    return rv
endf


function! s:InsertSelectedItems(rv, current) abort "{{{3
    let ci = index(a:rv, a:current)
    if ci != -1
        call remove(a:rv, ci)
    endif
    call insert(a:rv, a:current)
endf


" :nodoc:
function! s:prototype.SelectItemsByNames(mode, items) dict abort "{{{3
    for item in a:items
        let bi = index(self.base, item) + 1
        Tlibtrace 'tlib', item, bi
        if bi > 0
            let si = index(self.sel_idx, bi)
            Tlibtrace 'tlib', self.sel_idx
            Tlibtrace 'tlib', si
            if si == -1
                call add(self.sel_idx, bi)
            elseif a:mode ==# 'toggle'
                call remove(self.sel_idx, si)
            endif
        endif
    endfor
    return 1
endf


" :nodoc:
function! s:prototype.SelectItem(mode, index) dict abort "{{{3
    Tlibtrace 'tlib', a:mode, a:index
    let bi = self.GetBaseIdx(a:index)
    " if self.RespondTo('MaySelectItem')
    "     if !self.MaySelectItem(bi)
    "         return 0
    "     endif
    " endif
    Tlibtrace 'tlib', bi
    let si = index(self.sel_idx, bi)
    Tlibtrace 'tlib', self.sel_idx
    Tlibtrace 'tlib', si
    if si == -1
        call add(self.sel_idx, bi)
    elseif a:mode ==# 'toggle'
        call remove(self.sel_idx, si)
    endif
    return 1
endf


" :nodoc:
function! s:prototype.FormatBaseFromData() abort dict "{{{3
    if has_key(self, 'format_data') && has_key(self, 'data')
        let self.base = map(copy(self.data), 'call(self.format_data, [v:val], self)')
    endif    
endf


" :nodoc:
function! s:prototype.FormatArgs(format_string, arg) dict abort "{{{3
    let nargs = len(substitute(a:format_string, '%%\|[^%]', '', 'g'))
    return [a:format_string] + repeat([string(a:arg)], nargs)
endf


" :nodoc:
function! s:prototype.GetRx(filter) dict abort "{{{3
    return '\('. join(filter(copy(a:filter), 'v:val[0] !=# "!"'), '\|') .'\)' 
endf


" :nodoc:
function! s:prototype.GetRx0(...) dict abort "{{{3
    exec tlib#arg#Let(['negative'])
    let rx0 = []
    for filter in self.filter
        Tlibtrace 'tlib', filter
        let rx = join(reverse(filter(copy(filter), '!empty(v:val)')), '\|')
        Tlibtrace 'tlib', rx
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
function! s:prototype.FormatName(cache, format, value) dict abort "{{{3
    Tlibtrace 'tlib', a:format, a:value
    if has_key(a:cache, a:value)
        return a:cache[a:value]
    else
        let world = self
        let ftpl = self.FormatArgs(a:format, a:value)
        let fn = call(function('printf'), ftpl)
        let fmt = eval(fn)
        Tlibtrace 'tlib', ftpl, fn, fmt
        let a:cache[a:value] = fmt
        return fmt
    endif
endf


" :nodoc:
function! s:prototype.GetItem(idx) dict abort "{{{3
    return self.list[a:idx - 1]
endf


" :nodoc:
function! s:prototype.GetListIdx(baseidx) dict abort "{{{3
    " if empty(self.index_table)
        let baseidx = a:baseidx
    " else
    "     let baseidx = 0 + self.index_table[a:baseidx - 1]
    "     Tlibtrace 'tlib', a:baseidx, baseidx, self.index_table 
    " endif
    let rv = index(self.table, baseidx)
    Tlibtrace 'tlib', rv, self.table
    return rv
endf


" :nodoc:
" The first index is 1.
function! s:prototype.GetBaseIdx(idx) dict abort "{{{3
    Tlibtrace 'tlib', a:idx, self.table, self.index_table
    if !empty(self.table) && a:idx > 0 && a:idx <= len(self.table)
        return self.table[a:idx - 1]
    else
        return 0
    endif
endf


" :nodoc:
function! s:prototype.GetBaseIdx0(idx) dict abort "{{{3
    let idx0 = self.GetBaseIdx(a:idx) - 1
    if idx0 < 0
        call tlib#notify#Echo('TLIB: Internal Error: GetBaseIdx0: idx0 < 0', 'WarningMsg')
    endif
    return idx0
endf


" :nodoc:
function! s:prototype.GetBaseItem(idx) dict abort "{{{3
    return self.base[a:idx - 1]
endf


" :nodoc:
function! s:prototype.SetBaseItem(idx, item) dict abort "{{{3
    let self.base[a:idx - 1] = a:item
endf


" :nodoc:
function! s:prototype.GetLineIdx(lnum) dict abort "{{{3
    let line = getline(a:lnum)
    let prefidx = substitute(matchstr(line, '^\d\+\ze[*:]'), '^0\+', '', '')
    return prefidx
endf


" :nodoc:
function! s:prototype.SetPrefIdx() dict abort "{{{3
    " let pref = sort(range(1, self.llen), 'self.SortPrefs')
    " let self.prefidx = get(pref, 0, self.initial_index)
    let pref_idx = -1
    let pref_weight = -1
    Tlibtrace 'tlib', self.filter_pos, self.filter_neg
    let t0 = localtime()
    for idx in range(1, self.llen)
        let item = self.GetItem(idx)
        let weight = self.matcher.AssessName(self, item)
        Tlibtrace 'tlib', item, weight
        if weight > pref_weight
            let pref_idx = idx
            let pref_weight = weight
        endif
    endfor
    Tlibtrace 'tlib', localtime() - t0
    Tlibtrace 'tlib', pref_idx
    if pref_idx == -1
        let self.prefidx = self.initial_index
    else
        let self.prefidx = pref_idx
    endif
endf


" " :nodoc:
" function! s:prototype.GetCurrentItem() dict abort "{{{3
"     let idx = self.prefidx
"     Tlibtrace 'tlib', idx
"     if stridx(self.type, 'i') != -1
"         return idx
"     elseif !empty(self.list)
"         if len(self.list) >= idx
"             let idx1 = idx - 1
"             let rv = self.list[idx - 1]
"             Tlibtrace 'tlib', idx, idx1, rv, self.list
"             return rv
"         endif
"     else
"         return ''
"     endif
" endf


" :nodoc:
function! s:prototype.CurrentItem() dict abort "{{{3
    if stridx(self.type, 'i') != -1
        return self.GetBaseIdx(self.llen == 1 ? 1 : self.prefidx)
    else
        if self.llen == 1
            Tlibtrace 'tlib', self.llen
            return self.list[0]
        elseif self.prefidx > 0
            Tlibtrace 'tlib', self.prefidx
            " return self.GetCurrentItem()
            if len(self.list) >= self.prefidx
                let rv = self.list[self.prefidx - 1]
                Tlibtrace 'tlib', self.prefidx, len(self.list), rv
                return rv
            endif
        else
            return ''
        endif
    endif
endf


" :nodoc:
function! s:prototype.FilterRxPrefix() dict abort "{{{3
    return self.matcher.FilterRxPrefix()
endf


" :nodoc:
function! s:prototype.SetFilter() dict abort "{{{3
    " let mrx = '\V'. (a:0 >= 1 && a:1 ? '\C' : '')
    let mrx = self.FilterRxPrefix() . self.filter_options
    let self.filter_pos = []
    let self.filter_neg = []
    Tlibtrace 'tlib', mrx, self.filter
    for filter in self.filter
        Tlibtrace 'tlib', filter
        let rx = join(reverse(filter(copy(filter), '!empty(v:val)')), '\|')
        Tlibtrace 'tlib', rx
        if !empty(rx)
            if rx =~# '\u'
                let mrx1 = mrx .'\C'
            else
                let mrx1 = mrx
            endif
            Tlibtrace 'tlib', rx
            if rx[0] == g:tlib#input#not
                if len(rx) > 1
                    call add(self.filter_neg, mrx1 .'\('. rx[1:-1] .'\)')
                endif
            else
                call add(self.filter_pos, mrx1 .'\('. rx .'\)')
            endif
        endif
    endfor
    Tlibtrace 'tlib', self.filter_pos, self.filter_neg
endf


" :nodoc:
function! s:prototype.IsValidFilter() dict abort "{{{3
    let last = self.FilterRxPrefix() .'\('. self.filter[0][0] .'\)'
    Tlibtrace 'tlib', last
    Tlibtrace 'tlib', last
    try
        let a = match('', last)
        return 1
    catch
        Tlibtrace 'tlib', v:exception
        return 0
    endtry
endf


" :nodoc:
function! s:prototype.SetMatchMode(match_mode) dict abort "{{{3
    Tlibtrace 'tlib', a:match_mode
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


" function! s:prototype.Match(text) dict abort "{{{3
"     return self.matcher.Match(self, text)
" endf


" :nodoc:
function! s:prototype.MatchBaseIdx(idx) dict abort "{{{3
    let text = self.GetBaseItem(a:idx)
    if !empty(self.filter_format)
        let text = self.FormatName(self.fmt_filter, self.filter_format, text)
    endif
    Tlibtrace 'tlib', text
    " return self.Match(text)
    return self.matcher.Match(self, text)
endf


" :nodoc:
function! s:prototype.BuildTableList() dict abort "{{{3
    let time0 = str2float(reltimestr(reltime()))
    Tlibtrace 'tlib', time0
    call self.SetFilter()
    Tlibtrace 'tlib', self.filter_neg, self.filter_pos
    let self.table = range(1, len(self.base))
    Tlibtrace 'tlib', self.filtered_items
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
function! s:prototype.ReduceFilter() dict abort "{{{3
    Tlibtrace 'tlib', self.filter
    let reduced = 0
    while !reduced
        if self.filter[0] == [''] && len(self.filter) > 1
            call remove(self.filter, 0)
        elseif empty(self.filter[0][0]) && len(self.filter[0]) > 1
            call remove(self.filter[0], 0)
        else
            call self.matcher.ReduceFrontFilter(self)
        endif
        if self.IsValidFilter()
            let reduced = 1
        endif
    endwh
endf


" :nodoc:
" filter is either a string or a list of list of strings.
function! s:prototype.SetInitialFilter(filter) dict abort "{{{3
    " let self.initial_filter = [[''], [a:filter]]
    Tlibtrace 'tlib', a:filter
    if type(a:filter) == 3
        let self.initial_filter = deepcopy(a:filter)
    else
        let self.initial_filter = [[a:filter]]
    endif
endf


" :nodoc:
function! s:prototype.PopFilter() dict abort "{{{3
    Tlibtrace 'tlib', self.filter
    if len(self.filter[0]) > 1
        call remove(self.filter[0], 0)
    elseif len(self.filter) > 1
        call remove(self.filter, 0)
    else
        let self.filter[0] = ['']
    endif
endf


" :nodoc:
function! s:prototype.FilterIsEmpty() dict abort "{{{3
    Tlibtrace 'tlib', self.filter
    return self.filter == copy(self.initial_filter)
endf


" :nodoc:
function! s:prototype.DisplayFilter() dict abort "{{{3
    let filter1 = copy(self.filter)
    call filter(filter1, 'v:val != [""]')
    Tlibtrace 'tlib', self.matcher['_class']
    let rv = self.matcher.DisplayFilter(filter1)
    let rv = self.CleanFilter(rv)
    return rv
endf


" :nodoc:
function! s:prototype.SetFrontFilter(pattern) dict abort "{{{3
    call self.matcher.SetFrontFilter(self, a:pattern)
endf


" :nodoc:
function! s:prototype.PushFrontFilter(char) dict abort "{{{3
    call self.matcher.PushFrontFilter(self, a:char)
endf


" :nodoc:
function! s:prototype.CleanFilter(filter) dict abort "{{{3
    return self.matcher.CleanFilter(a:filter)
endf


" :nodoc:
function! s:prototype.UseScratch() dict abort "{{{3
    " if type(self.scratch) != 0 && get(self, 'buffer_local', 1)
    "     if self.scratch != fnamemodify(self.scratch, ':p')
    "         let self.scratch = tlib#file#Join([expand('%:p:h'), self.scratch])
    "         Tlibtrace 'tlib', self.scratch
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
function! s:prototype.CloseScratch(...) dict abort "{{{3
    TVarArg ['reset_scratch', 0]
    " TVarArg ['reset_scratch', 1]
    Tlibtrace 'tlib', reset_scratch
    if self.sticky
        return 0
    else
        let rv = tlib#scratch#CloseScratch(self, reset_scratch)
        Tlibtrace 'tlib', rv
        if rv
            call self.SwitchWindow('win')
        endif
        return rv
    endif
endf


" :nodoc:
function! s:prototype.Initialize() dict abort "{{{3
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
function! s:prototype.Leave() dict abort "{{{3
    if !empty(self.cache_var)
        exec 'let '. self.cache_var .' = self'
    endif
    for handler in self.on_leave
        call call(handler, [self])
    endfor
endf


" :nodoc:
function! s:prototype.UseInputListScratch() dict abort "{{{3
    let scratch = self.UseScratch()
    if !exists('b:tlib_list_init')
        call tlib#autocmdgroup#Init()
        autocmd TLib VimResized <buffer> call feedkeys("\<c-j>", 't')
        " autocmd TLib WinLeave <buffer> let b:tlib_world_event = 'WinLeave' | call feedkeys("\<c-j>", 't')
        let b:tlib_list_init = 1
    endif
    if !exists('w:tlib_list_init')
        Tlibtrace 'tlib', scratch
        if has_key(self, 'index_next_syntax')
            if type(self.index_next_syntax) == 1
                exec 'syntax match InputlListIndex /^\d\+:\s/ nextgroup='. self.index_next_syntax
            elseif type(self.index_next_syntax) == 4
                for [n, nsyn] in items(self.index_next_syntax)
                    let fn = printf('%0'. world.index_width .'d', n)
                    exec 'syntax match InputlListIndex /^'. fn .':\s/ nextgroup='. nsyn
                endfor
            endif
        else
            syntax match InputlListIndex /^\d\+:\s/
        endif
        call tlib#hook#Run('tlib_UseInputListScratch', self)
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
        let w:tlib_list_init = 1
    endif
    return scratch
endf


" s:prototype.Reset(?initial=0)
" :nodoc:
function! s:prototype.Reset(...) dict abort "{{{3
    TVarArg ['initial', 0]
    Tlibtrace 'tlib', initial
    Tlibtrace 'tlib', initial, self.initial_filter
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
    call self.FormatBaseFromData()
    return self
endf


" :nodoc:
function! s:prototype.ResetSelected() dict abort "{{{3
    let self.sel_idx   = []
endf


" :nodoc:
function! s:prototype.Retrieve(anyway) dict abort "{{{3
    Tlibtrace 'tlib', a:anyway, self.base
    if (a:anyway || empty(self.base))
        let ra = self.retrieve_eval
        Tlibtrace 'tlib', ra
        if !empty(ra)
            let back  = self.SwitchWindow('win')
            let world = self
            let self.base = eval(ra)
            Tlibtrace 'tlib', self.base
            exec back
            return 1
        endif
    endif
    return 0
endf


function! s:FormatHelp(help) abort "{{{3
    Tlibtrace 'tlib', a:help
    let max = [0, 0]
    for item in a:help
        Tlibtrace 'tlib', item
        if type(item) == 3
            let itemlen = map(copy(item), 'strwidth(v:val)')
            Tlibtrace 'tlib', itemlen
            let max = map(range(2), 'max[v:val] >= itemlen[v:val] ? max[v:val] : itemlen[v:val]')
        endif
        unlet item
    endfor
    Tlibtrace 'tlib', max
    let cols = float2nr((winwidth(0) - &foldcolumn - 1) / (max[0] + max[1] + 2))
    if cols < 1
        let cols = 1
    endif
    let fmt = printf('%%%ds: %%-%ds', max[0], max[1])
    Tlibtrace 'tlib', cols, fmt
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
    Tlibtrace 'tlib', help
    return help
endf


function! s:FormatHelpItem(item, fmt) abort "{{{3
    let args = [join(repeat([a:fmt], len(a:item)), '  ')]
    for item in a:item
        Tlibtrace 'tlib', item
        let args += item
    endfor
    Tlibtrace 'tlib', args
    return call('printf', args)
endf


" :nodoc:
function! s:prototype.InitHelp() dict abort "{{{3
    return []
endf


" :nodoc:
function! s:prototype.PushHelp(...) dict abort "{{{3
    Tlibtrace 'tlib', a:000
    if a:0 == 1
        if type(a:1) == 3
            let self.temp_lines += a:1
        else
            call add(self.temp_lines, a:1)
        endif
    elseif a:0 == 2
        call add(self.temp_lines, a:000)
    else
        throw 'TLIB: PushHelp: Wrong number of arguments: '. string(a:000)
    endif
    Tlibtrace 'tlib', helpstring
endf


" :nodoc:
function! s:prototype.DisplayHelp() dict abort "{{{3
    let self.temp_lines = self.InitHelp()
    call self.PushHelp('<Esc>', self.key_mode == 'default' ? 'Abort' : 'Reset keymap')
    call self.PushHelp('Enter, <cr>', 'Pick the current item')
    call self.PushHelp('Mouse', 'L: Pick item, R: Show menu')
    call self.PushHelp('<M-Number>',  'Select an item')
    call self.PushHelp('<BS>, <C-BS>', 'Reduce filter')
    call self.PushHelp('<Tab>', 'Complete word')
    call self.PushHelp('<S-Esc>, <F10>', 'Enter command')

    if self.key_mode == 'default'
        call self.PushHelp('<C|M-r>',      'Reset the display')
        call self.PushHelp('Up/Down',      'Next/previous item')
        call self.PushHelp('<C|M-q>',      'Edit top filter string')
        call self.PushHelp('Page Up/Down', 'Scroll')
        call self.PushHelp('<S-Space>',    'Enter * Wildcard')
        if self.allow_suspend
            call self.PushHelp('<C|M-z>', 'Suspend/Resume')
            call self.PushHelp('<C-o>', 'Switch to origin')
        endif
        if stridx(self.type, 'm') != -1
            call self.PushHelp('<S-Up/Down>', '(Un)Select items')
            call self.PushHelp('#', '(Un)Select the current item')
            call self.PushHelp('<C|M-a>', '(Un)Select all items')
            call self.PushHelp('<F9>', '(Un)Restrict view to selection')
            " \ '<c-\>        ... Show only selected',
        endif
    endif

    Tlibtrace 'tlib', len(self.temp_lines)
    call self.matcher.Help(self)

    Tlibtrace 'tlib', self.key_mode
    for handler in values(self.key_map[self.key_mode])
        Tlibtrace 'tlib', handler
        let key = get(handler, 'key_name', '')
        Tlibtrace 'tlib', key
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

    Tlibtrace 'tlib', len(self.temp_lines)
    call self.PushHelp([
                \ '',
                \ 'Matches at word boundaries are prioritized.',
                \ ])
    let self.temp_lines = s:FormatHelp(self.temp_lines)
    call self.PrintLines()
endf


function! s:prototype.PrintLines() dict abort "{{{3
    let self.temp_prompt = ['Press any key to continue.', 'Question']
    call tlib#buffer#DeleteRange('1', '$')
    call append(0, self.temp_lines)
    call tlib#buffer#DeleteRange('$', '$')
    1
    call self.Resize(len(self.temp_lines), 0)
    let self.temp_lines = []
endf


" :nodoc:
function! s:prototype.Resize(hsize, vsize) dict abort "{{{3
    Tlibtrace 'tlib', self.scratch_vertical, a:hsize, a:vsize
    let world_resize = ''
    let winpos = ''
    let scratch_split = get(self, 'scratch_split', 1)
    Tlibtrace 'tlib', scratch_split
    if scratch_split > 0
        if self.scratch_vertical
            if a:vsize
                let world_resize = 'vert resize '. a:vsize
                let winpos = tlib#fixes#Winpos()
                " let w:winresize = {'v': a:vsize}
                " setlocal winfixwidth
            endif
        else
            if a:hsize
                let world_resize = 'resize '. a:hsize
                " let w:winresize = {'h': a:hsize}
                " setlocal winfixheight
            endif
        endif
    endif
    if !empty(world_resize)
        Tlibtrace 'tlib', world_resize, winpos
        setlocal nowinfixheight
        setlocal nowinfixwidth
        exec world_resize
        setlocal winfixheight
        setlocal winfixwidth
        if !empty(winpos)
            exec winpos
        endif
        " redraw!
    endif
endf


" :nodoc:
function! s:prototype.GetResize(size) dict abort "{{{3
    let resize0 = get(self, 'resize', 0)
    let resize = empty(resize0) ? 0 : eval(resize0)
    Tlibtrace 'tlib', resize0, resize
    let resize = resize == 0 ? a:size : min([a:size, resize])
    " let min = self.scratch_vertical ? &cols : &lines
    let min1 = (self.scratch_vertical ? self.win_width : self.win_height) * g:tlib_inputlist_pct
    let min2 = (self.scratch_vertical ? &columns : &lines) * self.win_pct
    let min3 = &previewheight
    let min = max([min1, min2])
    let ns = [resize, (min / 100)]
    let maxn = self.scratch_vertical ? g:tlib_inputlist_max_cols : g:tlib_inputlist_max_lines
    if maxn > 0
        call add(ns, maxn)
    endif
    let resize = min(ns)
    Tlibtrace 'tlib', resize, a:size, min, min1, min2
    return resize
endf


" function! s:prototype.DisplayList(?query=self.Query(), ?list=[])
" :nodoc:
function! s:prototype.DisplayList(...) dict abort "{{{3
    Tlibtrace 'tlib', self.state
    let query = a:0 >= 1 ? a:1 : self.Query()
    let list = a:0 >= 2 ? a:2 : []
    Tlibtrace 'tlib', query, len(list)
    call self.UseScratch()
    Tlibtrace 'tlib', self.scratch
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
        Tlibtrace 'tlib', query
        " let ll = len(list)
        let ll = self.llen
        " let x  = len(ll) + 1
        let x  = self.index_width + 1
        Tlibtrace 'tlib', ll
        if self.state =~ '\<display\>'
            call self.Resize(self.GetResize(ll), eval(get(self, 'resize_vertical', 0)))
            call tlib#normal#WithRegister('gg"tdG', 't')
            let lines = copy(list)
            let lines = map(lines, 'substitute(v:val, ''[[:cntrl:][:space:]]'', " ", "g")')
            let w = winwidth(0) - &fdc
            " let w = winwidth(0) - &fdc - 1
            let lines = map(lines, 'printf("%-'. w .'.'. w .'S", v:val)')
            Tlibtrace 'tlib', lines
            call append(0, lines)
            call tlib#normal#WithRegister('G"tddgg', 't')
        endif
        Tlibtrace 'tlib', self.prefidx
        let base_pref = self.GetBaseIdx(self.prefidx)
        Tlibtrace 'tlib', base_pref
        if self.state =~ '\<redisplay\>'
            call filter(b:tlibDisplayListMarks, 'index(self.sel_idx, v:val) == -1 && v:val != base_pref')
            Tlibtrace 'tlib', b:tlibDisplayListMarks
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
        Tlibtrace 'tlib', self.offset
        call self.ScrollToOffset()
        let rx0 = self.GetRx0()
        Tlibtrace 'tlib', rx0
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
function! s:prototype.SetStatusline(query) dict abort "{{{3
    Tlibtrace 'tlib', a:query
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
        Tlibtrace 'tlib', &l:statusline, query
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
function! s:prototype.Query() dict abort "{{{3
    let flt = self.DisplayFilter()
    if g:tlib_inputlist_shortmessage
        let query = 'Filter: '. flt
    else
        let query = self.query .' (filter: '. flt .'; press <F1> for help)'
    endif
    return query
endf


" :nodoc:
function! s:prototype.ScrollToOffset() dict abort "{{{3
    Tlibtrace 'tlib', self.scratch_vertical, self.llen, winheight(0)
    exec 'norm! '. self.offset .'zt'
endf


" :nodoc:
function! s:prototype.SetOffset() dict abort "{{{3
    Tlibtrace 'tlib', self.prefidx, self.offset
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
    "     TLogVAR listtop, listoff, self.offset
    elseif self.prefidx < self.offset
        let self.offset = self.prefidx
    endif
    Tlibtrace 'tlib', self.offset
endf


" :nodoc:
function! s:prototype.ClearAllMarks() dict abort "{{{3
    let x = self.index_width + 1
    call map(range(1, line('$')), 'self.DisplayListMark(x, v:val, ":")')
endf


" :nodoc:
function! s:prototype.MarkCurrent(y) dict abort "{{{3
    let x = self.index_width + 1
    call self.DisplayListMark(x, a:y, '*')
endf


" :nodoc:
function! s:prototype.DisplayListMark(x, y, mark) dict abort "{{{3
    Tlibtrace 'tlib', a:y, a:mark
    if a:x > 0 && a:y >= 0
        let sy = self.GetListIdx(a:y) + 1
        Tlibtrace 'tlib', sy
        if sy >= 1
            call setpos('.', [0, sy, a:x, 0])
            exec 'norm! r'. a:mark
            " exec 'norm! '. a:y .'gg'. a:x .'|r'. a:mark
        endif
    endif
    return a:y
endf


" :nodoc:
function! s:prototype.SwitchWindow(where) dict abort "{{{3
    " if self.tabpagenr != tabpagenr()
    "     call tlib#tab#Set(self.tabpagenr)
    " endif
    " let wnr = get(self, a:where.'_wnr')
    " Tlibtrace 'tlib', self, wnr
    " return tlib#win#Set(wnr)
    return tlib#win#SetById(self[a:where .'_id'])
endf


" :nodoc:
function! s:prototype.FollowCursor() dict abort "{{{3
    if !empty(self.follow_cursor)
        let back = self.SwitchWindow('win')
        Tlibtrace 'tlib', back
        try
            call call(self.follow_cursor, [self, [self.CurrentItem()]])
        finally
            exec back
        endtry
    endif
endf


" :nodoc:
function! s:prototype.SetOrigin(...) dict abort "{{{3
    TVarArg ['winview', 0]
    Tlibtrace 'tlib', 'SetOrigin', self.win_id, self.bufnr, bufnr('%'), winnr()
    let self.win_wnr = winnr()
    let self.win_id = tlib#win#GetID()
    let self.win_height = winheight(self.win_wnr)
    let self.win_width = winwidth(self.win_wnr)
    Tlibtrace 'tlib', 'SetOrigin', self.win_id, self.win_height, self.win_width, bufnr('%'), winnr()
    let self.bufnr   = bufnr('%')
    let self.tabpagenr = tabpagenr()
    let self.cursor  = getpos('.')
    if winview
        let self.winview = tlib#win#GetLayout()
    endif
    Tlibtrace 'tlib', 'SetOrigin', self.win_id, self.bufnr, get(self,'winview','')
    return self
endf


" :nodoc:
function! s:prototype.RestoreWindow(...) dict abort "{{{3
    TVarArg ['winview', 0]
    if winview
        Tlibtrace 'tlib', winview
        call tlib#win#SetLayout(self.winview)
    endif
    call tlib#win#GotoID(self.win_id)
endf


" :nodoc:
function! s:prototype.RestoreOrigin(...) dict abort "{{{3
    call call(self.RestoreWindow, a:000)
    if bufnr('%') != self.bufnr
        exec 'buffer! '. self.bufnr
        call setpos('.', self.cursor)
    endif
endf


function! s:prototype.Suspend() dict abort "{{{3
    call tlib#agent#Suspend(self, self.rv)
endf

