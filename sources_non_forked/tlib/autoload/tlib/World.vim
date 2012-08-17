" World.vim -- The World prototype for tlib#input#List()
" @Author:      Tom Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-05-01.
" @Last Change: 2011-04-01.
" @Revision:    0.1.915

" :filedoc:
" A prototype used by |tlib#input#List|.
" Inherits from |tlib#Object#New|.


" Known keys & values:
"   scratch_split ... See |tlib#scratch#UseScratch()|
let s:prototype = tlib#Object#New({
            \ '_class': 'World',
            \ 'name': 'world',
            \ 'allow_suspend': 1,
            \ 'base': [], 
            \ 'bufnr': -1,
            \ 'cache_var': '',
            \ 'display_format': '',
            \ 'fileencoding': &fileencoding,
            \ 'fmt_display': {},
            \ 'fmt_filter': {},
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
            \ 'next_state': '',
            \ 'numeric_chars': tlib#var#Get('tlib_numeric_chars', 'bg'),
            \ 'offset': 1,
            \ 'offset_horizontal': 0,
            \ 'on_leave': [],
            \ 'pick_last_item': tlib#var#Get('tlib_pick_last_item', 'bg'),
            \ 'post_handlers': [],
            \ 'query': '',
            \ 'resize': 0,
            \ 'resize_vertical': 0,
            \ 'restore_from_cache': [],
            \ 'retrieve_eval': '',
            \ 'return_agent': '',
            \ 'rv': '',
            \ 'scratch': '__InputList__',
            \ 'scratch_filetype': 'tlibInputList',
            \ 'scratch_vertical': 0,
            \ 'scratch_split': 1,
            \ 'sel_idx': [],
            \ 'show_empty': 0,
            \ 'state': 'display', 
            \ 'state_handlers': [],
            \ 'sticky': 0,
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
    call object.SetMatchMode(tlib#var#Get('tlib_inputlist_match', 'g', 'cnf'))
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
    "             \ 'syntax match TLibMarker /\%>'. (1 + eval(g:tlib_inputlist_width_filename)) .'c |.\{-}| / | hi def link TLibMarker Special'
    " let self.tlib_UseInputListScratch .= '| syntax match TLibDir /\%>'. (4 + eval(g:tlib_inputlist_width_filename)) .'c\S\{-}[\/].*$/ | hi def link TLibDir Directory'
endf


" :nodoc:
function! s:prototype.Highlight_filename() dict "{{{3
    " exec 'syntax match TLibDir /\%>'. (3 + eval(g:tlib_inputlist_width_filename)) .'c \(\S:\)\?[\/].*$/ contained containedin=TLibMarker'
    exec 'syntax match TLibDir /\(\a:\|\.\.\..\{-}\)\?[\/][^&<>*|]*$/ contained containedin=TLibMarker'
    exec 'syntax match TLibMarker /\%>'. (1 + eval(g:tlib_inputlist_width_filename)) .'c |\( \|[[:alnum:]%*+-]*\)| \S.*$/ contains=TLibDir'
    hi def link TLibMarker Special
    hi def link TLibDir Directory
endf


" :nodoc:
function! s:prototype.FormatFilename(file) dict "{{{3
    let width = eval(g:tlib_inputlist_width_filename)
    let split = match(a:file, '[/\\]\zs[^/\\]\+$')
    if split == -1
        let fname = ''
        let dname = a:file
    else
        let fname = strpart(a:file, split)
        let dname = strpart(a:file, 0, split - 1)
    endif
    " let fname = fnamemodify(a:file, ":p:t")
    " " let fname = fnamemodify(a:file, ":t")
    " " if isdirectory(a:file)
    " "     let fname .='/'
    " " endif
    " let dname = fnamemodify(a:file, ":h")
    " let dname = pathshorten(fnamemodify(a:file, ":h"))
    let dnmax = &co - max([width, len(fname)]) - 11 - self.index_width - &fdc
    if len(dname) > dnmax
        let dname = '...'. strpart(dname, len(dname) - dnmax)
    endif
    let marker = []
    if g:tlib_inputlist_filename_indicators
        let bnr = bufnr(a:file)
        " TLogVAR a:file, bnr, self.bufnr
        if bnr != -1
            if bnr == self.bufnr
                call add(marker, '%')
            else
                call add(marker, ' ')
                " elseif buflisted(a:file)
                "     if getbufvar(a:file, "&mod")
                "         call add(marker, '+')
                "     else
                "         call add(marker, 'B')
                "     endif
                " elseif bufloaded(a:file)
                "     call add(marker, 'h')
                " else
                "     call add(marker, 'u')
            endif
        else
            call add(marker, ' ')
        endif
    endif
    call insert(marker, '|')
    call add(marker, '|')
    return printf("%-". eval(g:tlib_inputlist_width_filename) ."s %s %s", fname, join(marker, ''), dname)
endf


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
        if !empty(rx) && (negative ? rx[0] == g:tlib_inputlist_not : rx[0] != g:tlib_inputlist_not)
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
    return self.GetBaseIdx(a:idx) - 1
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
    " TLogVAR self.filter
    for filter in self.filter
        " TLogVAR filter
        let rx = join(reverse(filter(copy(filter), '!empty(v:val)')), '\|')
        if rx =~ '\u'
            let mrx1 = mrx .'\C'
        else
            let mrx1 = mrx
        endif
        " TLogVAR rx
        if rx[0] == g:tlib_inputlist_not
            if len(rx) > 1
                call add(self.filter_neg, mrx1 .'\('. rx[1:-1] .'\)')
            endif
        else
            call add(self.filter_pos, mrx1 .'\('. rx .'\)')
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
            throw 'tlib: Unknown mode for tlib_inputlist_match: '. a:match_mode
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
    call self.SetFilter()
    " TLogVAR self.filter_neg, self.filter_pos
    if empty(self.filter_pos) && empty(self.filter_neg)
        let self.table = range(1, len(self.base))
        let self.list = copy(self.base)
    else
        let self.table = filter(range(1, len(self.base)), 'self.MatchBaseIdx(v:val)')
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
    keepalt return tlib#scratch#UseScratch(self)
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
    " TLogVAR scratch
    syntax match InputlListIndex /^\d\+:/
    syntax match InputlListCursor /^\d\+\* .*$/ contains=InputlListIndex
    syntax match InputlListSelected /^\d\+# .*$/ contains=InputlListIndex
    hi def link InputlListIndex Constant
    hi def link InputlListCursor Search
    hi def link InputlListSelected IncSearch
    " exec "au BufEnter <buffer> call tlib#input#Resume(". string(self.name) .")"
    setlocal nowrap
    " hi def link InputlListIndex Special
    " let b:tlibDisplayListMarks = {}
    let b:tlibDisplayListMarks = []
    let b:tlibDisplayListWorld = self
    call tlib#hook#Run('tlib_UseInputListScratch', self)
    return scratch
endf


" :def: function! s:prototype.Reset(?initial=0)
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


" :nodoc:
function! s:prototype.DisplayHelp() dict "{{{3
    " \ 'Help:',
    let help = [
                \ 'Mouse        ... Pick an item            Letter          ... Filter the list',
                \ printf('<m-Number>   ... Pick an item            "%s", "%s", %sWORD ... AND, OR, NOT',
                \   g:tlib_inputlist_and, g:tlib_inputlist_or, g:tlib_inputlist_not),
                \ 'Enter        ... Pick the current item   <bs>, <c-bs>    ... Reduce filter',
                \ '<c|m-r>      ... Reset the display       Up/Down         ... Next/previous item',
                \ '<c|m-q>      ... Edit top filter string  Page Up/Down    ... Scroll',
                \ '<Esc>        ... Abort',
                \ ]

    if self.allow_suspend
        call add(help,
                \ '<c|m-z>      ... Suspend/Resume          <c-o>           ... Switch to origin')
    endif

    if stridx(self.type, 'm') != -1
        let help += [
                \ '#, <c-space> ... (Un)Select the current item',
                \ '<c|m-a>      ... (Un)Select all currently visible items',
                \ '<s-up/down>  ... (Un)Select items',
                \ ]
                    " \ '<c-\>        ... Show only selected',
    endif
    for handler in self.key_handlers
        let key = get(handler, 'key_name', '')
        if !empty(key)
            let desc = get(handler, 'help', '')
            call add(help, printf('%-12s ... %s', key, desc))
        endif
    endfor
    if !empty(self.help_extra)
        let help += self.help_extra
    endif
    let help += [
                \ '',
                \ 'Exact matches and matches at word boundaries is given more weight.',
                \ 'Warning: Please don''t resize the window with the mouse.',
                \ '',
                \ 'Press any key to continue.',
                \ ]
    " call tlib#normal#WithRegister('gg"tdG', 't')
    call tlib#buffer#DeleteRange('1', '$')
    call append(0, help)
    " call tlib#normal#WithRegister('G"tddgg', 't')
    call tlib#buffer#DeleteRange('$', '$')
    1
    call self.Resize(len(help), 0)
endf


" :nodoc:
function! s:prototype.Resize(hsize, vsize) dict "{{{3
    " TLogVAR self.scratch_vertical, a:hsize, a:vsize
    let world_resize = ''
    let scratch_split = get(self, 'scratch_split', 1)
    " TLogVAR scratch_split
    if scratch_split > 0
        if self.scratch_vertical
            if a:vsize
                let world_resize = 'vert resize '. a:vsize
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
        " TLogVAR world_resize
        exec world_resize
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


" function! s:prototype.DisplayList(query, ?list)
" :nodoc:
function! s:prototype.DisplayList(query, ...) dict "{{{3
    " TLogVAR a:query
    " TLogVAR self.state
    let list = a:0 >= 1 ? a:1 : []
    " TLogDBG 'len(list) = '. len(list)
    call self.UseScratch()
    " TLogVAR self.scratch
    " TAssert IsNotEmpty(self.scratch)
    if self.state == 'scroll'
        call self.ScrollToOffset()
    elseif self.state == 'help'
        call self.DisplayHelp()
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
        call self.SetStatusline(a:query)
        " TLogVAR self.offset
        call self.ScrollToOffset()
        let rx0 = self.GetRx0()
        " TLogVAR rx0
        if !empty(self.matcher.highlight)
            if empty(rx0)
                match none
            elseif self.IsValidFilter()
                exec 'match '. self.matcher.highlight .' /\c'. escape(rx0, '/') .'/'
            endif
        endif
    endif
    redraw
endf


function! s:prototype.SetStatusline(query) dict "{{{3
    let query   = a:query
    let options = [self.matcher.name]
    if self.sticky
        call add(options, '#')
    endif
    if !empty(options)
        let sopts = printf('[%s]', join(options, ', '))
        " let echo  = query . repeat(' ', &columns - len(sopts) - len(query) - 20) . sopts
        let echo  = query . '  ' . sopts
        " let query .= '%%='. sopts .' '
    endif
    " TLogVAR &l:statusline, query
    " let &l:statusline = query
    echo
    echo echo
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

