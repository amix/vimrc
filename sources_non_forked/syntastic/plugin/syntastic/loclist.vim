if exists('g:loaded_syntastic_loclist') || !exists('g:loaded_syntastic_plugin')
    finish
endif
let g:loaded_syntastic_loclist = 1

let g:SyntasticLoclist = {}

" Public methods {{{1

function! g:SyntasticLoclist.New(rawLoclist) abort " {{{2
    let newObj = copy(self)

    let llist = filter(copy(a:rawLoclist), 'v:val["valid"]')

    for e in llist
        if get(e, 'type', '') ==# ''
            let e['type'] = 'E'
        endif
    endfor

    let newObj._rawLoclist = llist
    let newObj._name = ''
    let newObj._owner = bufnr('')
    let newObj._sorted = 0
    let newObj._columns = g:syntastic_cursor_columns

    return newObj
endfunction " }}}2

function! g:SyntasticLoclist.current(...) abort " {{{2
    let buf = a:0 ? a:1 : bufnr('')
    let loclist = syntastic#util#getbufvar(buf, 'syntastic_loclist', {})
    if type(loclist) != type({}) || empty(loclist)
        unlet! loclist
        let loclist = g:SyntasticLoclist.New([])
    endif
    return loclist
endfunction " }}}2

function! g:SyntasticLoclist.extend(other) abort " {{{2
    call extend(self._rawLoclist, a:other.copyRaw())
endfunction " }}}2

function! g:SyntasticLoclist.sort() abort " {{{2
    if !self._sorted
        for e in self._rawLoclist
            call s:_set_screen_column(e)
        endfor

        call sort(self._rawLoclist, self._columns ? 's:_compare_error_items_by_columns' : 's:_compare_error_items_by_lines')

        let self._sorted = 1
    endif
endfunction " }}}2

function! g:SyntasticLoclist.isEmpty() abort " {{{2
    return empty(self._rawLoclist)
endfunction " }}}2

function! g:SyntasticLoclist.isNewerThan(stamp) abort " {{{2
    if !exists('self._stamp')
        let self._stamp = []
        return 0
    endif
    return syntastic#util#compareLexi(self._stamp, a:stamp) > 0
endfunction " }}}2

function! g:SyntasticLoclist.copyRaw() abort " {{{2
    return copy(self._rawLoclist)
endfunction " }}}2

function! g:SyntasticLoclist.getRaw() abort " {{{2
    return self._rawLoclist
endfunction " }}}2

function! g:SyntasticLoclist.getBuffers() abort " {{{2
    return syntastic#util#unique(map(copy(self._rawLoclist), 'str2nr(v:val["bufnr"])') + [self._owner])
endfunction " }}}2

function! g:SyntasticLoclist.getCursorColumns() abort " {{{2
    return self._columns
endfunction " }}}2

function! g:SyntasticLoclist.getStatuslineFlag() abort " {{{2
    if !exists('self._stl_format')
        let self._stl_format = ''
    endif
    if !exists('self._stl_flag')
        let self._stl_flag = ''
    endif

    if g:syntastic_stl_format !=# self._stl_format
        let self._stl_format = g:syntastic_stl_format

        if !empty(self._rawLoclist)
            let errors = self.errors()
            let warnings = self.warnings()

            let num_errors = len(errors)
            let num_warnings = len(warnings)
            let num_issues = len(self._rawLoclist)

            let output = self._stl_format

            "hide stuff wrapped in %E(...) unless there are errors
            let output = substitute(output, '\m\C%E{\([^}]*\)}', num_errors ? '\1' : '' , 'g')

            "hide stuff wrapped in %W(...) unless there are warnings
            let output = substitute(output, '\m\C%W{\([^}]*\)}', num_warnings ? '\1' : '' , 'g')

            "hide stuff wrapped in %B(...) unless there are both errors and warnings
            let output = substitute(output, '\m\C%B{\([^}]*\)}', (num_warnings && num_errors) ? '\1' : '' , 'g')

            let flags = {
                \ '%':  '%',
                \ 't':  num_issues,
                \ 'e':  num_errors,
                \ 'w':  num_warnings,
                \ 'N':  (num_issues ? fnamemodify( bufname(self._rawLoclist[0]['bufnr']), ':t') : ''),
                \ 'P':  (num_issues ? fnamemodify( bufname(self._rawLoclist[0]['bufnr']), ':p:~:.') : ''),
                \ 'F':  (num_issues ? self._rawLoclist[0]['lnum'] : ''),
                \ 'ne': (num_errors ? fnamemodify( bufname(errors[0]['bufnr']), ':t') : ''),
                \ 'pe': (num_errors ? fnamemodify( bufname(errors[0]['bufnr']), ':p:~:.') : ''),
                \ 'fe': (num_errors ? errors[0]['lnum'] : ''),
                \ 'nw': (num_warnings ? fnamemodify( bufname(warnings[0]['bufnr']), ':t') : ''),
                \ 'pw': (num_warnings ? fnamemodify( bufname(warnings[0]['bufnr']), ':p:~:.') : ''),
                \ 'fw': (num_warnings ? warnings[0]['lnum'] : '') }
            let output = substitute(output, '\v\C\%(-?\d*%(\.\d+)?)([npf][ew]|[NPFtew%])', '\=syntastic#util#wformat(submatch(1), flags[submatch(2)])', 'g')

            let self._stl_flag = output
        else
            let self._stl_flag = ''
        endif
    endif

    return self._stl_flag
endfunction " }}}2

function! g:SyntasticLoclist.getFirstError(...) abort " {{{2
    let max_issues = len(self._rawLoclist)
    if a:0 && a:1 < max_issues
        let max_issues = a:1
    endif

    for idx in range(max_issues)
        if get(self._rawLoclist[idx], 'type', '') ==? 'E'
            return idx + 1
        endif
    endfor

    return 0
endfunction " }}}2

function! g:SyntasticLoclist.getName() abort " {{{2
    return len(self._name)
endfunction " }}}2

function! g:SyntasticLoclist.setName(name) abort " {{{2
    let self._name = a:name
endfunction " }}}2

function! g:SyntasticLoclist.getOwner() abort " {{{2
    return self._owner
endfunction " }}}2

function! g:SyntasticLoclist.setOwner(buffer) abort " {{{2
    let self._owner = type(a:buffer) == type(0) ? a:buffer : str2nr(a:buffer)
endfunction " }}}2

function! g:SyntasticLoclist.deploy() abort " {{{2
    let self._stamp = syntastic#util#stamp()
    for buf in self.getBuffers()
        call setbufvar(buf, 'syntastic_loclist', self)
    endfor
endfunction " }}}2

function! g:SyntasticLoclist.destroy() abort " {{{2
    for buf in self.getBuffers()
        call setbufvar(buf, 'syntastic_loclist', {})
    endfor
endfunction " }}}2

function! g:SyntasticLoclist.decorate(tag) abort " {{{2
    for e in self._rawLoclist
        let e['text'] .= ' [' . a:tag . ']'
    endfor
endfunction " }}}2

function! g:SyntasticLoclist.balloons() abort " {{{2
    if !exists('self._cachedBalloons')
        let sep = has('balloon_multiline') ? "\n" : ' | '

        let self._cachedBalloons = {}
        for e in self._rawLoclist
            let buf = e['bufnr']

            if !has_key(self._cachedBalloons, buf)
                let self._cachedBalloons[buf] = {}
            endif

            if has_key(self._cachedBalloons[buf], e['lnum'])
                let self._cachedBalloons[buf][e['lnum']] .= sep . e['text']
            else
                let self._cachedBalloons[buf][e['lnum']] = e['text']
            endif
        endfor
    endif

    return get(self._cachedBalloons, bufnr(''), {})
endfunction " }}}2

function! g:SyntasticLoclist.errors() abort " {{{2
    if !exists('self._cachedErrors')
        let self._cachedErrors = self.filter({'type': 'E'})
    endif
    return self._cachedErrors
endfunction " }}}2

function! g:SyntasticLoclist.warnings() abort " {{{2
    if !exists('self._cachedWarnings')
        let self._cachedWarnings = self.filter({'type': 'W'})
    endif
    return self._cachedWarnings
endfunction " }}}2

" Legacy function.  Syntastic no longer calls it, but we keep it
" around because other plugins (f.i. powerline) depend on it.
function! g:SyntasticLoclist.hasErrorsOrWarningsToDisplay() abort " {{{2
    return !self.isEmpty()
endfunction " }}}2

" cache used by EchoCurrentError()
function! g:SyntasticLoclist.messages(buf) abort " {{{2
    if !exists('self._cachedMessages')
        let self._cachedMessages = {}

        let errors = self.errors() + self.warnings()
        for e in errors
            let b = e['bufnr']
            let l = e['lnum']

            if !has_key(self._cachedMessages, b)
                let self._cachedMessages[b] = {}
            endif

            if !has_key(self._cachedMessages[b], l)
                let self._cachedMessages[b][l] = [e]
            elseif self._columns
                call add(self._cachedMessages[b][l], e)
            endif
        endfor

        if self._columns
            if !self._sorted
                for b in keys(self._cachedMessages)
                    for l in keys(self._cachedMessages[b])
                        if len(self._cachedMessages[b][l]) > 1
                            for e in self._cachedMessages[b][l]
                                call s:_set_screen_column(e)
                            endfor
                            call sort(self._cachedMessages[b][l], 's:_compare_error_items_by_columns')
                        endif
                    endfor
                endfor
            endif

            for b in keys(self._cachedMessages)
                for l in keys(self._cachedMessages[b])
                    call s:_remove_shadowed_items(self._cachedMessages[b][l])
                endfor
            endfor
        endif
    endif

    return get(self._cachedMessages, a:buf, {})
endfunction " }}}2

"Filter the list and return new native loclist
"e.g.
"  .filter({'bufnr': 10, 'type': 'e'})
"
"would return all errors for buffer 10.
"
"Note that all string comparisons are done with ==?
function! g:SyntasticLoclist.filter(filters) abort " {{{2
    let conditions = values(map(copy(a:filters), 's:_translate(v:key, v:val)'))
    let filter = len(conditions) == 1 ?
        \ conditions[0] : join(map(conditions, '"(" . v:val . ")"'), ' && ')
    return filter(copy(self._rawLoclist), filter)
endfunction " }}}2

function! g:SyntasticLoclist.setloclist(new) abort " {{{2
    if !exists('w:syntastic_loclist_set')
        let w:syntastic_loclist_set = []
    endif
    if a:new || empty(w:syntastic_loclist_set) || w:syntastic_loclist_set != [self._owner, getbufvar(self._owner, 'changedtick')]
        let replace = !a:new && g:syntastic_reuse_loc_lists && !empty(w:syntastic_loclist_set)
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_NOTIFICATIONS, 'loclist: setloclist ' . (replace ? '(replace)' : '(new)'))
        call setloclist(0, self.getRaw(), replace ? 'r' : ' ')
        try
            " Vim 7.4.2200 or later
            call setloclist(0, [], 'r', { 'title': ':SyntasticCheck ' . self._name })
        catch /\m^Vim\%((\a\+)\)\=:E\%(118\|731\)/
            " do nothing
        endtry
        call syntastic#util#setLastTick(self._owner)
        let w:syntastic_loclist_set = [self._owner, getbufvar(self._owner, 'syntastic_lasttick')]
    endif
endfunction " }}}2

"display the cached errors for this buf in the location list
function! g:SyntasticLoclist.show() abort " {{{2
    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_NOTIFICATIONS, 'loclist: show')
    call self.setloclist(0)

    if !self.isEmpty()
        let num = winnr()
        execute 'lopen ' . syntastic#util#var('loc_list_height')
        if num != winnr()
            execute num . 'wincmd w'
        endif

        " try to find the loclist window and set w:quickfix_title
        let errors = getloclist(0)
        for buf in tabpagebuflist()
            if buflisted(buf) && bufloaded(buf) && getbufvar(buf, '&buftype') ==# 'quickfix'
                let win = bufwinnr(buf)
                let title = getwinvar(win, 'quickfix_title')

                " TODO: try to make sure we actually own this window; sadly,
                " errors == getloclist(0) is the only somewhat safe way to
                " achieve that
                if strpart(title, 0, 16) ==# ':SyntasticCheck ' ||
                            \ ( (title ==# '' || title ==# ':setloclist()') && errors == getloclist(0) )
                    call setwinvar(win, 'quickfix_title', ':SyntasticCheck ' . self._name)
                    call setbufvar(buf, 'syntastic_owner_buffer', self._owner)
                endif
            endif
        endfor
    endif
endfunction " }}}2

" }}}1

" Public functions {{{1

function! SyntasticLoclistHide() abort " {{{2
    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_NOTIFICATIONS, 'loclist: hide')
    silent! lclose
endfunction " }}}2

" }}}1

" Utilities {{{1

function! s:_translate(key, val) abort " {{{2
    return 'get(v:val, ' . string(a:key) . ', "") ==? ' . string(a:val)
endfunction " }}}2

function! s:_set_screen_column(item) abort " {{{2
    if !has_key(a:item, 'scol')
        let col = get(a:item, 'col', 0)
        if col != 0 && get(a:item, 'vcol', 0) == 0
            let buf = str2nr(a:item['bufnr'])
            try
                let line = getbufline(buf, a:item['lnum'])[0]
            catch  /\m^Vim\%((\a\+)\)\=:E684/
                let line = ''
            endtry
            let a:item['scol'] = syntastic#util#screenWidth(strpart(line, 0, col), getbufvar(buf, '&tabstop'))
        else
            let a:item['scol'] = col
        endif
    endif
endfunction " }}}2

function! s:_remove_shadowed_items(errors) abort " {{{2
    " keep only the first message at a given column
    let i = 0
    while i < len(a:errors) - 1
        let j = i + 1
        let dupes = 0
        while j < len(a:errors) && a:errors[j].scol == a:errors[i].scol
            let dupes = 1
            let j += 1
        endwhile
        if dupes
            call remove(a:errors, i + 1, j - 1)
        endif
        let i += 1
    endwhile

    " merge messages with the same text
    let i = 0
    while i < len(a:errors) - 1
        let j = i + 1
        let dupes = 0
        while j < len(a:errors) && a:errors[j].text == a:errors[i].text
            let dupes = 1
            let j += 1
        endwhile
        if dupes
            call remove(a:errors, i + 1, j - 1)
        endif
        let i += 1
    endwhile
endfunction " }}}2

function! s:_compare_error_items_by_columns(a, b) abort " {{{2
    if a:a['bufnr'] != a:b['bufnr']
        " group by file
        return a:a['bufnr'] - a:b['bufnr']
    elseif a:a['lnum'] != a:b['lnum']
        " sort by line
        return a:a['lnum'] - a:b['lnum']
    elseif a:a['scol'] != a:b['scol']
        " sort by screen column
        return a:a['scol'] - a:b['scol']
    elseif a:a['type'] !=? a:b['type']
        " errors take precedence over warnings
        return a:a['type'] ==? 'E' ? -1 : 1
    else
        return 0
    endif
endfunction " }}}2

function! s:_compare_error_items_by_lines(a, b) abort " {{{2
    if a:a['bufnr'] != a:b['bufnr']
        " group by file
        return a:a['bufnr'] - a:b['bufnr']
    elseif a:a['lnum'] != a:b['lnum']
        " sort by line
        return a:a['lnum'] - a:b['lnum']
    elseif a:a['type'] !=? a:b['type']
        " errors take precedence over warnings
        return a:a['type'] ==? 'E' ? -1 : 1
    else
        " sort by screen column
        return a:a['scol'] - a:b['scol']
    endif
endfunction " }}}2

" }}}1

" vim: set sw=4 sts=4 et fdm=marker:
