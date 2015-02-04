if exists("g:loaded_syntastic_checker") || !exists("g:loaded_syntastic_plugin")
    finish
endif
let g:loaded_syntastic_checker = 1

let g:SyntasticChecker = {}

" Public methods {{{1

function! g:SyntasticChecker.New(args) " {{{2
    let newObj = copy(self)

    let newObj._filetype = a:args['filetype']
    let newObj._name = a:args['name']
    let newObj._exec = get(a:args, 'exec', newObj._name)

    if has_key(a:args, 'redirect')
        let [filetype, name] = split(a:args['redirect'], '/')
        let prefix = 'SyntaxCheckers_' . filetype . '_' . name . '_'

        if exists('g:syntastic_' . filetype . '_' . name . '_sort') && !exists('g:syntastic_' . newObj._filetype . '_' . newObj._name . '_sort')
            let g:syntastic_{newObj._filetype}_{newObj._name}_sort = g:syntastic_{filetype}_{name}_sort
        endif
    else
        let prefix = 'SyntaxCheckers_' . newObj._filetype . '_' . newObj._name . '_'
    endif

    let newObj._locListFunc = function(prefix . 'GetLocList')

    if exists('*' . prefix . 'IsAvailable')
        let newObj._isAvailableFunc = function(prefix . 'IsAvailable')
    else
        let newObj._isAvailableFunc = function('s:_isAvailableDefault')
    endif

    if exists('*' . prefix . 'GetHighlightRegex')
        let newObj._highlightRegexFunc = function(prefix . 'GetHighlightRegex')
    endif

    return newObj
endfunction " }}}2

function! g:SyntasticChecker.getFiletype() " {{{2
    return self._filetype
endfunction " }}}2

function! g:SyntasticChecker.getName() " {{{2
    return self._name
endfunction " }}}2

function! g:SyntasticChecker.getExec() " {{{2
    return
        \ expand( exists('b:syntastic_' . self._name . '_exec') ? b:syntastic_{self._name}_exec :
        \ syntastic#util#var(self._filetype . '_' . self._name . '_exec', self._exec), 1 )
endfunction " }}}2

function! g:SyntasticChecker.getExecEscaped() " {{{2
    return syntastic#util#shescape(self.getExec())
endfunction " }}}2

function! g:SyntasticChecker.getLocListRaw() " {{{2
    let name = self._filetype . '/' . self._name
    try
        let list = self._locListFunc()
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_TRACE, 'getLocList: checker ' . name . ' returned ' . v:shell_error)
    catch /\m\C^Syntastic: checker error$/
        let list = []
        call syntastic#log#error('checker ' . name . ' returned abnormal status ' . v:shell_error)
    endtry
    call self._populateHighlightRegexes(list)
    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_LOCLIST, name . ' raw:', list)
    call self._quietMessages(list)
    return list
endfunction " }}}2

function! g:SyntasticChecker.getLocList() " {{{2
    return g:SyntasticLoclist.New(self.getLocListRaw())
endfunction " }}}2

function! g:SyntasticChecker.getVersion(...) " {{{2
    if !exists('self._version')
        let command = a:0 ? a:1 : self.getExecEscaped() . ' --version'
        let version_output = system(command)
        call self.log('getVersion: ' . string(command) . ': ' .
            \ string(split(version_output, "\n", 1)) .
            \ (v:shell_error ? ' (exit code ' . v:shell_error . ')' : '') )
        call self.setVersion(syntastic#util#parseVersion(version_output))
    endif
    return get(self, '_version', [])
endfunction " }}}2

function! g:SyntasticChecker.setVersion(version) " {{{2
    if len(a:version)
        let self._version = copy(a:version)
        call self.log(self.getExec() . ' version =', a:version)
    else
        call syntastic#log#error("checker " . self._filetype . "/" . self._name . ": can't parse version string (abnormal termination?)")
    endif
endfunction " }}}2

function! g:SyntasticChecker.log(msg, ...) " {{{2
    let leader = self._filetype . '/' . self._name . ': '
    if a:0 > 0
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_CHECKERS, leader . a:msg, a:1)
    else
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_CHECKERS, leader . a:msg)
    endif
endfunction " }}}2

function! g:SyntasticChecker.makeprgBuild(opts) " {{{2
    let basename = self._filetype . '_' . self._name . '_'

    let parts = []
    call extend(parts, self._getOpt(a:opts, basename, 'exe', self.getExecEscaped()))
    call extend(parts, self._getOpt(a:opts, basename, 'args', ''))
    call extend(parts, self._getOpt(a:opts, basename, 'fname', syntastic#util#shexpand('%')))
    call extend(parts, self._getOpt(a:opts, basename, 'post_args', ''))
    call extend(parts, self._getOpt(a:opts, basename, 'tail', ''))

    return join(parts)
endfunction " }}}2

function! g:SyntasticChecker.isAvailable() " {{{2
    if !has_key(self, '_available')
        let self._available = self._isAvailableFunc()
    endif
    return self._available
endfunction " }}}2

function! g:SyntasticChecker.wantSort() " {{{2
    return syntastic#util#var(self._filetype . '_' . self._name . '_sort', 0)
endfunction " }}}2

" This method is no longer used by syntastic.  It's here only to maintain
" backwards compatibility with external checkers which might depend on it.
function! g:SyntasticChecker.setWantSort(val) " {{{2
    if !exists('g:syntastic_' . self._filetype . '_' . self._name . '_sort')
        let g:syntastic_{self._filetype}_{self._name}_sort = a:val
    endif
endfunction " }}}2

" }}}1

" Private methods {{{1

function! g:SyntasticChecker._quietMessages(errors) " {{{2
    " wildcard quiet_messages
    let quiet_filters = copy(syntastic#util#var('quiet_messages', {}))
    if type(quiet_filters) != type({})
        call syntastic#log#warn('ignoring invalid syntastic_quiet_messages')
        unlet quiet_filters
        let quiet_filters = {}
    endif

    " per checker quiet_messages
    let name = self._filetype . '_' . self._name
    try
        call extend( quiet_filters, copy(syntastic#util#var(name . '_quiet_messages', {})), 'force' )
    catch /\m^Vim\%((\a\+)\)\=:E712/
        call syntastic#log#warn('ignoring invalid syntastic_' . name . '_quiet_messages')
    endtry

    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_LOCLIST, 'quiet_messages filter:', quiet_filters)

    if !empty(quiet_filters)
        call syntastic#util#dictFilter(a:errors, quiet_filters)
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_LOCLIST, 'filtered by quiet_messages:', a:errors)
    endif
endfunction " }}}2

function! g:SyntasticChecker._populateHighlightRegexes(errors) " {{{2
    if has_key(self, '_highlightRegexFunc')
        for e in a:errors
            if e['valid']
                let term = self._highlightRegexFunc(e)
                if term != ''
                    let e['hl'] = term
                endif
            endif
        endfor
    endif
endfunction " }}}2

function! g:SyntasticChecker._getOpt(opts, basename, name, default) " {{{2
    let ret = []
    call extend( ret, syntastic#util#argsescape(get(a:opts, a:name . '_before', '')) )
    call extend( ret, syntastic#util#argsescape(syntastic#util#var( a:basename . a:name, get(a:opts, a:name, a:default) )) )
    call extend( ret, syntastic#util#argsescape(get(a:opts, a:name . '_after', '')) )

    return ret
endfunction " }}}2

" }}}1

" Private functions {{{1

function! s:_isAvailableDefault() dict " {{{2
    return executable(self.getExec())
endfunction " }}}2

" }}}1

" vim: set sw=4 sts=4 et fdm=marker:
