if exists('g:loaded_syntastic_checker') || !exists('g:loaded_syntastic_plugin')
    finish
endif
let g:loaded_syntastic_checker = 1

let g:SyntasticChecker = {}

" Public methods {{{1

function! g:SyntasticChecker.New(args) abort " {{{2
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

    if has_key(a:args, 'enable')
        let newObj._enable = a:args['enable']
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

function! g:SyntasticChecker.getFiletype() abort " {{{2
    return self._filetype
endfunction " }}}2

function! g:SyntasticChecker.getName() abort " {{{2
    return self._name
endfunction " }}}2

" Synchronise _exec with user's setting.  Force re-validation if needed.
"
" XXX: This function must be called at least once before calling either
" getExec() or getExecEscaped().  Normally isAvailable() does that for you
" automatically, but you should keep still this in mind if you change the
" current checker workflow.
function! g:SyntasticChecker.syncExec() abort " {{{2
    let user_exec =
        \ expand( exists('b:syntastic_' . self._name . '_exec') ? b:syntastic_{self._name}_exec :
        \ syntastic#util#var(self._filetype . '_' . self._name . '_exec'), 1 )

    if user_exec !=# '' && user_exec !=# self._exec
        let self._exec = user_exec
        if has_key(self, '_available')
            " we have a new _exec on the block, it has to be validated
            call remove(self, '_available')
        endif
    endif
endfunction " }}}2

function! g:SyntasticChecker.getExec() abort " {{{2
    return self._exec
endfunction " }}}2

function! g:SyntasticChecker.getExecEscaped() abort " {{{2
    return syntastic#util#shescape(self._exec)
endfunction " }}}2

function! g:SyntasticChecker.getLocListRaw() abort " {{{2
    let name = self._filetype . '/' . self._name

    if has_key(self, '_enable')
        let status = syntastic#util#var(self._enable, -1)
        if status < 0
            call syntastic#log#error('checker ' . name . ': checks disabled for security reasons; ' .
                \ 'set g:syntastic_' . self._enable . ' to 1 to override')
        endif
        if status <= 0
            call syntastic#log#debug(g:_SYNTASTIC_DEBUG_TRACE, 'getLocList: checker ' . name . ' enabled but not forced')
            return []
        else
            call syntastic#log#debug(g:_SYNTASTIC_DEBUG_TRACE, 'getLocList: checker ' . name . ' forced')
        endif
    endif

    try
        let list = self._locListFunc()
        if self._exec !=# ''
            call syntastic#log#debug(g:_SYNTASTIC_DEBUG_TRACE, 'getLocList: checker ' . name . ' returned ' . v:shell_error)
        endif
    catch /\m\C^Syntastic: checker error$/
        let list = []
        if self._exec !=# ''
            call syntastic#log#error('checker ' . name . ' returned abnormal status ' . v:shell_error)
        else
            call syntastic#log#error('checker ' . name . ' aborted')
        endif
    endtry
    call self._populateHighlightRegexes(list)
    call syntastic#log#debug(g:_SYNTASTIC_DEBUG_LOCLIST, name . ' raw:', list)
    call self._quietMessages(list)
    return list
endfunction " }}}2

function! g:SyntasticChecker.getLocList() abort " {{{2
    return g:SyntasticLoclist.New(self.getLocListRaw())
endfunction " }}}2

function! g:SyntasticChecker.getVersion(...) abort " {{{2
    if !exists('self._version')
        let command = a:0 ? a:1 : self.getExecEscaped() . ' --version'
        let version_output = syntastic#util#system(command)
        call self.log('getVersion: ' . string(command) . ': ' .
            \ string(split(version_output, "\n", 1)) .
            \ (v:shell_error ? ' (exit code ' . v:shell_error . ')' : '') )
        let parsed_ver = syntastic#util#parseVersion(version_output)
        if len(parsed_ver)
            call self.setVersion(parsed_ver)
        else
            call syntastic#log#ndebug(g:_SYNTASTIC_DEBUG_LOCLIST, 'checker output:', split(version_output, "\n", 1))
            call syntastic#log#error("checker " . self._filetype . "/" . self._name . ": can't parse version string (abnormal termination?)")
        endif
    endif
    return get(self, '_version', [])
endfunction " }}}2

function! g:SyntasticChecker.setVersion(version) abort " {{{2
    if len(a:version)
        let self._version = copy(a:version)
        call self.log(self.getExec() . ' version =', a:version)
    endif
endfunction " }}}2

function! g:SyntasticChecker.log(msg, ...) abort " {{{2
    let leader = self._filetype . '/' . self._name . ': '
    if a:0 > 0
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_CHECKERS, leader . a:msg, a:1)
    else
        call syntastic#log#debug(g:_SYNTASTIC_DEBUG_CHECKERS, leader . a:msg)
    endif
endfunction " }}}2

function! g:SyntasticChecker.makeprgBuild(opts) abort " {{{2
    let basename = self._filetype . '_' . self._name . '_'

    let parts = []
    call extend(parts, self._getOpt(a:opts, basename, 'exe', self.getExecEscaped()))
    call extend(parts, self._getOpt(a:opts, basename, 'args', ''))
    call extend(parts, self._getOpt(a:opts, basename, 'fname', syntastic#util#shexpand('%')))
    call extend(parts, self._getOpt(a:opts, basename, 'post_args', ''))
    call extend(parts, self._getOpt(a:opts, basename, 'tail', ''))

    return join(parts)
endfunction " }}}2

function! g:SyntasticChecker.isAvailable() abort " {{{2
    call self.syncExec()
    if !has_key(self, '_available')
        let self._available = self._isAvailableFunc()
    endif
    return self._available
endfunction " }}}2

function! g:SyntasticChecker.isDisabled() abort " {{{2
    return has_key(self, '_enable') && syntastic#util#var(self._enable, -1) <= 0
endfunction " }}}2

function! g:SyntasticChecker.wantSort() abort " {{{2
    return syntastic#util#var(self._filetype . '_' . self._name . '_sort', 0)
endfunction " }}}2

" This method is no longer used by syntastic.  It's here only to maintain
" backwards compatibility with external checkers which might depend on it.
function! g:SyntasticChecker.setWantSort(val) abort " {{{2
    if !exists('g:syntastic_' . self._filetype . '_' . self._name . '_sort')
        let g:syntastic_{self._filetype}_{self._name}_sort = a:val
    endif
endfunction " }}}2

" }}}1

" Private methods {{{1

function! g:SyntasticChecker._quietMessages(errors) abort " {{{2
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

function! g:SyntasticChecker._populateHighlightRegexes(errors) abort " {{{2
    if has_key(self, '_highlightRegexFunc')
        for e in a:errors
            if e['valid']
                let term = self._highlightRegexFunc(e)
                if term !=# ''
                    let e['hl'] = term
                endif
            endif
        endfor
    endif
endfunction " }}}2

function! g:SyntasticChecker._getOpt(opts, basename, name, default) abort " {{{2
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
