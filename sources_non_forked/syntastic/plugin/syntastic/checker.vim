if exists("g:loaded_syntastic_checker")
    finish
endif
let g:loaded_syntastic_checker = 1

let g:SyntasticChecker = {}

" Public methods {{{1

function! g:SyntasticChecker.New(args)
    let newObj = copy(self)

    let newObj._filetype = a:args['filetype']
    let newObj._name = a:args['name']


    let prefix = 'SyntaxCheckers_' . newObj._filetype . '_' . newObj._name . '_'
    let newObj._locListFunc = function(prefix . 'GetLocList')
    let newObj._isAvailableFunc = function(prefix . 'IsAvailable')

    if exists('*' . prefix . 'GetHighlightRegex')
        let newObj._highlightRegexFunc = function(prefix. 'GetHighlightRegex')
    else
        let newObj._highlightRegexFunc = ''
    endif

    return newObj
endfunction

function! g:SyntasticChecker.getFiletype()
    return self._filetype
endfunction

function! g:SyntasticChecker.getName()
    return self._name
endfunction

function! g:SyntasticChecker.getLocList()
    try
        let list = self._locListFunc()
        call syntastic#util#debug('getLocList: checker ' . self._filetype . '/' . self._name . ' returned ' . v:shell_error)
    catch /\m\C^Syntastic: checker error$/
        let list = []
        call syntastic#util#error('checker ' . self._filetype . '/' . self._name . ' returned abnormal status ' . v:shell_error)
    endtry
    call self._populateHighlightRegexes(list)
    return g:SyntasticLoclist.New(list)
endfunction

function! g:SyntasticChecker.getHighlightRegexFor(error)
    if empty(self._highlightRegexFunc)
        return []
    endif

    return self._highlightRegexFunc(error)
endfunction

function! g:SyntasticChecker.isAvailable()
    return self._isAvailableFunc()
endfunction

" Private methods {{{1

function! g:SyntasticChecker._populateHighlightRegexes(list)
    let list = a:list
    if !empty(self._highlightRegexFunc)
        for i in range(0, len(list)-1)
            if list[i]['valid']
                let term = self._highlightRegexFunc(list[i])
                if len(term) > 0
                    let list[i]['hl'] = term
                endif
            endif
        endfor
    endif
    return list
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
