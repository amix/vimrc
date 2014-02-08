if exists("g:loaded_syntastic_modemap")
    finish
endif
let g:loaded_syntastic_modemap = 1

let g:SyntasticModeMap = {}

" Public methods {{{1

function! g:SyntasticModeMap.Instance()
    if !exists('s:SyntasticModeMapInstance')
        let s:SyntasticModeMapInstance = copy(self)
        call s:SyntasticModeMapInstance.synch()
    endif

    return s:SyntasticModeMapInstance
endfunction

function! g:SyntasticModeMap.synch()
    if exists('g:syntastic_mode_map')
        let self._mode = get(g:syntastic_mode_map, 'mode', 'active')
        let self._activeFiletypes = get(g:syntastic_mode_map, 'active_filetypes', [])
        let self._passiveFiletypes = get(g:syntastic_mode_map, 'passive_filetypes', [])
    else
        let self._mode = 'active'
        let self._activeFiletypes = []
        let self._passiveFiletypes = []
    endif
endfunction

function! g:SyntasticModeMap.allowsAutoChecking(filetype)
    let fts = split(a:filetype, '\m\.')

    if self.isPassive()
        return self._isOneFiletypeActive(fts)
    else
        return self._noFiletypesArePassive(fts)
    endif
endfunction

function! g:SyntasticModeMap.isPassive()
    return self._mode ==# 'passive'
endfunction

function! g:SyntasticModeMap.toggleMode()
    call self.synch()

    if self._mode ==# 'active'
        let self._mode = 'passive'
    else
        let self._mode = 'active'
    endif

    "XXX Changing a global variable.  Tsk, tsk...
    if !exists('g:syntastic_mode_map')
        let g:syntastic_mode_map = {}
    endif
    let g:syntastic_mode_map['mode'] = self._mode
endfunction

function! g:SyntasticModeMap.echoMode()
    echo "Syntastic: " . self._mode . " mode enabled"
endfunction

" Private methods {{{1

function! g:SyntasticModeMap._isOneFiletypeActive(filetypes)
    return !empty(filter(copy(a:filetypes), 'index(self._activeFiletypes, v:val) != -1'))
endfunction

function! g:SyntasticModeMap._noFiletypesArePassive(filetypes)
    return empty(filter(copy(a:filetypes), 'index(self._passiveFiletypes, v:val) != -1'))
endfunction

" vim: set sw=4 sts=4 et fdm=marker:
