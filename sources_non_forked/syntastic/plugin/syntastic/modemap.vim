if exists('g:loaded_syntastic_modemap') || !exists('g:loaded_syntastic_plugin')
    finish
endif
let g:loaded_syntastic_modemap = 1

let g:SyntasticModeMap = {}

" Public methods {{{1

function! g:SyntasticModeMap.Instance() abort " {{{2
    if !exists('s:SyntasticModeMapInstance')
        let s:SyntasticModeMapInstance = copy(self)
        call s:SyntasticModeMapInstance.synch()
    endif

    return s:SyntasticModeMapInstance
endfunction " }}}2

function! g:SyntasticModeMap.synch() abort " {{{2
    if exists('g:syntastic_mode_map')
        let self._mode = get(g:syntastic_mode_map, 'mode', 'active')
        let self._activeFiletypes = copy(get(g:syntastic_mode_map, 'active_filetypes', []))
        let self._passiveFiletypes = copy(get(g:syntastic_mode_map, 'passive_filetypes', []))
    else
        let self._mode = 'active'
        let self._activeFiletypes = []
        let self._passiveFiletypes = []
    endif
endfunction " }}}2

function! g:SyntasticModeMap.allowsAutoChecking(filetype) abort " {{{2
    let registry = g:SyntasticRegistry.Instance()
    let fts = registry.resolveFiletypes(a:filetype)

    if self.isPassive()
        return self._isOneFiletypeActive(fts)
    else
        return self._noFiletypesArePassive(fts)
    endif
endfunction " }}}2

function! g:SyntasticModeMap.doAutoChecking(buf) abort " {{{2
    let local_mode = getbufvar(a:buf, 'syntastic_mode')
    if local_mode ==# 'active' || local_mode ==# 'passive'
        return local_mode ==# 'active'
    endif

    return self.allowsAutoChecking(getbufvar(a:buf, '&filetype'))
endfunction " }}}2

function! g:SyntasticModeMap.isPassive() abort " {{{2
    return self._mode ==# 'passive'
endfunction " }}}2

function! g:SyntasticModeMap.toggleMode() abort " {{{2
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
endfunction " }}}2

function! g:SyntasticModeMap.echoMode() abort " {{{2
    echo 'Syntastic: ' . self._mode . ' mode enabled'
endfunction " }}}2

function! g:SyntasticModeMap.modeInfo(filetypes) abort " {{{2
    echomsg 'Syntastic version: ' . g:syntastic_version
    let type = len(a:filetypes) ? a:filetypes[0] : &filetype
    echomsg 'Info for filetype: ' . type

    call self.synch()
    echomsg 'Global mode: ' . self._mode
    if self._mode ==# 'active'
        if len(self._passiveFiletypes)
            let plural = len(self._passiveFiletypes) != 1 ? 's' : ''
            echomsg 'Passive filetype' . plural . ': ' . join(sort(copy(self._passiveFiletypes)))
        endif
    else
        if len(self._activeFiletypes)
            let plural = len(self._activeFiletypes) != 1 ? 's' : ''
            echomsg 'Active filetype' . plural . ': ' . join(sort(copy(self._activeFiletypes)))
        endif
    endif
    echomsg 'Filetype ' . type . ' is ' . (self.allowsAutoChecking(type) ? 'active' : 'passive')

    if !len(a:filetypes)
        if exists('b:syntastic_mode') && (b:syntastic_mode ==# 'active' || b:syntastic_mode ==# 'passive')
            echomsg 'Local mode: ' . b:syntastic_mode
        endif

        echomsg 'The current file will ' . (self.doAutoChecking(bufnr('')) ? '' : 'not ') . 'be checked automatically'
    endif
endfunction " }}}2

" }}}1

" Private methods {{{1

function! g:SyntasticModeMap._isOneFiletypeActive(filetypes) abort " {{{2
    return !empty(filter(copy(a:filetypes), 'index(self._activeFiletypes, v:val) != -1'))
endfunction " }}}2

function! g:SyntasticModeMap._noFiletypesArePassive(filetypes) abort " {{{2
    return empty(filter(copy(a:filetypes), 'index(self._passiveFiletypes, v:val) != -1'))
endfunction " }}}2

" }}}1

" vim: set sw=4 sts=4 et fdm=marker:
