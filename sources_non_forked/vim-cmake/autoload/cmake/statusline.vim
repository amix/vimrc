" ==============================================================================
" Location:    autoload/cmake/statusline.vim
" Description: Functions for handling  statusline information
" ==============================================================================

let s:statusline = {}
let s:statusline.build_info = ''
let s:statusline.cmd_info = ''

let s:logger = cmake#logger#Get()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Public functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Set build info string for statusline/airline.
"
" Params:
"     build_info : String
"         statusline build info
"
function! s:statusline.SetBuildInfo(build_info) abort
    call s:logger.LogDebug('Invoked: statusline.SetBuildInfo(%s)', a:build_info)
    let l:self.build_info = a:build_info
endfunction

" Set command info string for statusline/airline.
"
" Params:
"     cmd_info : String
"         statusline command info
"
function! s:statusline.SetCmdInfo(cmd_info) abort
    call s:logger.LogDebug('Invoked: statusline.SetCmdInfo(%s)', a:cmd_info)
    let l:self.cmd_info = a:cmd_info
endfunction

" Force a refresh of the statusline/airline.
"
function! s:statusline.Refresh() abort
    if exists('g:loaded_airline') && g:loaded_airline
        execute 'AirlineRefresh!'
    else
        execute 'redrawstatus!'
    endif
endfunction

" Get build info string for statusline/airline.
"
" Params:
"     active : Number
"         whether called for the statusline of an active window
"
" Returns:
"     String
"         statusline build info
"
function! cmake#statusline#GetBuildInfo(active) abort
    if a:active
        return s:statusline.build_info
    else
        return '[' . s:statusline.build_info . ']'
    endif
endfunction

" Get command info string for statusline/airline.
"
" Returns:
"     String
"         statusline command info (command currently running)
"
function! cmake#statusline#GetCmdInfo() abort
    if len(s:statusline.cmd_info) > 0
        return s:statusline.cmd_info
    else
        return ' '
    endif
endfunction

" Get statusline 'object'.
"
function! cmake#statusline#Get() abort
    return s:statusline
endfunction
