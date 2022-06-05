" ==============================================================================
" Location:    autoload/cmake/logger.vim
" Description: Logger
" ==============================================================================

let s:logger = {}

function! s:Echo(fmt, arglist) abort
    " Trick to convert list (a:arglist) into arguments for printf().
    let l:PrintfPartial = function('printf', [a:fmt] + a:arglist)
    echomsg '[Vim-CMake] ' . l:PrintfPartial()
endfunction

function! s:Log(fmt, level, arglist) abort
    " Trick to convert list (a:arglist) into arguments for printf().
    let l:PrintfPartial = function('printf', [a:fmt] + a:arglist)
    let l:logstring = printf(
            \ '[%s] [%5s] %s',
            \ strftime('%Y-%m-%d %T'),
            \ a:level,
            \ l:PrintfPartial()
            \ )
    call writefile([l:logstring], g:cmake_log_file, 'a')
endfunction

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Public functions
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Log a debug message.
"
" Params:
"     fmt : String
"         printf-like format string (see :help printf())
"     ... :
"         list of arguments to replace placeholders in format string
"
function! s:logger.LogDebug(fmt, ...) abort
    if g:cmake_log_file !=# ''
        call s:Log(a:fmt, 'DEBUG', a:000)
    end
endfunction

" Log an information message.
"
" Params:
"     fmt : String
"         printf-like format string (see :help printf())
"     ... :
"         list of arguments to replace placeholders in format string
"
function! s:logger.LogInfo(fmt, ...) abort
    if g:cmake_log_file !=# ''
        call s:Log(a:fmt, 'INFO', a:000)
    end
endfunction

" Log a warning message.
"
" Params:
"     fmt : String
"         printf-like format string (see :help printf())
"     ... :
"         list of arguments to replace placeholders in format string
"
function! s:logger.LogWarn(fmt, ...) abort
    if g:cmake_log_file !=# ''
        call s:Log(a:fmt, 'WARN', a:000)
    end
endfunction

" Log an error message.
"
" Params:
"     fmt : String
"         printf-like format string (see :help printf())
"     ... :
"         list of arguments to replace placeholders in format string
"
function! s:logger.LogError(fmt, ...) abort
    if g:cmake_log_file !=# ''
        call s:Log(a:fmt, 'ERROR', a:000)
    end
endfunction

" Echo an information message.
"
" Params:
"     fmt : String
"         printf-like format string (see :help printf())
"     ... :
"         list of arguments to replace placeholders in format string
"
function! s:logger.EchoInfo(fmt, ...) abort
    echohl MoreMsg
    call s:Echo(a:fmt, a:000)
    echohl None
endfunction

" Echo a warning message.
"
" Params:
"     fmt : String
"         printf-like format string (see :help printf())
"     ... :
"         list of arguments to replace placeholders in format string
"
function! s:logger.EchoWarn(fmt, ...) abort
    echohl WarningMsg
    call s:Echo(a:fmt, a:000)
    echohl None
endfunction

" Echo an error message.
"
" Params:
"     fmt : String
"         printf-like format string (see :help printf())
"     ... :
"         list of arguments to replace placeholders in format string
"
function! s:logger.EchoError(fmt, ...) abort
    echohl Error
    call s:Echo(a:fmt, a:000)
    echohl None
endfunction

" Get logger 'object'
"
function! cmake#logger#Get() abort
    return s:logger
endfunction
