" Author: Dan Loman <https://github.com/namolnad>
" Description: Functions for integrating with Swift tools

" Find the nearest dir containing a Package.swift file and assume it is the root of the Swift project.
function! ale#swift#FindProjectRoot(buffer) abort
    let l:swift_config = ale#path#FindNearestFile(a:buffer, 'Package.swift')

    if !empty(l:swift_config)
        return fnamemodify(l:swift_config, ':h')
    endif

    return ''
endfunction

" Support Apple Swift Format {{{1

call ale#Set('swift_appleswiftformat_executable', 'swift-format')
call ale#Set('swift_appleswiftformat_use_swiftpm', 0)

" Return the executable depending on whether or not to use Swift Package Manager.
"
" If not asked to use Swift Package Manager (use_swiftpm = 0), the returned
" value is the global executable, else the returned value is 'swift' because
" the final command line will be `swift run swift-format ...`.
"
" Failure is expected if use_swiftpm is `1` but no Package.swift can be located.
function! ale#swift#GetAppleSwiftFormatExecutable(buffer) abort
    if !ale#Var(a:buffer, 'swift_appleswiftformat_use_swiftpm')
        return ale#Var(a:buffer, 'swift_appleswiftformat_executable')
    endif

    if ale#path#FindNearestFile(a:buffer, 'Package.swift') is# ''
        " If there is no Package.swift file, we don't use swift-format even if it exists,
        " so we return '' to indicate failure.
        return ''
    endif

    return 'swift'
endfunction

" Return the command depending on whether or not to use Swift Package Manager.
"
" If asked to use Swift Package Manager (use_swiftpm = 1), the command
" arguments are prefixed with 'swift run'.
"
" In either case, the configuration file is located and added to the command.
function! ale#swift#GetAppleSwiftFormatCommand(buffer) abort
    let l:executable = ale#swift#GetAppleSwiftFormatExecutable(a:buffer)
    let l:command_args = ''

    if ale#Var(a:buffer, 'swift_appleswiftformat_use_swiftpm')
        let l:command_args = ' ' . 'run swift-format'
    endif

    return ale#Escape(l:executable) . l:command_args
endfunction

" Locate the nearest '.swift-format' configuration file, and return the
" arguments, else return an empty string.
function! ale#swift#GetAppleSwiftFormatConfigArgs(buffer) abort
    let l:config_filepath = ale#path#FindNearestFile(a:buffer, '.swift-format')

    if l:config_filepath isnot# ''
        return '--configuration' . ' ' . l:config_filepath
    endif

    return ''
endfunction

" }}}
