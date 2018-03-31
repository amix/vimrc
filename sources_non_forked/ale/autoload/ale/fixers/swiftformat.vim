" Author: gfontenot (Gordon Fontenot) <gordon@fonten.io>
" Description: Integration of SwiftFormat with ALE.

call ale#Set('swift_swiftformat_executable', 'swiftformat')
call ale#Set('swift_swiftformat_use_global', 0)
call ale#Set('swift_swiftformat_options', '')

function! ale#fixers#swiftformat#GetExecutable(buffer) abort
    return ale#node#FindExecutable(a:buffer, 'swift_swiftformat', [
    \   'Pods/SwiftFormat/CommandLineTool/swiftformat',
    \   'ios/Pods/SwiftFormat/CommandLineTool/swiftformat',
    \   'swiftformat',
    \])
endfunction

function! ale#fixers#swiftformat#Fix(buffer) abort
    let l:options = ale#Var(a:buffer, 'swift_swiftformat_options')

    return {
    \   'read_temporary_file': 1,
    \   'command': ale#Escape(ale#fixers#swiftformat#GetExecutable(a:buffer))
    \       . ' %t'
    \       . ' ' . l:options,
    \}
endfunction
