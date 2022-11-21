" Author: (bosr) <bosr@bosr.cc>
" Description: Integration of apple/swift-format formatter with ALE.

function! ale#fixers#appleswiftformat#Fix(buffer) abort
    let l:command_args = ale#swift#GetAppleSwiftFormatCommand(a:buffer) . ' format --in-place %t'
    let l:config_args = ale#swift#GetAppleSwiftFormatConfigArgs(a:buffer)

    if l:config_args isnot# ''
        let l:command_args = l:command_args . ' ' . l:config_args
    endif

    return {
    \   'read_temporary_file': 1,
    \   'command': l:command_args,
    \}
endfunction
