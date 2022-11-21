" Author: w0rp <devw0rp@gmail.com>
" Description: This module implements a function for parsing arguments for
" commands.

" Given a list of valid arguments like ['foo', 'bar'] and a string to parse,
" parse the arguments from the string and return [parsed_args, remainder].
"
" Arguments must be prefixed in the string with a single minus (-), and a
" double minus (--) denotes the end of arguments.
function! ale#args#Parse(arg_list, string) abort
    let l:parsed = {}
    let l:end_of_args = 0
    let l:word_list = split(a:string, ' ')
    let l:index = 0

    while l:index < len(l:word_list)
        let l:word = l:word_list[l:index]

        if l:word[:0] is# '-'
            let l:index += 1

            if l:word is# '--'
                break
            endif

            let l:arg = l:word[1:]

            if index(a:arg_list, l:arg) >= 0
                let l:parsed[l:arg] = ''
            else
                throw 'Invalid argument: ' . l:word
            endif
        elseif l:word is# ''
            let l:index += 1
        else
            break
        endif
    endwhile

    let l:new_string = join(l:word_list[l:index :], ' ')

    return [l:parsed, l:new_string]
endfunction
