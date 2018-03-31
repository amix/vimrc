" Author: Jeff Willette <jrwillette88@gmail.com>
" Description: Integration of importjs with ALE.

call ale#Set('js_importjs_executable', 'importjs')

function! ale#fixers#importjs#ProcessOutput(buffer, output) abort
    let l:result = ale#util#FuzzyJSONDecode(a:output, [])
    return split(get(l:result, 'fileContent', ''), "\n")
endfunction

function! ale#fixers#importjs#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'js_importjs_executable')

    if !executable(l:executable)
        return 0
    endif

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' fix'
    \       . ' %s',
    \   'process_with': 'ale#fixers#importjs#ProcessOutput',
    \}
endfunction
