call ale#Set('qml_qmlfmt_executable', 'qmlfmt')

function! ale#fixers#qmlfmt#GetExecutable(buffer) abort
    return ale#Var(a:buffer, 'qml_qmlfmt_executable')
endfunction

function! ale#fixers#qmlfmt#Fix(buffer) abort
    return {
    \  'command': ale#Escape(ale#fixers#qmlfmt#GetExecutable(a:buffer)),
    \}
endfunction
