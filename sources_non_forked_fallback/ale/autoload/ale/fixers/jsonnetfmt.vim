" Authors: Trevor Whitney <trevorjwhitney@gmail.com> and Takuya Kosugiyama <re@itkq.jp>
" Description: Integration of jsonnetfmt with ALE.

call ale#Set('jsonnet_jsonnetfmt_executable', 'jsonnetfmt')
call ale#Set('jsonnet_jsonnetfmt_options', '')

function! ale#fixers#jsonnetfmt#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'jsonnet_jsonnetfmt_executable')
    let l:options = ale#Var(a:buffer, 'jsonnet_jsonnetfmt_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' -i'
    \       . ale#Pad(l:options)
    \       . ' %t',
    \   'read_temporary_file': 1,
    \}
endfunction
