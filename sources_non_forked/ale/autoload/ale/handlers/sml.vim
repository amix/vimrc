" Author: Jake Zimmerman <jake@zimmerman.io>
" Description: Shared functions for SML linters

" The glob to use for finding the .cm file.
"
" See :help ale-sml-smlnj for more information.
call ale#Set('sml_smlnj_cm_file', '*.cm')

function! ale#handlers#sml#GetCmFile(buffer) abort
    let l:pattern = ale#Var(a:buffer, 'sml_smlnj_cm_file')
    let l:as_list = 1

    let l:cmfile = ''

    for l:path in ale#path#Upwards(expand('#' . a:buffer . ':p:h'))
        let l:results = glob(l:path . '/' . l:pattern, 0, l:as_list)

        if len(l:results) > 0
            " If there is more than one CM file, we take the first one
            " See :help ale-sml-smlnj for how to configure this.
            let l:cmfile = l:results[0]
        endif
    endfor

    return l:cmfile
endfunction

" Only one of smlnj or smlnj-cm can be enabled at a time.
" executable_callback is called before *every* lint attempt
function! s:GetExecutable(buffer, source) abort
    if ale#handlers#sml#GetCmFile(a:buffer) is# ''
        " No CM file found; only allow single-file mode to be enabled
        if a:source is# 'smlnj-file'
            return 'sml'
        elseif a:source is# 'smlnj-cm'
            return ''
        endif
    else
        " Found a CM file; only allow cm-file mode to be enabled
        if a:source is# 'smlnj-file'
            return ''
        elseif a:source is# 'smlnj-cm'
            return 'sml'
        endif
    endif
endfunction

function! ale#handlers#sml#GetExecutableSmlnjCm(buffer) abort
    return s:GetExecutable(a:buffer, 'smlnj-cm')
endfunction

function! ale#handlers#sml#GetExecutableSmlnjFile(buffer) abort
    return s:GetExecutable(a:buffer, 'smlnj-file')
endfunction

function! ale#handlers#sml#Handle(buffer, lines) abort
    " Try to match basic sml errors
    " TODO(jez) We can get better errorfmt strings from Syntastic
    let l:out = []
    let l:pattern = '^.*\:\([0-9\.]\+\)\ \(\w\+\)\:\ \(.*\)'
    let l:pattern2 = '^.*\:\([0-9]\+\)\.\?\([0-9]\+\).* \(\(Warning\|Error\): .*\)'

    for l:line in a:lines
        let l:match2 = matchlist(l:line, l:pattern2)

        if len(l:match2) != 0
          call add(l:out, {
          \   'bufnr': a:buffer,
          \   'lnum': l:match2[1] + 0,
          \   'col' : l:match2[2] - 1,
          \   'text': l:match2[3],
          \   'type': l:match2[3] =~# '^Warning' ? 'W' : 'E',
          \})
          continue
        endif

        let l:match = matchlist(l:line, l:pattern)

        if len(l:match) != 0
          call add(l:out, {
          \   'bufnr': a:buffer,
          \   'lnum': l:match[1] + 0,
          \   'text': l:match[2] . ': ' . l:match[3],
          \   'type': l:match[2] is# 'error' ? 'E' : 'W',
          \})
          continue
        endif
    endfor

    return l:out
endfunction

" vim:ts=4:sts=4:sw=4
