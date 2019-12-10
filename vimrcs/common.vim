" Get the file path of the current citc client.
" If it is not a citc client, return the entire path.
function! GetSmartFilePath()
  let path = expand('%:p:h')
  " If in google3 path, display the client name and the absolute path
  if matchstr(path, 'google3')  == 'google3'
    let output = ''
    let subs = split(path, '/')
    let add_to_output = 0
    let prev_s = ''
    for s in subs
        if add_to_output == 1
            let output .= '/'
            let output .= s
        endif
        if s == 'google3' 
            let output .= prev_s
            let output .= ':'
            let add_to_output = 1
        endif 
        let prev_s = s
    endfor
    return output
  else
    return path
  endif
endfunction
