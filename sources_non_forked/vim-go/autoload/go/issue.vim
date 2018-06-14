let s:templatepath = go#util#Join(expand('<sfile>:p:h:h:h'), '.github', 'ISSUE_TEMPLATE.md')

function! go#issue#New() abort
  let body = substitute(s:issuebody(), '[^A-Za-z0-9_.~-]', '\="%".printf("%02X",char2nr(submatch(0)))', 'g')
  let url = "https://github.com/fatih/vim-go/issues/new?body=" . l:body
  call go#tool#OpenBrowser(l:url)
endfunction

function! s:issuebody() abort
  let lines = readfile(s:templatepath)

  let rtrimpat = '[[:space:]]\+$'
  let body = []
  for l in lines
    let body = add(body, l)

    if l =~ '^\* Vim version'
      redir => out
        silent version
      redir END
      let body = extend(body, split(out, "\n")[0:2])
    elseif l =~ '^\* Go version'
      let [out, err] = go#util#Exec(['go', 'version'])
      let body = add(body, substitute(l:out, rtrimpat, '', ''))
    elseif l =~ '^\* Go environment'
      let [out, err] = go#util#Exec(['go', 'env'])
      let body = add(body, substitute(l:out, rtrimpat, '', ''))
    endif
  endfor

  return join(body, "\n")
endfunction

" vim: sw=2 ts=2 et
