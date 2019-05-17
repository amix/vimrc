" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

let s:templatepath = go#util#Join(expand('<sfile>:p:h:h:h'), '.github', 'ISSUE_TEMPLATE.md')

function! go#issue#New() abort
  let body = go#uri#Encode(s:issuebody())
  let url = "https://github.com/fatih/vim-go/issues/new?body=" . l:body
  call go#util#OpenBrowser(l:url)
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

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
