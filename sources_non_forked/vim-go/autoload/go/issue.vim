" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

let s:templatepath = go#util#Join(resolve(expand('<sfile>:p:h:h:h')), '.github', 'ISSUE_TEMPLATE.md')

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

    if l =~ '^<!-- :version'
      let out = execute('version')
      let body = extend(body, split(out, "\n")[0:2])
    elseif l =~ '^<!-- go version -->'
      let [out, err] = go#util#Exec(['go', 'version'])
      let body = add(body, substitute(l:out, rtrimpat, '', ''))
    elseif l =~ '^<!-- go env -->'
      let [out, err] = go#util#ExecInDir(['go', 'env'])
      let body = add(body, substitute(l:out, rtrimpat, '', ''))
    elseif l=~ '^<!-- gopls version -->'
      let [out, err] = go#util#Exec(['gopls', 'version'])
      let body = add(body, substitute(l:out, rtrimpat, '', ''))
    endif
  endfor

  let body = add(body, "\n#### vim-go configuration:\n<details><summary>vim-go configuration</summary><br><pre>")

  for k in keys(g:)
    if k =~ '^go_'
      let body = add(body, 'g:' . k . ' = ' . string(get(g:, k)))
    endif
  endfor

  let body = add(body, '</pre></details>')

  let body = add(body, printf("\n#### filetype detection configuration:\n<details><summary>filetype detection</summary><br><pre>%s", execute('filetype')))
  let body = add(body, '</pre></details>')

  return join(body, "\n")
endfunction

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
