let s:save_cpo = &cpoptions
set cpoptions&vim

let s:source = {
      \ 'name': 'decls',
      \ 'description': 'GoDecls implementation for unite',
      \ 'syntax': 'uniteSource__Decls',
      \ 'action_table': {},
      \ 'hooks': {},
      \ }

function! unite#sources#decls#define()
  return s:source
endfunction

function! s:source.gather_candidates(args, context) abort
  let l:bin_path = go#path#CheckBinPath('motion')
  if empty(l:bin_path)
    return []
  endif

  let l:path = expand(get(a:args, 0, '%:p:h'))
  if isdirectory(l:path)
    let l:mode = 'dir'
  elseif filereadable(l:path)
    let l:mode = 'file'
  else
    return []
  endif

  let l:include = get(g:, 'go_decls_includes', 'func,type')
  let l:command = printf('%s -format vim -mode decls -include %s -%s %s', l:bin_path, l:include, l:mode, shellescape(l:path))
  let l:candidates = []
  try
    let l:result = eval(unite#util#system(l:command))
    let l:candidates = get(l:result, 'decls', [])
  catch
    call unite#print_source_error(['command returned invalid response.', v:exception], s:source.name)
  endtry

  return map(l:candidates, "{
        \ 'word': printf('%s :%d :%s', fnamemodify(v:val.filename, ':~:.'), v:val.line, v:val.full),
        \ 'kind': 'jump_list',
        \ 'action__path': v:val.filename,
        \ 'action__line': v:val.line,
        \ 'action__col': v:val.col,
        \ }")
endfunction

function! s:source.hooks.on_syntax(args, context) abort
  syntax match uniteSource__Decls_Filepath /[^:]*\ze:/ contained containedin=uniteSource__Decls
  syntax match uniteSource__Decls_Line /\d\+\ze :/ contained containedin=uniteSource__Decls
  syntax match uniteSource__Decls_WholeFunction /\vfunc %(\([^)]+\) )?[^(]+/ contained containedin=uniteSource__Decls
  syntax match uniteSource__Decls_Function /\S\+\ze(/ contained containedin=uniteSource__Decls_WholeFunction
  syntax match uniteSource__Decls_WholeType /type \S\+/ contained containedin=uniteSource__Decls
  syntax match uniteSource__Decls_Type /\v( )@<=\S+/ contained containedin=uniteSource__Decls_WholeType
  highlight default link uniteSource__Decls_Filepath Comment
  highlight default link uniteSource__Decls_Line LineNr
  highlight default link uniteSource__Decls_Function Function
  highlight default link uniteSource__Decls_Type Type

  syntax match uniteSource__Decls_Separator /:/ contained containedin=uniteSource__Decls conceal
  syntax match uniteSource__Decls_SeparatorFunction /func / contained containedin=uniteSource__Decls_WholeFunction conceal
  syntax match uniteSource__Decls_SeparatorType /type / contained containedin=uniteSource__Decls_WholeType conceal
endfunction

let &cpoptions = s:save_cpo
unlet s:save_cpo

" vim: sw=2 ts=2 et
