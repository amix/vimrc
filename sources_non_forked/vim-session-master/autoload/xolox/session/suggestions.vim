" Example function for session name suggestions.
"
" Author: Peter Odding
" Last Change: July 6, 2014
" URL: http://peterodding.com/code/vim/session/

function! xolox#session#suggestions#vcs_feature_branch() " {{{1
  " This function implements an example of a function that can be used with
  " the `g:session_name_suggestion_function` option. It finds the name of the
  " current git or Mercurial feature branch (if any) and suggests this name as
  " the name for the session that is being saved with :SaveSession. Returns a
  " list with one string on success and an empty list on failure.
  let [kind, directory] = xolox#session#suggestions#find_vcs_repository()
  if kind == 'git'
    let command = 'git rev-parse --abbrev-ref HEAD'
    let names_to_ignore = ['master']
  elseif kind == 'hg'
    let command = 'hg branch'
    let names_to_ignore = ['default']
  else
    return []
  endif
  let result = xolox#misc#os#exec({'command': command, 'check': 0})
  if result['exit_code'] == 0 && !empty(result['stdout'])
    let branch_name = xolox#misc#str#trim(result['stdout'][0])
    if !empty(branch_name) && index(names_to_ignore, branch_name) == -1
      return [xolox#misc#str#slug(branch_name)]
    endif
  endif
  return []
endfunction

function! xolox#session#suggestions#find_vcs_repository()
  for name in ['git', 'hg']
    let match = finddir('.' . name, '.;')
    if !empty(match)
      let directory = fnamemodify(match, ':h')
      return [name, directory]
    endif
  endfor
  return ['', '']
endfunction
