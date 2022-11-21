" in sh/bash you can type export to get a list of environment variables
" This function assigns those env vars to Vim.
" Does not delete env vars yet
" Example: env_reload#ReloadEnv(system("sh -c 'export'")
fun! env_reload#ReloadEnv(bash_export_command_output)
  for i in split(a:bash_export_command_output,"\n")
    let m = matchlist(i, 'export \([^=]\+\)="\(.*\)"')
    if empty(m) | continue | endif
    " don't care about quoted values right now.
    exec 'let $'.m[1].'='.string(m[2])
  endfor
endf
