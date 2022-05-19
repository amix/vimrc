let s:mkdp_root_dir = expand('<sfile>:h:h:h')

function! health#mkdp#check() abort
  call health#report_info('Platform: ' . mkdp#util#get_platform())
  let l:info = system('nvim --version')
  call health#report_info('Nvim Version: '. split(l:info, '\n')[0])
  let l:mkdp_server_script = s:mkdp_root_dir . '/app/bin/markdown-preview-' . mkdp#util#get_platform()
  if executable(l:mkdp_server_script)
    call health#report_info('Pre build: ' . l:mkdp_server_script)
    call health#report_info('Pre build version: ' . mkdp#util#pre_build_version())
    call health#report_ok('Using pre build')
  elseif executable('node')
    call health#report_info('Node version: ' . system('node --version'))
    let l:mkdp_server_script = s:mkdp_root_dir . '/app/server.js'
    call health#report_info('Script: ' . l:mkdp_server_script)
    call health#report_info('Script exists: ' . filereadable(l:mkdp_server_script))
    call health#report_ok('Using node')
  endif
endfunction
