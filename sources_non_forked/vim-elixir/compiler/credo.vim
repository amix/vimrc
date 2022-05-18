if exists('current_compiler')
    finish
endif
let current_compiler = 'credo'

if exists(":CompilerSet") != 2
    command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet errorformat=%f:%l:%c:\ %t:\ %m,%f:%l:\ %t:\ %m
CompilerSet makeprg=mix\ credo\ suggest\ --format=flycheck
