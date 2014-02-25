if exists("g:loaded_syntastic_makeprg_autoload")
    finish
endif
let g:loaded_syntastic_makeprg_autoload = 1

"Returns a makeprg of the form
"
"[exe] [args] [filename] [post_args] [tail]
"
"A (made up) example:
"    ruby -a -b -c test_file.rb --more --args > /tmp/output
"
"To generate this you would call:
"
"    let makeprg = syntastic#makeprg#build({
"                \ 'exe': 'ruby',
"                \ 'args': '-a -b -c',
"                \ 'post_args': '--more --args',
"                \ 'tail': '> /tmp/output',
"                \ 'filetype': 'ruby',
"                \ 'subchecker': 'mri' })
"
"Note that the current filename is added by default - but can be overridden by
"passing in an 'fname' arg.
"
"Arguments 'filetype' and 'subchecker' are mandatory, handling of composite
"types and user-defined variables breaks if you omit them.
"
"All other options can be overriden by the user with global variables - even
"when not specified by the checker in syntastic#makeprg#build().
"
"E.g. They could override the checker exe with
"
"   let g:syntastic_ruby_mri_exe="another_ruby_checker_exe.rb"
"
"The general form of the override option is:
"   syntastic_[filetype]_[subchecker]_[option-name]
"
function! syntastic#makeprg#build(opts)
    let builder = g:SyntasticMakeprgBuilder.New(
                \ get(a:opts, 'exe', ''),
                \ get(a:opts, 'args', ''),
                \ get(a:opts, 'fname', ''),
                \ get(a:opts, 'post_args', ''),
                \ get(a:opts, 'tail', ''),
                \ get(a:opts, 'filetype', ''),
                \ get(a:opts, 'subchecker', '') )

    return builder.makeprg()
endfunction
