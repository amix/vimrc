" The next function was based on s:function and s:add_methods in fugitive
" <https://github.com/tpope/vim-fugitive/blob/master/plugin/fugitive.vim>
function! snipmate#util#add_methods(sfile, namespace, methods) abort
	let dict = {}
	for name in a:methods
		let dict[name] = function(join([matchstr(a:sfile, '<SNR>\d\+'),
                    \ a:namespace, name], '_'))
	endfor
	return dict
endfunction

function! snipmate#util#eval(arg)
    try
        let ret = eval(a:arg)
    catch
        echohl ErrorMsg
        echom 'SnipMate:Expression: ' . v:exception
        echohl None
        let ret = ''
    endtry
    return type(ret) == type('') ? ret : string(ret)
endfunction
