function! CurrentBufferIsModule(module_name)
    return EndsWith(bufname('%'), a:module_name.'.py')
endfunction


function EndsWith(string, end)
    let l:should = len(a:string) - strlen(a:end)
    return l:should == stridx(a:string, a:end, should)
endfunction

" vim: et:ts=4:sw=4
