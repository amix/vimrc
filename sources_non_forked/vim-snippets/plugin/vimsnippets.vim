if exists("b:done_vimsnippets")
   finish
endif
let b:done_vimsnippets = 1

" Expanding the path is not needed on Vim 7.4
if &cp || version >= 704
    finish
endif

" Add pythonx to the python search path if needed (i.e. <= Vim 7.3).
if !has("python") && !has("python3")
   finish
end

" This will fail if UltiSnips is not installed.
try
   call UltiSnips#bootstrap#Bootstrap()
catch /E117/
   finish
endtry


" This should have been set by UltiSnips, otherwise something is wrong.
if !exists("g:_uspy")
   finish
end


" Expand our path
let s:SourcedFile=expand("<sfile>")
exec g:_uspy "import vim, os, sys"
exec g:_uspy "sourced_file = vim.eval('s:SourcedFile')"
exec g:_uspy "while not os.path.exists(os.path.join(sourced_file, 'pythonx')):
   \ sourced_file = os.path.dirname(sourced_file)"
exec g:_uspy "module_path = os.path.join(sourced_file, 'pythonx')"
exec g:_uspy "sys.path.append(module_path)"
