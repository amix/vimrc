source plugin/jedi.vim
source test/_utils.vim

describe 'pyimport'
    before
        let g:jedi#use_tabs_not_buffers = 1
        let g:jedi#project_path = 'autoload'
    end

    after
        try | %bwipeout! | catch | endtry
        unlet g:jedi#project_path
    end

    it 'open_tab'
        Pyimport os 
        Expect CurrentBufferIsModule('os') == 1
        Pyimport subprocess 
        Expect CurrentBufferIsModule('subprocess') == 1
        " the empty tab is sometimes also a tab
        Expect tabpagenr('$') >= 2
    end

    it 'completion'
        " don't know how to test this directly
        "execute "Pyimport subproc\<Tab>"
        "Expect CurrentBufferIsModule('subprocess') == 1

        Expect jedi#py_import_completions('subproc', 0, 0) == 'subprocess'
        Expect jedi#py_import_completions('subprocess', 0, 0) == 'subprocess'
        let g:comp = jedi#py_import_completions('sre_', 0, 0)
        Expect g:comp == "sre_compile\nsre_constants\nsre_parse"
    end
end
