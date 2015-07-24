source plugin/jedi.vim
source test/utils.vim

describe 'pyimport'
    before
        let g:jedi#use_tabs_not_buffers = 1
    end

    after
        bd!
        bd!
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
        Expect jedi#py_import_completions('zip', 0, 0) == "zipfile\nzipimport"
    end
end
