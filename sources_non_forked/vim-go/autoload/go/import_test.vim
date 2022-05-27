" don't spam the user when Vim is started in Vi compatibility mode
let s:cpo_save = &cpo
set cpo&vim

func! Test_SwitchImportAddIgnoresCommented()
  try
    let l:tmp = gotest#write_file('import/import.go', [
          \ 'package import',
          \ '',
          \ 'import (',
          \ "\t" . '// "fmt"',
          \ "\t" . '"io"',
          \ "\t" . '"ioutil"',
          \ "\t" . '"os"',
          \ ')',
          \ '',
          \ 'func main() {',
          \ ' io.Copy(ioutil.Discard, os.Stdin)',
          \ ' fmt.Println("import the package")',
          \ '}',
        \ ])
    call go#import#SwitchImport(1, '', 'fmt', 0)

    let l:actual = getline(4)
    call assert_equal("\t" . '"fmt"', l:actual)
  finally
    call delete(l:tmp, 'rf')
  endtry
endfunc

" restore Vi compatibility settings
let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 ts=2 et
