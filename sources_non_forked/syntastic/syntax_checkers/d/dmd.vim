"============================================================================
"File:        d.vim
"Description: Syntax checking plugin for syntastic.vim
"Maintainer:  Alfredo Di Napoli <alfredo dot dinapoli at gmail dot com>
"License:     Based on the original work of Gregor Uhlenheuer and his
"             cpp.vim checker so credits are dued.
"             THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
"             EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
"             OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
"             NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
"             HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
"             WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
"             FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
"             OTHER DEALINGS IN THE SOFTWARE.
"
"============================================================================

if exists('g:loaded_syntastic_d_dmd_checker')
    finish
endif
let g:loaded_syntastic_d_dmd_checker = 1

if !exists('g:syntastic_d_compiler_options')
    let g:syntastic_d_compiler_options = ''
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_d_dmd_IsAvailable() dict
    if !exists('g:syntastic_d_compiler')
        let g:syntastic_d_compiler = self.getExec()
    endif
    call self.log('g:syntastic_d_compiler =', g:syntastic_d_compiler)
    return executable(expand(g:syntastic_d_compiler))
endfunction

function! SyntaxCheckers_d_dmd_GetLocList() dict
    if !exists('g:syntastic_d_include_dirs')
        let g:syntastic_d_include_dirs = filter(glob($HOME . '/.dub/packages/*', 1, 1), 'isdirectory(v:val)')
        call map(g:syntastic_d_include_dirs, 'isdirectory(v:val . "/source") ? v:val . "/source" : v:val')
        call add(g:syntastic_d_include_dirs, './source')
    endif

    return syntastic#c#GetLocList('d', 'dmd', {
        \ 'errorformat':
        \     '%-G%f:%s:,%f(%l): %m,' .
        \     '%f:%l: %m',
        \ 'main_flags': '-c -of' . syntastic#util#DevNull(),
        \ 'header_names': '\m\.di$' })
endfunction

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'd',
    \ 'name': 'dmd' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set et sts=4 sw=4:
