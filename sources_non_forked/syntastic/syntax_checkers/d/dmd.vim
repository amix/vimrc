"============================================================================
"File:        d.vim
"Description: Syntax checking plugin for syntastic
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

if !exists('g:syntastic_d_use_dub')
    let g:syntastic_d_use_dub = 1
endif

if !exists('g:syntastic_d_dub_exec')
    let g:syntastic_d_dub_exec = 'dub'
endif

let s:save_cpo = &cpo
set cpo&vim

function! SyntaxCheckers_d_dmd_IsAvailable() dict " {{{1
    if !exists('g:syntastic_d_compiler')
        let g:syntastic_d_compiler = self.getExec()
    endif
    call self.log('g:syntastic_d_compiler =', g:syntastic_d_compiler)
    return executable(expand(g:syntastic_d_compiler, 1))
endfunction " }}}1

function! SyntaxCheckers_d_dmd_GetLocList() dict " {{{1
    let buf = bufnr('')
    if !exists('g:syntastic_d_include_dirs')
        let g:syntastic_d_include_dirs = s:GetIncludes(self, fnamemodify(bufname(buf), ':p:h'))
    endif

    return syntastic#c#GetLocList('d', 'dmd', {
        \ 'errorformat':
        \     '%-G%f:%s:,%f(%l): %m,' .
        \     '%f:%l: %m',
        \ 'main_flags': '-c -of' . syntastic#util#DevNull(),
        \ 'header_names': '\m\.di$' })
endfunction " }}}1

" Utilities {{{1

function! s:GetIncludes(checker, base) " {{{2
    let includes = []

    if g:syntastic_d_use_dub && !exists('s:dub_ok')
        let s:dub_ok = s:ValidateDub(a:checker)
    endif

    if g:syntastic_d_use_dub && s:dub_ok
        let where = escape(a:base, ' ') . ';'

        let old_suffixesadd = &suffixesadd
        let dirs = syntastic#util#unique(map(filter(
            \   findfile('dub.json', where, -1) +
            \   findfile('dub.sdl', where, -1) +
            \   findfile('package.json', where, -1),
            \ 'filereadable(v:val)'), 'fnamemodify(v:val, ":h")'))
        let &suffixesadd = old_suffixesadd
        call a:checker.log('using dub: looking for includes in', dirs)

        for dir in dirs
            try
                execute 'silent lcd ' . fnameescape(dir)
                let paths = split(syntastic#util#system(syntastic#util#shescape(g:syntastic_d_dub_exec) . ' describe --import-paths'), "\n")
                silent lcd -
                if v:shell_error == 0
                    call extend(includes, paths)
                    call a:checker.log('using dub: found includes', paths)
                endif
            catch /\m^Vim\%((\a\+)\)\=:E472/
                " evil directory is evil
            endtry
        endfor
    endif

    if empty(includes)
        let includes = filter(glob($HOME . '/.dub/packages/*', 1, 1), 'isdirectory(v:val)')
        call map(includes, 'isdirectory(v:val . "/source") ? v:val . "/source" : v:val')
        call add(includes, './source')
    endif

    return syntastic#util#unique(includes)
endfunction " }}}2

function! s:ValidateDub(checker) " {{{2
    let ok = 0

    if executable(g:syntastic_d_dub_exec)
        let command = syntastic#util#shescape(g:syntastic_d_dub_exec) . ' --version'
        let version_output = syntastic#util#system(command)
        call a:checker.log('getVersion: ' . string(command) . ': ' .
            \ string(split(version_output, "\n", 1)) .
            \ (v:shell_error ? ' (exit code ' . v:shell_error . ')' : '') )
        let parsed_ver = syntastic#util#parseVersion(version_output)
        call a:checker.log(g:syntastic_d_dub_exec . ' version =', parsed_ver)
        if len(parsed_ver)
            let ok =  syntastic#util#versionIsAtLeast(parsed_ver, [0, 9, 24])
        endif
    endif

    return ok
endfunction " }}}2

" }}}1

call g:SyntasticRegistry.CreateAndRegisterChecker({
    \ 'filetype': 'd',
    \ 'name': 'dmd' })

let &cpo = s:save_cpo
unlet s:save_cpo

" vim: set sw=4 sts=4 et fdm=marker:
