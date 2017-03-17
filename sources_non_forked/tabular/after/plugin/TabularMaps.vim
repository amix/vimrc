" Copyright (c) 2016, Matthew J. Wozniski
" All rights reserved.
"
" Redistribution and use in source and binary forms, with or without
" modification, are permitted provided that the following conditions are met:
"     * Redistributions of source code must retain the above copyright notice,
"       this list of conditions and the following disclaimer.
"     * Redistributions in binary form must reproduce the above copyright
"       notice, this list of conditions and the following disclaimer in the
"       documentation and/or other materials provided with the distribution.
"     * The names of the contributors may not be used to endorse or promote
"       products derived from this software without specific prior written
"       permission.
"
" THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER ``AS IS'' AND ANY EXPRESS
" OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
" OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
" NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY DIRECT, INDIRECT,
" INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
" LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
" OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
" LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
" NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
" EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

if !exists(':Tabularize') || get(g:, 'no_default_tabular_maps', 0)
  finish " Tabular.vim wasn't loaded or the default maps are unwanted
endif

let s:save_cpo = &cpo
set cpo&vim

AddTabularPattern!  assignment      /[|&+*/%<>=!~-]\@<!\([<>!=]=\|=\~\)\@![|&+*/%<>=!~-]*=/l1r1
AddTabularPattern!  two_spaces      /  /l0

AddTabularPipeline! multiple_spaces /  / map(a:lines, "substitute(v:val, '   *', '  ', 'g')") | tabular#TabularizeStrings(a:lines, '  ', 'l0')

AddTabularPipeline! argument_list   /(.*)/ map(a:lines, 'substitute(v:val, ''\s*\([(,)]\)\s*'', ''\1'', ''g'')')
                                       \ | tabular#TabularizeStrings(a:lines, '[(,)]', 'l0')
                                       \ | map(a:lines, 'substitute(v:val, ''\(\s*\),'', '',\1 '', "g")')
                                       \ | map(a:lines, 'substitute(v:val, ''\s*)'', ")", "g")')

function! SplitCDeclarations(lines)
  let rv = []
  for line in a:lines
    " split the line into declaractions
    let split = split(line, '\s*[,;]\s*')
    " separate the type from the first declaration
    let type = substitute(split[0], '\%(\%([&*]\s*\)*\)\=\k\+$', '', '')
    " add the ; back on every declaration
    call map(split, 'v:val . ";"')
    " add the first element to the return as-is, and remove it from the list
    let rv += [ remove(split, 0) ]
    " transform the other elements by adding the type on at the beginning
    call map(split, 'type . v:val')
    " and add them all to the return
    let rv += split
  endfor
  return rv
endfunction

AddTabularPipeline! split_declarations /,.*;/ SplitCDeclarations(a:lines)

AddTabularPattern! ternary_operator /^.\{-}\zs?\|:/l1

AddTabularPattern! cpp_io /<<\|>>/l1

AddTabularPattern! pascal_assign /:=/l1

AddTabularPattern! trailing_c_comments /\/\*\|\*\/\|\/\//l1

let &cpo = s:save_cpo
unlet s:save_cpo
