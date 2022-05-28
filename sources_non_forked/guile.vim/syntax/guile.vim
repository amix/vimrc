" License:  The MIT License (MIT) {{{
"    Copyright (c) 2019 HiPhish
"
"    Permission is hereby granted, free of charge, to any person obtaining a
"    copy of this software and associated documentation files (the
"    "Software"), to deal in the Software without restriction, including
"    without limitation the rights to use, copy, modify, merge, publish,
"    distribute, sublicense, and/or sell copies of the Software, and to permit
"    persons to whom the Software is furnished to do so, subject to the
"    following conditions:
"
"    The above copyright notice and this permission notice shall be included
"    in all copies or substantial portions of the Software.
"
"    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
"    NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
"    DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
"    OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
"    USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}

" GNU Guile syntax highlighting for extensions to Scheme
scriptencoding utf-8


" =============================================================================
" Multi-line comments, used for the shebang
syntax region guileComment start='\v<#!' end='\v!#'

" Keywords
syntax match guileKeyword '\v<#:[^ ()]+>'


" ===[ Special keywords ]======================================================
" Special keywords
syntax keyword guileSyntax define*
syntax keyword guileSyntax define-public
syntax keyword guileSyntax define-module
syntax keyword guileSyntax define-accessor
syntax keyword guileSyntax define-class
syntax keyword guileSyntax define-enumeration
syntax keyword guileSyntax define-inlinable
syntax keyword guileSyntax define-syntax-parameter

syntax keyword guileSyntax λ
syntax keyword guileSyntax lambda*

syntax keyword guileSyntax use-modules

syntax keyword guileSyntax call-with-input-file
syntax keyword guileSyntax call-with-input-string
syntax keyword guileSyntax call-with-output-file
syntax keyword guileSyntax call-with-output-string
syntax keyword guileSyntax call-with-prompt
syntax keyword guileSyntax call-with-trace

syntax keyword guileSyntax eval-when

syntax keyword guileSyntax syntax-parameterize

syntax keyword guileSyntax with-error-to-file
syntax keyword guileSyntax with-error-to-port
syntax keyword guileSyntax with-error-to-string
syntax keyword guileSyntax with-fluid*
syntax keyword guileSyntax with-fluids
syntax keyword guileSyntax with-fluids*
syntax keyword guileSyntax with-input-from-port
syntax keyword guileSyntax with-input-from-string
syntax keyword guileSyntax with-output-to-port
syntax keyword guileSyntax with-output-to-string

" Macros
syntax keyword guileSyntaxSyntax define-syntax-rule


" ===[ Literals ]==============================================================
" Boolean literals
syntax keyword guileBoolean #true
syntax keyword guileBoolean #false

" Unspecified literal (e.g. the return value of '(if #f #f)')
syntax match guileConstant '\v<#\<unspecified\>>'

" Byte vector literal
syntax match guileQuote '\v<\zs#vu8\ze\('

" Number literals
syntax match guileNumber '\v<#[bB][+-]?[0-1]+>'
syntax match guileNumber '\v<#[oO][+-]?[0-7]+>'
syntax match guileNumber '\v<#[dD][+-]?\d+>'
syntax match guileNumber '\v<#[xX][+-]?[0-9a-fA-F]+>'
syntax match guileNumber '\v<#[eE][+-]?(\d+\.\d*|\d*\.\d+|\d+)>'  " Exact
syntax match guileNumber '\v<(\+|-)(inf|nan)\.0>'  " NaN and infinity


" =============================================================================
highlight link guileComment        schemeComment
highlight link guileQuote          schemeQuote
highlight link guileSyntax         schemeSyntax
highlight link guileSyntaxSyntax   schemeSyntaxSyntax
highlight link guileBoolean        schemeBoolean
highlight link guileConstant       schemeConstant
highlight link guileNumber         schemeNumber
highlight link guileKeyword        Type
