function! emmet#lang#less#findTokens(str) abort
  return emmet#lang#html#findTokens(a:str)
endfunction

function! emmet#lang#less#parseIntoTree(abbr, type) abort
  return emmet#lang#scss#parseIntoTree(a:abbr, a:type)
endfunction

function! emmet#lang#less#toString(settings, current, type, inline, filters, itemno, indent) abort
  return emmet#lang#scss#toString(a:settings, a:current, a:type, a:inline, a:filters, a:itemno, a:indent)
endfunction

function! emmet#lang#less#imageSize() abort
  call emmet#lang#css#imageSize()
endfunction

function! emmet#lang#less#encodeImage() abort
  return emmet#lang#css#encodeImage()
endfunction

function! emmet#lang#less#parseTag(tag) abort
  return emmet#lang#css#parseTag(a:tag)
endfunction

function! emmet#lang#less#toggleComment() abort
  call emmet#lang#css#toggleComment()
endfunction

function! emmet#lang#less#balanceTag(flag) range abort
  call emmet#lang#scss#balanceTag(a:flag)
endfunction

function! emmet#lang#less#moveNextPrevItem(flag) abort
  return emmet#lang#less#moveNextPrev(a:flag)
endfunction

function! emmet#lang#less#moveNextPrev(flag) abort
  call emmet#lang#scss#moveNextPrev(a:flag)
endfunction

function! emmet#lang#less#splitJoinTag() abort
  call emmet#lang#css#splitJoinTag()
endfunction

function! emmet#lang#less#removeTag() abort
  call emmet#lang#css#removeTag()
endfunction
