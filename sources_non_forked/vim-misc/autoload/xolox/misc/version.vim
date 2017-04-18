" Version string handling.
"
" Author: Peter Odding <peter@peterodding.com>
" Last Change: June 22, 2013
" URL: http://peterodding.com/code/vim/misc/

function! xolox#misc#version#parse(version_string)
  " Convert a version string to a list of integers.
  let result = map(split(a:version_string, '\.'), 'v:val + 0')
  call xolox#misc#msg#debug("vim-misc %s: Parsed version string %s into %s.", g:xolox#misc#version, string(a:version_string), string(result))
  return result
endfunction

function! xolox#misc#version#at_least(expected_version, available_version)
  " Check whether the second version string is equal to or greater than the
  " first version string. Returns 1 (true) when it is, 0 (false) otherwise.
  let expected_version = xolox#misc#version#parse(a:expected_version)
  let available_version = xolox#misc#version#parse(a:available_version)
  for idx in range(max([len(expected_version), len(available_version)]))
    let expected_number = get(expected_version, idx, 0)
    let available_number = get(available_version, idx, 0)
    if available_number > expected_number
      call xolox#misc#msg#debug("vim-misc %s: Available version (%s) is higher than expected version (%s).", g:xolox#misc#version, a:available_version, a:expected_version)
      return 1
    elseif available_number < expected_number
      call xolox#misc#msg#debug("vim-misc %s: Available version (%s) is lower than expected version (%s).", g:xolox#misc#version, a:available_version, a:expected_version)
      return 0
    endif
  endfor
  call xolox#misc#msg#debug("vim-misc %s: Available version (%s) is equal to expected version (%s).", g:xolox#misc#version, a:available_version, a:expected_version)
  return 1
endfunction

" vim: ts=2 sw=2 et
