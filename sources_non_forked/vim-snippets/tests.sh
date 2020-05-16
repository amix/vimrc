#!/usr/bin/env bash

check=0

function test_space_indented {
  local spaced
  spaced=$(grep -REn '^ ' --include '*.snippets' snippets)

  if [[ $? -ne 1 ]]; then
    echo "These snippet lines are indented with spaces:"
    echo "$spaced"
    echo
    (( check++ ))
  fi
}

function test_snipmate_format {
  local ultisnips_in_snipmate
  ultisnips_in_snipmate=$(grep -REn 'endsnippet' --include '*.snippets' snippets)
  if [[ $? -ne 1 ]]; then
    echo "These snippet definitions are probably in UltiSnips format but stored in the snipmate directory"
    echo "$ultisnips_in_snipmate"
    echo
    (( check++ ))
  fi
}

test_space_indented
test_snipmate_format

if [ $check -eq 0 ]; then
  echo "Tests passed!"
  exit 0
else
  echo "$check test(s) failed out of 2!"
  exit 1
fi
