#!/bin/bash

set -e

cd $(dirname $0)

# install dependencies
go get github.com/fatih/gomodifytags
go get golang.org/x/tools/cmd/goimports

# cleanup test.log
if [ -f "test.log" ]; then
   rm test.log
fi

if [ -f "FAILED" ]; then
   rm FAILED
fi

for test_file in ../autoload/go/*_test.vim
do
	vim -u NONE -S runtest.vim $test_file
done

if [ -f "test.log" ]; then
   cat test.log
fi

# if Failed exists, test failed
if [ -f "FAILED" ]; then
  echo 2>&1 "FAIL"
  exit 1
fi
echo 2>&1 "PASS"

# Run vimhelplint
[ -d vim-vimhelplint ] || git clone https://github.com/machakann/vim-vimhelplint
echo "Running vimhelplint"
lint=$(vim -esN --cmd 'set rtp+=./vim-vimhelplint' -c 'filetype plugin on' \
        -c 'e ../doc/vim-go.txt' -c 'verb VimhelpLintEcho' -c q 2>&1)
if [ -n "$lint" ]; then 
       exit 1
else
       exit 0
fi
