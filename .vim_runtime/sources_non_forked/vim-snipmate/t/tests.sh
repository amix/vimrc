#!/bin/sh

tmp="$(mktemp || tmpfile)"
vim -Es $tmp <<- EOF
    source ~/.vimrc
    %delete _
    call append(0, split(&rtp, ','))
    delete _
    wq
EOF

rtp="$(grep -iE 'vspec|snipmate|tlib|mw-utils' < $tmp | grep -v after)"
vspec="$(grep -iE 'vspec' < $tmp | grep -v after)"
test_files="${*:-parser jumping}"

for test in $test_files; do
    $vspec/bin/vspec $rtp ${test%%.vim}.vim
done

rm $tmp
