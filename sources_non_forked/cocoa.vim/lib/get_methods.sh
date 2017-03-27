dir=`dirname $0`
classes=`grep -m 1 ^$1 ${dir}/cocoa_indexes/classes.txt`
if [ -z "$classes" ]; then exit; fi
zgrep "^\($classes\)" ${dir}/cocoa_indexes/methods.txt.gz | sed 's/^[^ ]* //'
