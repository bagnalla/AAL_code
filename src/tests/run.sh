#!/bin/sh

for f in `ls *.aal`; do
    rm -f $f.result;
    ../aalc.native -i $f --dump-tycheck --dump-marshal;
    # diff $f.expected $f.result;
    # if [ $? -eq 0 ]; then 
    # 	echo "$f matches expected"
    # else 
    # 	echo "*** $f FAILED: output does not match expected ***"
    # fi
done
