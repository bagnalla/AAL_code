#!/bin/sh

# This script creates reference output for each program [f] in [ls *.gpy],
# storing the results in [f.expected].

# This script should be run only rarely, when new tests are added for example.
# Be sure, after running this script, to check that the new tests cases are 
# actually producing the correct results!

for f in `ls *.gpy`; do
    ../grumpy.native -i $f;
    clang $f.ll -o $f.out;
    ./$f.out;
    echo $? > $f.expected;
done