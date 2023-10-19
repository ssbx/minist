#!/bin/sh

COMPTESTFILES=" tcomp1.st "
PARSETESTFILES="
            tparse1.st \
            tparse2.st \
            tparse3.st "

MINISTPROG=./minist-img-gen
MINISTTMPOUT=/tmp/minist-tests

for i in $PARSETESTFILES; do
    echo -n "running parse test on $i: "
    $MINISTPROG -p ./tests/$i > $MINISTTMPOUT
    diff ./tests/$i.expect $MINISTTMPOUT > /dev/null 2>&1 && echo "SUCCESS" || echo "FAILED"
done

for i in $COMPTESTFILES; do
    echo -n "running compile test on $i: "
    $MINISTPROG ./tests/$i > $MINISTTMPOUT
    diff ./tests/$i.expect $MINISTTMPOUT > /dev/null 2>&1 && echo "SUCCESS" || echo "FAILED"
done

