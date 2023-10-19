#!/bin/sh

COMPTESTFILES=" tcomp1.st "
PARSETESTFILES="
            tparse1.st \
            tparse2.st \
            tparse3.st \
            tparse4.st \
            tparseSqueakRectangle.st \
            tparseSt80Behavior.st \
            tparseSt80Object.st "

MINISTPROG=./minist-img-gen

for i in $PARSETESTFILES; do
    $MINISTPROG -p ./tests/$i > ./tests/$i.expect.out
    diff ./tests/$i.expect ./tests/$i.expect.out> /dev/null 2>&1 && echo -n "SUCCESS" || echo -n "FAILED"
    echo " parse test: $i"
done

for i in $COMPTESTFILES; do
    $MINISTPROG ./tests/$i > ./tests/$i.expect.out
    diff ./tests/$i.expect ./tests/$i.expect.out > /dev/null 2>&1 && echo -n "SUCCESS" || echo -n "FAILED"
    echo " compile test: $i"

done

