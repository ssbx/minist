#!/bin/sh


MINISTPROG=./minist-img-gen

#
# reset
#
#for i in $PARSETESTFILES; do
#    mv $i.expect.out $i.expect
#done
#exit
#for i in $COMPTESTFILES; do
#   mv ./tests/$i.expect.out ./tests/$i.expect
#done

for i in $(/bin/ls tests/tp*.st); do
    $MINISTPROG -p $i > $i.expect.out
    diff $i.expect $i.expect.out && echo -n "SUCCESS" || echo -n "FAILED"
    echo " parse test: $i"
done

for i in $(/bin/ls tests/tc*.st); do
    $MINISTPROG $i > $i.expect.out
    diff $i.expect $i.expect.out && echo -n "SUCCESS" || echo -n "FAILED"
    echo " compile test: $i"

done


