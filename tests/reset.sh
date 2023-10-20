#!/bin/sh



#
# reset
#
for i in $(ls *.st); do
    mv $i.expect.out $i.expect
done

