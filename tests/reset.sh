#!/bin/sh



#
# reset
#
for i in $(ls *.st.expect); do
    mv $i.out $i
done

