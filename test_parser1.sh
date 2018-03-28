#!/usr/bin/env bash
for prog in $(ls tests/test*.prog)
do
  echo -e "$prog\t" $(./$1 $prog out)
done
