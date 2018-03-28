#!/usr/bin/env bash
for prog in $(ls tests/test*.errs)
do
  echo -e "$prog\t" $(./$1 $prog out)
done
