#!/bin/bash
x=$(smap -c | grep 'dtolley' | wc -l)
while [ $x -ge 220 ] 
do
  sleep 60     #sleep for 1 minute
  x=$(smap -c | grep 'dtolley' | wc -l)
done