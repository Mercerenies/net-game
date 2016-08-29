#!/bin/bash

echo "[]" | ./bash/stage3.sh >./temp/alpha.txt

./bash/stage0.sh 9321 &
./bash/stage4.sh -C 9321 -f ./temp/alpha.txt
