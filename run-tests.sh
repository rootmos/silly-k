#!/bin/bash

CC=./c
DIR=tests
DIFF_STYLE="--side-by-side --width=40"

SUCCESSFUL=0
TESTS=0

RED='\e[0;33m'
GREEN='\e[0;32m'
NC='\033[0m' # No Color

for TC in $DIR/*.sk; do
    NAME=$(basename $TC | sed 's/\.sk$//')
    echo -e "Testing $NAME..."
    sed 's/^/> /' $TC
    BINARY=$DIR/$NAME.bin
    $CC -o $BINARY $TC
    if [ $? -eq 0 ]; then
        INPUT=$DIR/$NAME.input
        [ -f $INPUT ] && sed 's/^/< /' $INPUT
        ( ([ -f $INPUT ] && cat $INPUT) || cat /dev/null) | ./$BINARY | diff $DIFF_STYLE - $DIR/$NAME.expected
        if [ $? -eq 0 ]; then
            echo -e "${GREEN}Ok${NC}\n"
            TESTS=$(expr $TESTS + 1)
            SUCCESSFUL=$(expr $SUCCESSFUL + 1)
        else
            echo -e "${RED}FAILED!${NC}\n"
            TESTS=$(expr $TESTS + 1)
        fi
    else
        echo -e "${RED}COMPILATION ERROR!${NC}\n"
        TESTS=$(expr $TESTS + 1)
    fi
done

echo -e "$SUCCESSFUL/$TESTS"
[ $SUCCESSFUL = $TESTS ]
