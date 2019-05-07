#!/bin/sh

cd c_src
if [ ! -d sss ]; then
    git clone --recursive https://github.com/dsprenkels/sss.git
fi

if [ ! -f c_src/libsss.a ]; then
    (cd sss ; make)
fi

make
