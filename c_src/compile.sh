#!/bin/sh

if [ ! -d c_src/sss ]; then
    git clone --recursive https://github.com/dsprenkels/sss.git c_src/sss
fi

cd c_src/sss

make
