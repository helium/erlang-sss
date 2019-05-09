#!/bin/sh

if [ -d c_src/sss ]; then
    rm -rf c_src/sss/
fi

if [ -f c_src/libsss.a ]; then
    rm c_src/libsss.a
fi
