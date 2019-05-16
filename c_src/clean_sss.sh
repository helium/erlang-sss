#!/bin/sh

if [ -d c_src/sss ]; then
    rm -rf c_src/sss/
fi

if [ -f c_src/libsss.a ]; then
    rm c_src/libsss.a
fi

if [ -f c_src/erlang_sss.o ]; then
    rm c_src/erlang_sss.o
fi
