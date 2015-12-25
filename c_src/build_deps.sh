#!/bin/sh

LIBRSYNC_VSN="v2.0.0"

set -e

if [ `basename $PWD` != "c_src" ]; then
    # originally "pushd c_src" of bash
    # but no need to use directory stack push here
    cd c_src
fi

BASEDIR="$PWD"

case "$1" in
    rm-deps)
        rm -rf librsync
        ;;

    clean)
        if [ -d librsync ]; then
            (cd librsync && make clean)
        fi
        ;;

    get-deps)
        if [ ! -d librsync ]; then
            git clone https://github.com/librsync/librsync.git
            (cd librsync && git checkout $LIBRSYNC_VSN)
        fi

        ;;
    *)
        if [ ! -d librsync ]; then
            git clone https://github.com/librsync/librsync.git
            (cd librsync && git checkout $LIBRSYNC_VSN)
        fi
        (cd librsync && cmake . && make)

        ;;

esac
