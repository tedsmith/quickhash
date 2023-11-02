#!/bin/sh

export LANG=C

set -e

show_deps() {
    echo ""
    file "$1"
    echo "dependencies:"
    objdump -x "$1" 2>/dev/null | sed -nr 's|.*DLL Name: (.*).*|\1|p'
}

build_libewf()
{
    host="$1"
    arch="x86"

    if [ "$host" = "x86_64-w64-mingw32" ]; then
        arch="x64"
    fi

    ver="20230212"
    file="libewf-experimental-$ver.tar.gz"
    test -f $file || wget https://github.com/libyal/libewf/releases/download/$ver/$file
    rm -rf libewf-$ver
    tar xf $file
    cd libewf-$ver

    ./configure --prefix="$PWD/temp" --host="$host" \
        --disable-shared \
        --enable-static \
        --enable-static-executables \
        --enable-winapi \
        --enable-wide-character-type

    make -j4
    make install-strip

    # must link DLL manually
    mkdir temp/objs
    ar x temp/lib/libewf.a --output temp/objs
    ${host}-gcc -shared temp/objs/*.o -o ../libs/$arch/libewf-${arch}.dll -static -s \
        -Wl,--export-all-symbols \
        -Wl,--enable-auto-import \
        -Wl,--enable-auto-image-base

    cd ..
}

mkdir -p libs/x86 libs/x64

build_libewf i686-w64-mingw32
build_libewf x86_64-w64-mingw32

echo ""
show_deps libs/x86/libewf-x86.dll
show_deps libs/x64/libewf-x64.dll
