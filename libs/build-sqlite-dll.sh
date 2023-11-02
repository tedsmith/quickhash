#!/bin/sh

export LANG=C

set -e

show_deps() {
    echo ""
    file "$1"
    echo "dependencies:"
    objdump -x "$1" 2>/dev/null | sed -nr 's|.*DLL Name: (.*).*|\1|p'
}

build_sqlite3()
{
    dir="sqlite-autoconf-3440000"
    rm -rf ${dir}
    wget -c https://www.sqlite.org/2023/${dir}.tar.gz
    tar xf ${dir}.tar.gz
    cd ${dir}

    host="$1"
    arch="x86"
    bits="32"

    if [ "$host" = "x86_64-w64-mingw32" ]; then
        arch="x64"
        bits="64"
    fi

    LDFLAGS="-static-libgcc" ./configure --prefix="$PWD/temp" --host="$host" --enable-shared --disable-static
    make -j1
    make install-strip
    cp -f temp/bin/libsqlite3-0.dll ../libs/$arch/sqlite3-win${bits}.dll

    cd ..
}

mkdir -p libs/x86 libs/x64

build_sqlite3 i686-w64-mingw32
build_sqlite3 x86_64-w64-mingw32

echo ""
show_deps libs/x86/sqlite3-win32.dll
show_deps libs/x64/sqlite3-win64.dll
