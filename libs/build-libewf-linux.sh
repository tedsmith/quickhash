#!/bin/sh
set -e

if [ "$(uname -s)" != "Linux" ]; then
    echo "this script is made for Linux, not $(uname -s)"
    exit
fi

if [ "$(uname -m)" = "x86_64" ]; then
    arch="x64"
else
    arch="x86"
fi

ver="20230212"
file="libewf-experimental-$ver.tar.gz"
test -f $file || wget https://github.com/libyal/libewf/releases/download/$ver/$file
rm -rf libewf-$ver
tar xf $file
cd libewf-$ver

./configure --prefix="$PWD/tmp" \
    --enable-shared \
    --disable-static \
    --with-bzip2=no \
    --with-openssl=no \
    --with-libuuid=no \
    --with-libfuse=no

make -j4
make install-strip

mkdir -p ../libs/$arch
cp -f tmp/lib/libewf.so.3 ../libs/$arch/libewf-Linux-${arch}.so

echo ""
file ../libs/$arch/libewf-Linux-${arch}.so
echo ""
LANG=C readelf -d ../libs/$arch/libewf-Linux-${arch}.so
