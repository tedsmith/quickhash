#!/bin/sh

mkdir -p x64 x86

url="https://github.com/darealshinji/quickhash-libs/releases/download/20231102"

wget "$url/libewf-Linux-x64.so"
wget "$url/libewf-x64.dll"
wget "$url/libewf-x86.dll"
wget "$url/sqlite3-win32.dll"
wget "$url/sqlite3-win64.dll"
wget "$url/sha512sums.txt"

sha512sum -c sha512sums.txt | exit 1
echo "checksums are OK"

mv -f *64.* x64/
mv *.dll x86/
