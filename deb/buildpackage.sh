#!/bin/bash

LANG=C
LANGUAGE=C
LC_ALL=C
QUILT_PATCHES="debian/patches"

packages="$(grep '^Package: ' debian/control | cut -d ' ' -f2)"
source=$(dpkg-parsechangelog | grep '^Source:' | cut -d ' ' -f2)
version=$(dpkg-parsechangelog | grep '^Version:' | cut -d ' ' -f2)
arch=$(dpkg-architecture -qDEB_HOST_ARCH)
log="../build_${source}_${version}_${arch}.log"

# check build dependencies
dpkg-checkbuilddeps || exit 1

rm -f $log
touch $log

# apply patches
if [ ! -f .patches_applied ]; then
  quilt push -a | tee -a $log
  touch .patches_applied
fi

# build package
echo "Note: log filtering will replace '$PWD' with '<<BUILDDIR>>'" | tee -a $log
dpkg-buildpackage -j`nproc` -rfakeroot -b -us -uc 2>&1 | sed "s|$PWD|<<BUILDDIR>>|g" | tee -a $log
echo "" | tee -a $log

# revert patches
if [ -f .patches_applied ]; then
  quilt pop -a && rm .patches_applied
fi

# show infos about package

for p in $packages ; do
  f="../${p}_${version}_${arch}.deb"
  if [ -e $f ]; then
    echo "$(basename $f):"
    dpkg-deb -I $f
    echo ""
  fi
done 2>&1 | sed "s|$PWD|<<BUILDDIR>>|g" | tee -a $log

for p in $packages ; do
  f="../${p}_${version}_${arch}.deb"
  if [ -e $f ]; then
    echo "$(basename $f):"
    dpkg-deb -c $f
    echo ""
  fi
done 2>&1 | sed "s|$PWD|<<BUILDDIR>>|g" | tee -a $log

if [ -x $(which lintian) ]; then
  echo ""
  echo "Lintian checks:"
  echo ""
  for p in $packages ; do
    f="../${p}_${version}_${arch}.deb"
    if [ -e $f ]; then
      echo "$(basename $f):"
      lintian $f
      echo ""
    fi
  done 2>&1 | sed "s|$PWD|<<BUILDDIR>>|g" | tee -a $log
fi

