#!/bin/sh

LANG=C
LANGUAGE=C
LC_ALL=C

packages="$(grep '^Package: ' debian/control | cut -d ' ' -f2)"
source=$(dpkg-parsechangelog | grep '^Source:' | cut -d ' ' -f2)
version=$(dpkg-parsechangelog | grep '^Version:' | cut -d ' ' -f2)
arch=$(dpkg-architecture -qDEB_HOST_ARCH)
log="../build_${source}_${version}_${arch}.log"

dpkg-checkbuilddeps || exit 1

if [ "$(dpkg-query -W -f='${Status} ${version} ' lazarus-project fpc-laz fpc-src 2>/dev/null; echo $?)" != \
     "install ok installed 3.2.0 install ok installed 3.2.0 install ok installed 2.0.10 0" ];
then
  cat <<EOL
Before you can continue to build Debian packages
you need to install the packages
 lazarus-project version 2.0.10
 fpc-laz version 3.2.0
 fpc-src version 3.2.0
from Sourceforge:
https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.0.10/
https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20i386%20DEB/Lazarus%202.0.10/
EOL
  exit 1
fi

echo "Note: log filtering will replace '$PWD' with '<<BUILDDIR>>'" | tee $log
dpkg-buildpackage -j`nproc` -rfakeroot -b -us -uc 2>&1 | sed "s|$PWD|<<BUILDDIR>>|g" | tee -a $log
echo "" | tee -a $log

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

