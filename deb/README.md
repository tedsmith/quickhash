How To
======

Install the required build-dependencies: `sudo apt install --no-install-recommends build-essential fakeroot lintian debhelper`

You also need the packages from Sourceforge:
[amd64](https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/)
[i386](https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20i386%20DEB/)

Copy the directory `debian` and the script `buildpackage.sh` into QuickHash's source directory and run `./buildpackage.sh`.
Remember to keep the changelog up to date.
