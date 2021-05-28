How To
======

Install the required build-dependencies: `sudo apt-get install --no-install-recommends fakeroot lintian debhelper`

You also need the packages from Sourceforge:
[amd64](https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.0.12/)
[i386](https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20i386%20DEB/Lazarus%202.0.12/)

Copy the directory `debian` and the script `buildpackage.sh` into QuickHash's source directory and run `./buildpackage.sh`.
Remember to keep the changelog up to date.
