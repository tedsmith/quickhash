#How To

Install the required build-dependencies:
`sudo apt-get install --no-install-recommends fakeroot lintian debhelper fpc lazarus lcl lcl-utils`

Copy the directory `debian` and the script `buildpackage.sh` into QuickHash's source directory and run `./buildpackage.sh`.
Remember to keep the changelog up to date.
