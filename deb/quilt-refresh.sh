#!/bin/sh
set -e
set -x
QUILT_PATCHES="debian/patches"
quilt push -a
quilt refresh
quilt pop -a
