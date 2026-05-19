#!/bin/bash
set -e
set -x

export QUILT_PATCHES="debian/patches"
export QUILT_PATCH_OPTS="--binary"

quilt push -a
quilt refresh
quilt pop -a
