#!/bin/sh

input="MainLogo-Minimal-New.jpg"

for size in 16 24 32 48 64 96 128 ; do
  rm -f quickhash_${size}.png
  convert $input -filter Lanczos -resize ${size}x${size} quickhash_${size}.png
done

rm -f QuickHash.ico
convert $input QuickHash.ico

