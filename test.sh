#!/bin/sh
mkdir -p bootstrap/
cp -R src/build bootstrap/
fsi --exec test.fsx "$@"

