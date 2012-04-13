#!/bin/sh

set -ex

TARGET=$1
PREFIX=$HOME/coq/$TARGET

COQVER="$2"
COQ=coq-$COQVER
SSR="$3"

mkdir -p build
cd build

wget -nc http://coq.inria.fr/distrib/V$COQVER/files/$COQ.tar.gz
rm -rf "$COQ"
tar zxvf $COQ.tar.gz

cd $COQ

./configure -prefix $PREFIX -coqide no -with-doc no -with-geoproof no -browser /bin/false
make
make install
cd ..

[ -z "$SSR" ] && exit

wget -nc http://www.msr-inria.inria.fr/Projects/math-components/$SSR.tar.gz
rm -rf "$SSR"
tar zxvf $SSR.tar.gz

cd $SSR

PATH=$PATH:$PREFIX/bin
make
make install
