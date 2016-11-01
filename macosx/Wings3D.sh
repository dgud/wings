#!/bin/sh
pushd `dirname $0` > /dev/null
ROOTDIR=`pwd -P`
popd > /dev/null
ROOTDIR=$(dirname "$ROOTDIR")/Resources
BINDIR=$ROOTDIR/bin
EMU=beam
PROGNAME=$(basename $0)
export EMU
export ROOTDIR
export BINDIR
export PROGNAME
unset ERL_LIBS
exec "$BINDIR/erlexec" -smp -noshell -run wings_start start_halt ${1+"$@"}
