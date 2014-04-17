#!/bin/sh
ROOTDIR=$(dirname "$0")
ROOTDIR=$(dirname "$ROOTDIR")/Resources
BINDIR=$ROOTDIR/bin
EMU=beam
PROGNAME=$(basename $0)
export EMU
export ROOTDIR
export BINDIR
export PROGNAME
exec "$BINDIR/erlexec" -smp -noshell -run wings_start start_halt ${1+"$@"}
