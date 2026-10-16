#!/bin/sh

##
##  Generates the init.scm for scheme, or the w3d_we.py file for python.
##
##  gen-init-we-script.sh <file1> <file2> <callable> <modnames> <scm|py>
##
##  Copyright 2025 Edward Blake
##
##  See the file "license.terms" for information on usage and redistribution
##  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
##
##     $Id$
##

dir="$(dirname $0)"

file1=$1; shift
file2=$1; shift
callable=$1; shift
modnames=$1; shift
modtype=$1; shift

if [ ! -f "${file1}" ] || [ ! -f "${callable}" ] || [ ! -f "${modnames}" ]
then
  echo "file does not exist" 1>&2
  exit 1
fi

if [ "${modtype}" != "py" ] && [ "${modtype}" != "scm" ]
then
  echo "not py or scm" 1>&2
  exit 1
fi

cat "${file1}"

escript ${dir}/gen-wrapper-script.escript \
    "${callable}" "${modnames}" "${modtype}"

if [ "${file2}" != "-" ]
then
    if [ ! -f "${file2}" ]
    then
      echo "${file2} not a file" 1>&2
      exit 1
    fi

    cat "${file2}"
fi

