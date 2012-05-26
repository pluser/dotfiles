#!/bin/sh
#
# @(#) mvwithinode.sh Ver.1.0.0 2012.05.26
#
# Usarge:
#  mvwithinode.sh source [source...] dest
#   source - source file
#   dest - destination directory
#
# Description:
#  This is very thin wrapper for mv command with inode.
#
#####

if [ $# -lt 2 ]; then
    echo "Userge: `basename $0` source destination" 1>&2
    exit 1
fi

SOURCE_INUM=`ls -i1 "$@" | head -n -1 | cut -d' ' -f1`
DESTINATION=`ls -d1 "$@" | tail -n 1`

for inum in $SOURCE_INUM; do
    find -inum ${inum} -exec mv {} -t $DESTINATION \; # For ONLY GNU Coreutils
done
