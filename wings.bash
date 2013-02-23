#!/bin/bash

# locate the directory, where this script lives in
SCRIPT_NAME=(basename $0)
SCRIPT_DIR=$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)

# --- display usage information ---------------------------------------
action_usage() {
cat <<EOF
usage: ${BN} [ start | help ]
EOF
}

# --- display extended usage information ------------------------------
action_help() {
cat <<EOF
To be done...
EOF
}

action_start() {
	#exec erl -smp -pa $ESDL_PATH/ebin -pa /home/bjorng/wings-1.0/ebin -run wings_start start_halt ${1+"$@"}
	#exec erl -pa "${SCRIPT_DIR}/ebin" -run wings_start start_halt ${1+"$@"}
	exec erl -pa "${SCRIPT_DIR}/ebin" -run wings_start start_halt $*
}

# === main entry point ================================================
case $1 in
start)
	shift
	action_start $*
;;
help)
	action_usage
	action_help
;;
*)
	action_usage
	exit 1
;;
esac

exit 0

#
# end of file
#
