
##
##  Scripting for Shapes  (Scheme and Python)
##
##  Copyright 2023 Edward Blake
##
##  See the file "license.terms" for information on usage and redistribution
##  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
##
##     $Id$
##

##
## This python code acts as a boot strap to define some useful functions
## and then load the actual script that is sent as a symbolic expression through
## standard input by the erlang plugin.
##

import w3d_int
import sys
import time
from os import path
from runpy import run_path


sys.stdin.reconfigure(encoding="utf-8")
sys.stdout.reconfigure(encoding="utf-8")
sys.stderr.reconfigure(encoding="utf-8")

scr_langstrs = {}

def set_lang_strings(l):
	global scr_langstrs
	for a in l:
		scr_langstrs[a[0]] = a[1]

def __t(id, str):
	global scr_langstrs
	if id in scr_langstrs:
		return scr_langstrs[id]
	else:
		return str

def script_loader_loop():
	# Main function to call, can be called many times
	scr_main_fun = None
	while True:
		while True:
			line = sys.stdin.readline()
			if (line == ""):
				pass
			cmd_0, _ = w3d_int.scm_parse(line)
			cmd = cmd_0[0]
			break
		
		cmd_atom = cmd[0]
		if cmd_atom == "run_init":
			g = {}
			g["scm_parse"] = w3d_int.scm_parse
			g["OutputList"] = w3d_int.OutputList
			## Use __t(20, "Text") for localized text
			g["__t"] = __t
			# g["wings_set_var"] = w3d_int.wings_set_var
			# g["wings_query"] = w3d_int.wings_query
			script_file = cmd[1]
			set_lang_strings(cmd[2])
			sys.path.insert(0, path.dirname(script_file))
			g2 = run_path(script_file, g)
			scr_main_fun = None
			if "w3d_main_function" in g2:
				scr_main_fun = g2["w3d_main_function"]
			if scr_main_fun == None:
				sys.stderr.write("ERROR: main function not set\n")
				exit(1)
			print("(ok)")
			sys.stdout.flush()
			
		elif cmd_atom == "run":
			## Parameters being passed to the script from the script's
			## parameter options window.
			params = []

			## Access parameters by key, for those that used an atom key
			params_by_key = {}

			## Extra parameters passed as the third argument
			## These are parameters that are not set via a parameter
			## window and may be auxiliary variables dependent on the
			## type of script (e.g. "content" contains an e3d_file tuple
			## for exporter plugins, parameters set with "script_params"
			## also show up in here).
			extra_params = {}

			params = cmd[2]
			## Add to params_by_key parameters that are keyed
			for p in params:
				if hasattr(p, "__len__"):
					params_by_key[p[0]] = p[1]
			## Grab the extra params
			extra_params_1 = cmd[3]
			for p in extra_params_1:
				extra_params[p[0]] = p[1]
			scr_main_fun(params, params_by_key, extra_params)
			print("")
			sys.stdout.flush()
			print("(%ok)")
			sys.stdout.flush()
			
		else:
			print("Not run")
			exit(1)

script_loader_loop()

