
##
##  This generates the function documentation (mainly the we
##  functions) for the Python3 and Scheme scripting manuals.
##
##  No extensions are used so this should work with most versions
##  of awk.
##
##  Copyright 2025 Edward Blake
##
##  See the file "license.terms" for information on usage and redistribution
##  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
##
##     $Id$
##



function ts (s) {
  gsub("^[ ]+|[ ]+$","",s)
  return s
}

function snk (s) {
  ## Make snake case
  v = ""
  l = split(s, chrs, "");
  for (i2 = 1; i2 <= l; i2++) {
    c2 = chrs[i2];
    if (match(c2, "[A-Z]")) {
      if (i2 > 1) {
        v = v "_";
      }
      v = v tolower(c2);
    } else {
      v = v c2;
    }
  }
  return v
}

## Print an argument formatted for either Scheme or Python
##
function print_args (s) {
  c = split(s, args, " +");
  if (type == "scm") {
    for (i = 1; i <= c; i ++) {
      if (i > 1) {
        printf " ";
      }
      printf "%s", args[i];
    }
  }
  if (type == "py") {
    for (i = 1; i <= c; i ++) {
      if (i > 1) {
        printf ", ";
      }
      printf "%s", snk(args[i]);
    }
  }
}

## Substitute one placeholder
##
function note_arg (s) {
  if (type == "scm") {
    if (match(s, "^TUPLE ")) {
      cont = substr(s, RLENGTH+1);
      return "#(" cont ")";
    }
    if (match(s, "^ARG ")) {
      cont = substr(s, RLENGTH+1);
      return cont;
    }
    if (match(s, "^ATOM ")) {
      cont = substr(s, RLENGTH+1);
      return "'" cont;
    }
  }
  if (type == "py") {
    if (match(s, "^TUPLE ")) {
      cont = substr(s, RLENGTH+1);
      cont2 = "";
      c = split(cont, conta, " +");
      for (i = 1; i <= c; i++) {
        if (i > 1) {
            cont2 = cont2 ", ";
        }
        cont2 = cont2 snk(conta[i])
      }
      return "(" cont2 ")";
    }
    if (match(s, "^ARG ")) {
      cont = substr(s, RLENGTH+1);
      return snk(cont);
    }
    if (match(s, "^ATOM ")) {
      cont = substr(s, RLENGTH+1);
      return "'" cont "'";
    }
  }

  return "??";
}

## Parse out the text placeholders and substitute them
## with formatted text (for tuples, atoms, etc).
function notes2 (s) {
  str2 = ""
  while (match(s, "\\$\\(")) {
    str2 = str2 substr(s, 0, RSTART-1);
    s = substr(s, RSTART+RLENGTH);
    until = index(s, ")");
    str2 = str2 note_arg(substr(s, 0, until-1));
    s = substr(s, until+1);
  }
  str2 = str2 s;
  return str2;
}

function notes (s) {
  return notes2(s);
}

BEGIN {
  FS = "\\|"
  funname0 = ""
}

## Some notes that should be added to the generated text.
NF == 2 {
  if ($1 == "") {
    note = ts($2);
    print notes(note);
  }
}

## A function definition
NF > 5 {
  section = ts($1);
  module = ts($2);
  funname = ts($3);
  changeswe = ts($4);
  reqargs = ts($5)
  optargs = ts($6)
  returns = ts($7)

  ## When it is a different function name, add a line break
  if (funname0 != funname) {
    printf "\n";
    funname0 = funname;
  }

  ## The documentation for Scheme scripting
  if (type == "scm") {
    printf "(<b>";
    if (section != "") {
      printf "%s:", section;
    }
    printf "%s:%s%s</b>", module, funname, changeswe;
    if ((reqargs != "") || (optargs != "")) {
      printf "\n ";
      if (reqargs != "") {
        print_args(reqargs);
        if (optargs != "") {
          printf "\n ";
        }
      }
      if (optargs != "") {
        printf "<u><i>optional</i></u> ";
        print_args(optargs);
      }
    }
    printf ")\n";
  }

  ## The documentation for Python3 scripting
  if (type == "py") {
    printf "<b>%s__%s</b>(", module, funname;
    if (reqargs != "") {
      print_args(reqargs);
    }
    if (optargs != "") {
      if (reqargs != "") {
        printf " ";
      }
      printf "[";
      if (reqargs != "") {
        printf ", ";
      }
      print_args(optargs);
      printf "]";
    }
    printf ")\n";
    if (changeswe == "!") {
      printf "! Changes #we{}\n";
    }
  }

  if (returns != "") {
    if (returns != "_") {
      printf "<i>returns:</i> %s\n", notes(returns);
    }
  }
}
END {
}



