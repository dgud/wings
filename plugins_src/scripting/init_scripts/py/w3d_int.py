
##
##  Scripting for Shapes  (Scheme and Python)
##
##  Copyright 2023-2025 Edward Blake
##
##  See the file "license.terms" for information on usage and redistribution
##  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
##
##     $Id$
##

##
## Symbolic expression reader and writer for script input/output
##
## Python script side
##

import sys
import threading
import time

def t_1():
    scm_parse("(test '(0 1 2 3) #(2 3 4 5) '(\"Test\" 2 #f))")

def scm_parse(a):
    inp = a
    lst = []
    tok = ""
    while inp != "":
        if inp == "" and tok == "":
            return lst, ""
        elif inp == "" and len(tok) > 0:
            lst.append(bare_word(tok))
            tok = ""
        elif inp[0] == '\"':
            r = inp[1:]
            str1, r_0 = scm_parse_str(r)
            inp = r_0
            lst.append(str1)
        elif inp[0] == '#' and inp[1] == '(':
            if tok == "":
                r = inp[2:]
                sublist, r_0 = scm_parse(r)
                lst.append(sublist)
                inp = r_0
            else:
                lst.append(bare_word(tok))
                tok = ""
        elif inp[0] == '\'' and inp[1] == '(':
            if tok == "":
                r = inp[2:]
                sublist, r_0 = scm_parse(r)
                lst.append(sublist)
                inp = r_0
            else:
                lst.append(bare_word(tok))
                tok = ""
        elif inp[0] == '(':
            if tok == "":
                sublist, r_0 = scm_parse(inp[1:])
                inp = r_0
                lst.append(sublist)
            else:
                lst.append(bare_word(tok))
                tok = ""
        elif inp[0] == ')':
            if tok == "":
                return lst, inp[1:]
            else:
                lst.append(bare_word(tok))
                tok = ""
        elif inp[0] == ' ' or inp[0] == '\t' or inp[0] == '\r' or inp[0] == '\n':
            if tok == "":
                inp = inp[1:]
                tok = ""
            else:
                lst.append(bare_word(tok))
                tok = ""
        elif ((inp[0] >= '0' and inp[0] <= '9') or (inp[0] == '.') or (inp[0] == '-')) and tok == "":
            num, r_1 = scm_parse_num(inp)
            lst.append(num)
            tok = ""
            inp = r_1
        else:
            tok = tok + inp[0]
            inp = inp[1:]
    return lst, ""
    

def scm_parse_num(a):
    inp = a
    str1 = ""
    is_float = False
    while inp != "":
        if inp[0] == ' ' or inp[0] == ')' or inp[0] == '\r' or inp[0] == '\n':
            if is_float:
                num = float(str1)
            else:
                num = int(str1)
            return num, inp
        else:
            if inp[0] == '.':
                is_float = True
            str1 = str1 + inp[0]
            inp = inp[1:]
    if is_float:
        num = float(str1)
    else:
        num = int(str1)
    return num, ""

def scm_parse_str(a):
    inp = a
    str1 = ""
    while inp != "":
        if inp[0] == '\\':
            str1 = str1 + inp[1]
            inp = inp[2:]
        elif inp[0] == '"':
            return str1, inp[1:]
        else:
            str1 = str1 + inp[0]
            inp = inp[1:]

def bare_word(a0):
    a1 = a0
    if a1 == "#t":
        return True
    elif a1 == "#f":
        return False
    else:
        return a1

def send_get_reply(b):
    print("")
    b.write_list_out(sys.stdout)
    print("")
    sys.stdout.flush()
    reply = None
    while True:
        line = sys.stdin.readline()
        if (line == ""):
            pass
        reply_0, _ = scm_parse(line)
        reply = reply_0[0]
        break
    return reply


def wings_set_var(varname, varval):
    b = OutputList()
    b.add_symbol("%setvar")
    b.add_str(varname)
    b.add_value(varval)
    return send_get_reply(b)

def wings_get_var(varname):
    b = OutputList()
    b.add_symbol("%getvar")
    b.add_str(varname)
    return send_get_reply(b)

def wings_query(str):
    b = OutputList()
    b.add_symbol("%query")
    b.add_str(str)
    return send_get_reply(b)


def wings_we__get(module,fun,*args):
    b = OutputList()
    b.add_symbol("%we")
    lt = OutputList()
    lt.add_symbol(module)
    lt.add_symbol(fun)
    for arg in args:
        lt.add_value(arg)
    b.add_list(lt)
    return send_get_reply(b)


def wings_we__change(module,fun,*args):
    b = OutputList()
    b.add_symbol("%we!")
    lt = OutputList()
    lt.add_symbol(module)
    lt.add_symbol(fun)
    for arg in args:
        lt.add_value(arg)
    b.add_list(lt)
    return send_get_reply(b)


## TODO: wings_with_we_change Mod Name Args Fun


def wings_previous_we__get(stackindex,module,fun,*args):
    b = OutputList()
    b.add_symbol("%we-previous")
    b.add_value(stackindex)
    b.add_symbol(module)
    b.add_symbol(fun)
    for arg in args:
        b.add_value(arg)
    return send_get_reply(b)


def wings_previous_we__change(stackindex,module,fun,*args):
    b = OutputList()
    b.add_symbol("%we-previous!")
    b.add_value(stackindex)
    b.add_symbol(module)
    b.add_symbol(fun)
    for arg in args:
        b.add_value(arg)
    return send_get_reply(b)



def list_of_objects_to_outputlist(objlist):
    if hasattr(objlist, "__len__"):
        outputlist = OutputList()
        for obj in objlist:
            if type(obj) is str:
                outputlist.add_str(obj)
            else:
                outputlist.add_value(obj)
        return outputlist
    else:
        return objlist.as_output_list()

def list_of_pairs_to_outputlist(objlist):
    if hasattr(objlist, "__len__"):
        outputlist = OutputList()
        for obj in objlist:
            o = obj.as_output_list()
            outputlist.add_vector(o)
        return outputlist
    else:
        return objlist.as_output_list()

class OutputList:
    def __init__(self):
        self.lst_cont = []
        self.lst_type = []
    
    def add_symbol(self, a):
        self.add(a, 0)

    def add_str(self, a):
        self.add(a, 1)
        
    def add_number(self, a):
        self.add(a, 2)
        
    def add_numbers(self, alst):
        for a in alst:
            self.add(a, 2)
    
    def add_integer(self, a):
        self.add(a, 2)
        
    def add_integers(self, alst):
        for a in alst:
            self.add(a, 2)
    
    def add_float(self, a):
        self.add(a, 2)
        
    def add_floats(self, alst):
        for a in alst:
            self.add(a, 2)
    
    def add_list(self, a):
        self.add(a, 3)
    
    def add_vector(self, a):
        self.add(a, 4)
    
    def add(self, a, typ):
        self.lst_cont.append(a)
        self.lst_type.append(typ)

    def add_value(self, obj):
        if type(obj) is str:
            self.add_str(obj)
        elif type(obj) is tuple:
            o = list_of_objects_to_outputlist(obj)
            self.add_vector(o)
        elif hasattr(obj, "__len__"):
            o = list_of_objects_to_outputlist(obj)
            self.add_list(o)
        elif (type(obj) is int):
            self.add_integer(obj)
        elif type(obj) is float:
            self.add_float(obj)
        elif hasattr(obj, "as_output_list"):
            o = obj.as_output_list()
            self.add_list(o)
        else:
            o = obj.as_output_list()
            self.add_list(o)
    
    def write_list_out(self, ost):
        ost.write("(")
        for i in range(0, len(self.lst_cont)):
            if i > 0:
                ost.write(" ")
            if self.lst_type[i] == 0:
                ost.write(self.lst_cont[i])
            elif self.lst_type[i] == 1:
                ts = self.lst_cont[i]
                ts = ts.replace("\\", "\\\\")
                ts = ts.replace("\"", "\\\"")
                ost.write("\"" + ts + "\"")
            elif self.lst_type[i] == 2:
                ost.write(str(self.lst_cont[i]))
            elif self.lst_type[i] == 3:
                self.lst_cont[i].write_list_out(ost)
            elif self.lst_type[i] == 4:
                ost.write("#")
                self.lst_cont[i].write_list_out(ost)
        ost.write(")")

class Okay:
    def __init__(self,*args):
        self.args = args
    def as_output_list(self):
        o = OutputList()
        o.add_symbol("ok")
        for arg in self.args:
            o.add_list(arg.as_output_list())
        return o


## For scripts that change vertex positions
class Points:
    def __init__(self):
        self.list = []
    
    def as_output_list(self):
        o3 = OutputList()
        for v in self.list:
            vo2 = OutputList()
            vo3 = OutputList()
            vo3.add_float(v[1][0])
            vo3.add_float(v[1][1])
            vo3.add_float(v[1][2])
            vo2.add_integer(v[0])
            vo2.add_vector(vo3)
            o3.add_vector(vo2)
        return o3
    
    def load_from(self, vlist):
        for pair in vlist:
            a1 = pair[0]
            a2 = pair[1]
            a2_x = a2[0]
            a2_y = a2[1]
            a2_z = a2[2]
            self.list.append((a1, (a2_x, a2_y, a2_z)))

class SetPoints:
    def __init__(self,points):
        self.points = points
    def as_output_list(self):
        o = OutputList()
        o.add_symbol("set_points")
        o.add_value(self.points)
        return o


## Long running process
##
## Example:
## 
## class Examp:
##     def run(self):
##         sum = 0
##         for i in range(0, 100000000):
##             sum = sum + i
## 
## print("Begin")
## lrp = LongRunningProcess()
## lrp.run(Examp())
## print("Done")
## 

def lrp_standby_function(lro):
    while True:
        lro.standbySema.acquire()
        v = lro.standbyState
        lro.standbySema.release()
        if v:
            break
        print("('%keepalive 0)")
        time.sleep(1)

## A thread that simply sends keep-alive messages back to the scripting
## plugin to keep it from timing out while the script processes something
## that takes a while.
##
class LongRunningProcess:
    def __init__(self):
        self.standbyState = False
        self.standbySema = threading.Semaphore()
        self.standbyThread = threading.Thread(target=lrp_standby_function, args=(self,))

    def run(self, runo):
        self.standbyThread.start()
        ## Try is needed or main thread will close on an error but leaves
        ## the standby thread running.
        try:
            runo.run()
        finally:
            self.standbySema.acquire()
            self.standbyState = True
            self.standbySema.release()
            self.standbyThread.join()

