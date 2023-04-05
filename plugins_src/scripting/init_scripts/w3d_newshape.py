
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

from w3d_int import OutputList
import sys

def list_of_objects_to_outputlist(objlist):
	if hasattr(objlist, "__len__"):
		outputlist = OutputList()
		for obj in objlist:
			if type(obj) is str:
				outputlist.add_str(obj)
			elif type(obj) is tuple:
				o = list_of_objects_to_outputlist(obj)
				outputlist.add_vector(o)
			elif hasattr(obj, "__len__"):
				o = list_of_objects_to_outputlist(obj)
				outputlist.add_list(o)
			elif (type(obj) is int):
				outputlist.add_integer(obj)
			elif type(obj) is float:
				outputlist.add_float(obj)
			else:
				o = obj.as_output_list()
				outputlist.add_list(o)
		return outputlist
	else:
		return objlist.as_output_list()


class ListOfArrays:
	def __init__(self, l):
		self.l = l
	def as_output_list(self):
		l2 = OutputList()
		for item in self.l:
			l3 = OutputList()
			l3.add_numbers(item)
			l2.add_list(l3)
		return l2

class ListOfTuples:
	def __init__(self, l):
		self.l = l
	def as_output_list(self):
		l2 = OutputList()
		for item in self.l:
			l3 = OutputList()
			l3.add_numbers(item)
			l2.add_vector(l3)
		return l2

class NewShape:
	def __init__(self):
		self.prefix = "shape"
		## Face and Vertices version
		self.fs = []
		self.vs = []
		
		## E3DObject version
		self.obj = None
		self.mat = []
		
	def as_output_list(self):
		o3 = OutputList()
		o3.add_symbol("new_shape")
		o3.add_str(self.prefix)
		if self.obj == None:
			o3.add_list(list_of_objects_to_outputlist(self.fs))
			o3.add_list(list_of_objects_to_outputlist(self.vs))
		else:
			o3.add_list(self.obj.as_output_list())
			o3.add_list(list_of_objects_to_outputlist(self.mat))
		return o3

