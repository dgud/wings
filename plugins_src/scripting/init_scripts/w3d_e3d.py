
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
## Helper classes for Scripting to Wings 3D
##

from w3d_int import OutputList
import sys

E3D_INFINITY = 3.402823e+38  ## 32 bits float max

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

def list_of_atoms_to_outputlist(objlist):
	if hasattr(objlist, "__len__"):
		outputlist = OutputList()
		outputlist.add_symbol('!list')
		for obj in objlist:
			if type(obj) is str:
				outputlist.add_symbol(obj)
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

## Types for transform
class E3DTransf:
	def __init__(self):
		self.mat = [] # e3d_mat:identity() :: e3d_mat:matrix(),
		self.inv = [] # e3d_mat:identity() :: e3d_mat:matrix()}).
	
	def send(self):
		o3 = OutputList()
		o3.add_symbol("e3d_transf")
		o3.add_list(self.mat)
		o3.add_list(self.inv)
		return o3
	
	def load_from(self, tuple):
		if tuple[0] == "e3d_transf":
			t_mats = tuple[1]
			for mt in t_mats:
				self.mat.append(mt)
			t_invs = tuple[2]
			for inv in t_invs:
				self.inv.append(inv)
		

class Ray:
	def __init__(self):
		self.o = None
		self.d = None
		self.n = None          # Near, far (or MinT MaxT)
		self.f = None
		self.bfc = True        # Backface culling?
	
	def as_output_list(self):
		o3 = OutputList()
		o3.add_symbol("ray")
		o3.add_vector(self.o)
		o3.add_vector(self.d)
		o3.add_float(self.n)
		o3.add_float(self.f)
		o3.add_boolean(self.bfc)
		
		return o3
	
	def load_from(self, tuple):
		if tuple[0] == "ray":
			t_o = tuple[1]
			self.o = t_o
			t_d = tuple[2]
			self.d = t_d
			t_n = tuple[3]
			self.n = t_n          # Near, far (or MinT MaxT)
			t_f = tuple[4]
			self.f = t_f
			self.bfc = tuple[5]
		

class E3DFace:
	def __init__(self):
		self.vs = []         #List of vertex indices.
		self.vc = []         #Vertex color indices.
		self.tx = []         #List of texture indices.
		self.ns = []         #List of normal indices.
		self.mat = []        #Materials for face.
		self.sg = 1          #Smooth group for face.
		self.vis = -1        #Visible edges (as in 3DS).
	
	def as_output_list(self):
		o3 = OutputList()
		o3.add_symbol("e3d_face")
		
		o3.add_list(list_of_objects_to_outputlist(self.vs))
		o3.add_list(list_of_objects_to_outputlist(self.vc))
		o3.add_list(list_of_objects_to_outputlist(self.tx))
		o3.add_list(list_of_objects_to_outputlist(self.ns))
		o3.add_list(list_of_atoms_to_outputlist(self.mat))
		
		o3.add_integer(self.sg)
		o3.add_integer(self.vis)
		return o3
	
	def load_from(self, tuple):
		if tuple[0] == "e3d_face":
			t_vs = tuple[1]
			self.vs = []
			for vs1 in t_vs:
				self.vs.append(vs1)
			
			t_vc = tuple[2]
			self.vc = []
			for vc1 in t_vc:
				self.vc.append(vc1)
			
			t_tx = tuple[3]
			self.tx = []
			for tx1 in t_tx:
				self.tx.append(tx1)
			
			t_ns = tuple[4]
			self.ns = []
			for ns1 in t_ns:
				self.ns.append(ns1)
			
			t_mats = tuple[5]
			self.mat = []        #Materials for face.
			for mat1 in t_mats:
				self.mat.append(mat1)
			self.sg = tuple[6]
			self.vis = tuple[7]

## Polygon mesh.
class E3DMesh:
	def __init__(self):
		self.type = "triangle"  #  'triangle' | 'quad' | 'polygon',
		self.vs = []            #Vertex table (list).
		self.vc = []            #Vertex color table (list).
		self.tx = []            #Texture coordinates (list).
		self.ns = []            #Normal table (list).
		self.fs = []            #Face table (list of e3d_face).
		self.he = []            #List of chains of hard edges.
		self.matrix = "identity" #Local coordinate system.
		
	def as_output_list(self):
		o3 = OutputList()
		o3.add_symbol("e3d_mesh")
		o3.add_symbol(self.type)
		
		o3.add_list(list_of_objects_to_outputlist(self.vs))
		o3.add_list(list_of_objects_to_outputlist(self.vc))
		o3.add_list(list_of_objects_to_outputlist(self.tx))
		o3.add_list(list_of_objects_to_outputlist(self.ns))
		o3.add_list(list_of_objects_to_outputlist(self.fs))
		o3.add_list(list_of_objects_to_outputlist(self.he))
		
		if self.matrix == 'identity':
			o3.add_symbol(self.matrix)
		else:
			o3.add_vector(list_of_objects_to_outputlist(self.matrix))
		
		return o3
	
	def load_from(self, tuple):
		if tuple[0] == "e3d_mesh":
			self.type = tuple[1]
			
			t_vs = tuple[2]
			self.vs = []
			for vs1 in t_vs:
				self.vs.append((vs1[0], vs1[1], vs1[2]))
			
			t_vc = tuple[3]
			self.vc = []
			for vc1 in t_vc:
				self.vc.append(vc1)
			
			t_tx = tuple[4]
			self.tx = []
			for tx1 in t_tx:
				self.tx.append(tx1)
			
			t_ns = tuple[5]
			self.ns = []
			for ns1 in t_ns:
				self.ns.append(ns1)
			
			t_fs = tuple[6]
			self.fs = []
			for fs1 in t_fs:
				new_fs = E3DFace()
				new_fs.load_from(fs1)
				self.fs.append(new_fs)
			
			t_he = tuple[7]
			self.he = []
			for he1 in t_he:
				self.he.append(he1)
			
			self.matrix = tuple[8]

class E3DObject:
	def __init__(self):
		self.name = None   #Name of object (string), or 'undefined' if no name.
		self.obj = None    #Object implementation.
		self.mat = []      #Materials for this object.
		self.attr = []     #List of attributes.
		
	def as_output_list(self):
		o3 = OutputList()
		o3.add_symbol("e3d_object")
		if self.name is None:
			o3.add_symbol("undefined")
		else:
			o3.add_str(self.name)
		
		if self.obj is None:
			o3.add_symbol("undefined")
		else:
			o_obj = self.obj.as_output_list()
			o3.add_list(o_obj)
		
		o3.add_list(list_of_objects_to_outputlist(self.mat))
		o3.add_list(list_of_objects_to_outputlist(self.attr))
		return o3
	
	def load_from(self, tuple):
		if tuple[0] == 'e3d_object':
			t_name_0 = tuple[1]
			if t_name_0 != 'undefined':
				t_name = str(t_name_0)
				if t_name != '':
					self.name = t_name
			t_obj = tuple[2]
			self.obj = None
			if t_obj != 'undefined' and hasattr(t_obj, "__len__"):
				new_obj = E3DMesh()
				new_obj.load_from(t_obj)
				self.obj = new_obj
			
			t_mats = tuple[3]
			for t_mat in t_mats:
				pass ## TODO
			
			t_attrs = tuple[4]
			for t_attr in t_attrs:
				pass ## TODO

class E3DImage:
	def __init__(self):
		self.type = 'r8g8b8'      ## [g8 (gray8), a8 (alpha8) (Ch:Size)+[s|f]=signed|float]
		self.bytes_pp = 3         ## bytes per pixel
		self.alignment = 1        ## A = 1|2|4 Next row starts direct|even 2|even 4
		self.order = 'lower_left' ## First pixel is in: lower_left,lower_right,upper_left,upper_right]
		self.width = 0            ## in pixels
		self.height = 0           ## in pixels
		self.image = None         ## This is a temporary file path to a raw image
		self.filename = None      ## Filename or none
		self.name = ""	          ## Name of image
		self.extra = []
	
	def as_output_list(self):
		o3 = OutputList()
		o3.add_symbol('e3d_image')
		o3.add_symbol(self.type)
		o3.add_integer(self.bytes_pp)
		o3.add_integer(self.alignment)
		o3.add_symbol(self.order)
		o3.add_integer(self.width)
		o3.add_integer(self.height)
		
		if self.image == None:
			o3.add_symbol('none')
		else:
			o3.add_str(self.image) ## Temporary file for raw images
		
		if self.filename == None:
			o3.add_symbol('none')
		else:
			o3.add_str(self.filename)
		o3.add_str(self.name)
		o3.add_list(list_of_objects_to_outputlist(self.extra))
			   
		return o3
	
	def load_from(self, tuple):
		if tuple[0] == 'e3d_image':
			self.type = tuple[1]
			self.bytes_pp = tuple[2]
			self.alignment = tuple[3]
			self.order = tuple[4]
			self.width = tuple[5]
			self.height = tuple[6]
			
			## TODO: Grab the binary raw image from the temporary file
			self.image = tuple[7]  ## Temporary file to raw image
			
			self.filename = tuple[8]
			self.name = tuple[9]
			self.extra = tuple[10]

class MaterialMaps:
	def __init__(self):
		self.maps = {}
		
	def as_output_list(self):
		o3 = OutputList()
		
		for k in self.maps.keys():
			v = self.maps[k]
			attr_item = OutputList()
			attr_item.add_symbol(k)
			attr_item.add_vector(v.as_output_list())
			o3.add_vector(attr_item)
		return o3
	
	def load_from(self, tlist):
		for t_m in tlist:
			if t_m[0] == 'diffuse':
				new_img = E3DImage()
				new_img.load_from(t_m[1])
				self.maps[t_m[0]] = new_img
              

class MaterialOpenGLAttributes:
	def __init__(self):
		self.ambient = [0.0,0.0,0.0,0.0]
		self.specular = [0.1689853807692308,0.17133333333333334,0.15940444444444446,1.0]
		self.shininess = 0.19999999999999996
		self.diffuse = [0.7898538076923077,0.8133333333333334,0.6940444444444445,1.0]
		self.emission = [0.0,0.0,0.0,1.0]
		self.metallic = 0.1
		self.roughness = 0.8
		self.vertex_colors = 'set'
		
	def as_output_list(self):
		
		o3 = OutputList()
		
		ov_r = OutputList()
		ov_r.add_numbers(self.ambient)
		ov = OutputList()
		ov.add_symbol('ambient')
		ov.add_vector(ov_r)
		o3.add_list(ov)
		
		ov_r = OutputList()
		ov_r.add_numbers(self.specular)
		ov = OutputList()
		ov.add_symbol('specular')
		ov.add_vector(ov_r)
		o3.add_list(ov)
		
		ov = OutputList()
		ov.add_symbol('shininess')
		ov.add_float(self.shininess)
		o3.add_list(ov)
		
		ov_r = OutputList()
		ov_r.add_numbers(self.diffuse)
		ov = OutputList()
		ov.add_symbol('diffuse')
		ov.add_vector(ov_r)
		o3.add_list(ov)
		
		ov_r = OutputList()
		ov_r.add_numbers(self.emission)
		ov = OutputList()
		ov.add_symbol('emission')
		ov.add_vector(ov_r)
		o3.add_list(ov)
		
		ov = OutputList()
		ov.add_symbol('metallic')
		ov.add_float(self.metallic)
		o3.add_list(ov)
		
		ov = OutputList()
		ov.add_symbol('roughness')
		ov.add_float(self.roughness)
		o3.add_list(ov)
		
		ov = OutputList()
		ov.add_symbol('vertex_colors')
		ov.add_symbol(self.vertex_colors)
		o3.add_list(ov)

		return o3
	
	def load_from(self, tlist):
		for t_at in tlist:
			if t_at[0] == 'ambient':
				self.ambient = t_at[1] ## [0.0, 0.0, 0.0, 0.0]
			if t_at[0] == 'specular':
				self.specular = t_at[1] ## [0.1689853807692308, 0.17133333333333334, 0.15940444444444446, 1.0]
			if t_at[0] == 'shininess':
				self.shininess = t_at[1] ## 0.19999999999999996
			if t_at[0] == 'diffuse':
				self.diffuse = t_at[1] ## [0.7898538076923077, 0.8133333333333334, 0.6940444444444445, 1.0]
			if t_at[0] == 'emission':
				self.emission = t_at[1] ## [0.0, 0.0, 0.0, 1.0]
			if t_at[0] == 'metallic':
				self.metallic = t_at[1] ## 0.1
			if t_at[0] == 'roughness':
				self.roughness = t_at[1] ## 0.8
			if t_at[0] == 'vertex_colors':
				self.vertex_colors = t_at[1]


class Material:
	def __init__(self):
		self.name = "material"
		self.attrs = {}
	
	def as_output_list(self):
		o3 = OutputList()
		o3.add_symbol(self.name)
		
		attr_list = OutputList()
		for k in self.attrs.keys():
			v = self.attrs[k]
			attr_item = OutputList()
			attr_item.add_symbol(k)
			attr_item.add_list(v.as_output_list())
			attr_list.add_list(attr_item)
		o3.add_list(attr_list)
		return o3
	
	def load_from(self, tuple):
		self.name = tuple[0]
		t_attrs = tuple[1]
		for t_att in t_attrs:
			k = t_att[0]
			if k == 'maps':
				new_attr = MaterialMaps()
				new_attr.load_from(t_att[1])
			elif k == 'opengl':
				new_attr = MaterialOpenGLAttributes()
				new_attr.load_from(t_att[1])
			else:
				new_attr = t_att[1]
			self.attrs[k] = new_attr


class E3DFile:
	def __init__(self):
		self.objs = []     # List of objects.
		self.mat = []      # List of materials.
		self.creator = ""  # Creator string.
		self.dir = ""      # Directory for file.
		
	def as_output_list(self):
		o3 = OutputList()
		o3.add_symbol("e3d_file")
		
		o_objs = OutputList()
		o3.add_list(list_of_objects_to_outputlist(self.objs))
		o3.add_list(list_of_objects_to_outputlist(self.mat))
		
		o3.add_str(self.creator)
		o3.add_str(self.dir)
		return o3
		
	def load_from(self, tuple):
		if tuple[0] == 'e3d_file':
			t_objects = tuple[1]
			self.objs = []
			for t_object in t_objects:
				new_obj = E3DObject()
				new_obj.load_from(t_object)
				self.objs.append(new_obj)
			
			t_mats = tuple[2]
			self.mat = []
			for t_mat in t_mats:
				new_mat = Material()
				new_mat.load_from(t_mat)
				self.mat.append(new_mat)
			
			self.creator = str(tuple[3])
			
			t_dir = tuple[4]
			if t_dir == 'undefined':
				self.dir = None
			else:
				self.dir = str(t_dir)

			

## For change_points scripts
class SetPoints:
	def __init__(self):
		self.list = []
	
	def as_output_list(self):
		o3 = OutputList()
		for v in self.list:
			vo3 = OutputList()
			vo3.add_float(v[1][0])
			vo3.add_float(v[1][1])
			vo3.add_float(v[1][2])
			v.add_integer(v[0])
			v.add_vector(vo3)
			o3.add_vector(v)
		o2 = OutputList()
		o2.add_symbol("set_points")
		o2.add_list(o3)
		return o2
	
	def load_from(self, vlist):
		for pair in vlist:
			a1 = pair[0]
			a2 = pair[1]
			a2_x = a2[0]
			a2_y = a2[1]
			a2_z = a2[2]
			self.list.append((a1, (a2_x, a2_y, a2_z)))


def test():
	b = E3DFile()
	b.objs.append(E3DObject())
	o = b.as_output_list()
	o.write_list_out(sys.stdout)
	

