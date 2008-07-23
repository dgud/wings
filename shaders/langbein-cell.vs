// $Id$

varying vec3 normal;
varying vec4 position;

void main()
{
  normal = gl_NormalMatrix * gl_Normal;
  position = gl_ModelViewMatrix * gl_Vertex;
	#ifdef __GLSL_CG_DATA_TYPES // Fix clipping for Nvidia and ATI
	gl_ClipVertex = gl_ModelViewMatrix * gl_Vertex;
	#endif
  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}

