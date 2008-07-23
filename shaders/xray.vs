// $Id$
//
// Application to vertex shader

varying vec3 N;
varying vec3 I;
varying vec4 Cs;

void main()
{
	vec4 P = gl_ModelViewMatrix * gl_Vertex;
	I  = P.xyz - vec3 (0);
	N  = gl_NormalMatrix * gl_Normal;
	Cs = gl_Color;
	#ifdef __GLSL_CG_DATA_TYPES // Fix clipping for Nvidia and ATI
	gl_ClipVertex = gl_ModelViewMatrix * gl_Vertex;
	#endif
	gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}
