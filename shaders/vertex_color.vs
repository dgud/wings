// $Id$
// Colors based on vertex-normal or face-normals

varying vec3 VertexColor;
uniform int  Flag;

void main()
{
	if (Flag==0)
		VertexColor = gl_Vertex.xyz;
	else
		VertexColor = gl_Normal.xyz;
	#ifdef __GLSL_CG_DATA_TYPES // Fix clipping for Nvidia and ATI
	gl_ClipVertex = gl_ModelViewMatrix * gl_Vertex;
	#endif
	gl_Position = ftransform();
}
