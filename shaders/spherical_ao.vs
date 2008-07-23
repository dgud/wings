// $Id$
// Spherical Ambient Occlusion (spherical depth)

varying vec3 DepthColor;

void main()
{
	float z = length(gl_Vertex.xyz);
	z = pow(z, 8.0);
	vec4 color = gl_FrontMaterial.diffuse * gl_Color;
	DepthColor = vec3(color * vec4(z));
	#ifdef __GLSL_CG_DATA_TYPES // Fix clipping for Nvidia and ATI
	gl_ClipVertex = gl_ModelViewMatrix * gl_Vertex;
	#endif
	gl_Position = ftransform();
}
