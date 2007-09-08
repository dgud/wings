// $Id$
// Spherical Ambient Occlusion (spherical depth)

varying vec3 DepthColor;

void main()
{
    float z = length(gl_Vertex.xyz);
	z = pow(z, 8.0);
	DepthColor = vec3(z);
	gl_Position = ftransform();
}
