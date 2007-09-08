// $Id: spherical_ao.vs 117 2007-07-19 16:44:01Z antoneos $
// Spherical Ambient Occlusion (spherical depth)

varying vec3 DepthColor;

void main()
{
	gl_FragColor = vec4(DepthColor, 1.0);
}
