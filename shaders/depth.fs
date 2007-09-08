// $Id$
// Depth

varying vec3 DepthColor;

void main()
{
	gl_FragColor = vec4(DepthColor, 1.0);
}
