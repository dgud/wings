// $Id$

void main()
{
	float DepthNear = -1.0;
	float DepthFar = 1.0;
	gl_Position = ftransform();
	vec3 ecPos = vec3(gl_ModelViewMatrix * gl_Vertex);
	vec3 EyeDir = normalize(-ecPos);
	vec3 offset = gl_Vertex.xyz/gl_Vertex.w;
	float z = dot(offset, EyeDir);
	z = (z-DepthNear) / (DepthFar-DepthNear);
	gl_FrontColor = vec4(z, z, z, 1.0);
}
