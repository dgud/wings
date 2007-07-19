// $Id:$
// Spherical Ambient Occlusion

void main()
{
    gl_Position = ftransform();
    float z = length(gl_Vertex.xyz);
    z = pow(z, 8.0);
    gl_FrontColor = vec4(z, z, z, 1.0);
}
