// $Id:$

void main()
{
    gl_Position = ftransform();
    //vec4 GL_VERTEX = ((gl_Vertex.xyz/gl_Vertex.w), 1.0);
    vec3 ecPos = vec3(gl_ModelViewMatrix * gl_Vertex);
    //float z = distance(ecPos, vec3(0))/15.0;
    //float z = length(ecPos)/15.0;
    float z = length(ecPos)/15.0;
    gl_FrontColor = vec4(z, z, z, 1.0);
}
