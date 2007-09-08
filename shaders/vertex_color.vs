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
    gl_Position = ftransform();
}
