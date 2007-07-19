// $Id:$
//

varying vec3 vertex_color;
uniform int  Flag;

void main()
{
    gl_Position = ftransform();
    if (Flag==0)
        vertex_color = gl_Vertex.xyz;
    else
        vertex_color = gl_Normal.xyz;
    gl_FrontColor = vec4(vertex_color, 1.0);
}
