//
// Vertex shader for most of the frament shader
//
#version 120

varying vec3 ws_position; // world space

void main(void)
{
    ws_position = gl_Vertex.xyz;
    mat4x4 view = mat4x4(mat3x3(gl_ModelViewMatrix));
    gl_Position     = gl_ProjectionMatrix * view * gl_Vertex;
}
