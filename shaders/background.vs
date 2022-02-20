//
// Vertex shader for most of the fragment shader
//
#version 120

varying vec3 position;
uniform mat3x4 ws_matrix;

void main(void)
{
    position = gl_Vertex.xyz;

    mat4 proj = gl_ProjectionMatrix;
    mat4 view = mat4(mat3(gl_ModelViewMatrix));

    vec4 pos = proj * view * vec4(gl_Vertex.xyz, 1.0);
    gl_Position = pos.xyww;
}
