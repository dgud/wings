//
// Vertex shader for most of the frament shader
//
#version 120

attribute vec4 wings_tangent;

uniform vec4 diffuse;

varying vec3 ws_normal;   // world space
varying vec4 ws_tangent;  // world space
varying vec3 ws_position; // world space
varying vec4 v_basecolor;

uniform mat3x4 ws_matrix;

void main(void)
{
    ws_position = mat3x3(ws_matrix)*gl_Vertex.xyz;
    // ws_position = gl_Vertex.xyz;
    v_basecolor	= diffuse * gl_Color;
    ws_normal	= normalize(mat3x3(ws_matrix)*gl_Normal);
    vec3 T      = mat3x3(ws_matrix)*wings_tangent.xyz;
    ws_tangent  = vec4(T.xyz, wings_tangent.w);
    gl_TexCoord[0]	= gl_MultiTexCoord0;
#ifdef __GLSL_CG_DATA_TYPES // Fix clipping for Nvidia and ATI
    gl_ClipVertex   = gl_ModelViewMatrix * gl_Vertex;
#endif
    gl_Position 	= ftransform();
}
