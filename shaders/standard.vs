//
// Vertex shader for most of the fragment shader
//
#version 120

attribute vec4 wings_tangent;

uniform vec4 diffuse;

varying vec3 ws_normal;   // world space
varying vec4 ws_tangent;  // world space
varying vec3 ws_position; // world space
varying vec4 v_basecolor;

uniform mat3x4 ws_matrix;

vec4 SRGBtoLINEAR(vec4 srgbIn)
{
    vec3 bLess = step(vec3(0.04045),srgbIn.xyz);
    vec3 linOut = mix( srgbIn.xyz/vec3(12.92), pow((srgbIn.xyz+vec3(0.055))/vec3(1.055),vec3(2.4)), bLess );
    return vec4(linOut,srgbIn.w);
}

void main(void)
{
    ws_position = mat3x3(ws_matrix)*gl_Vertex.xyz;
    // ws_position = gl_Vertex.xyz;
    v_basecolor	= diffuse * SRGBtoLINEAR(gl_Color);
    ws_normal	= normalize(mat3x3(ws_matrix)*gl_Normal);
    vec3 T      = mat3x3(ws_matrix)*wings_tangent.xyz;
    ws_tangent  = vec4(T.xyz, wings_tangent.w);
    gl_TexCoord[0]	= gl_MultiTexCoord0;
#ifdef __GLSL_CG_DATA_TYPES // Fix clipping for Nvidia and ATI
    gl_ClipVertex   = gl_ModelViewMatrix * gl_Vertex;
#endif
    gl_Position     = gl_ModelViewProjectionMatrix * gl_Vertex;
}
