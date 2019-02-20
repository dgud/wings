// $Id$
//
// Fragment shader for skybox rendering
//
// Author: Dan Gudmundsson
//

#version 120

#include "lib_base.glsl"
#include "lib_envlight.glsl"

varying vec3 ws_position;
uniform float sb_roughness;

void main(void)
{
    vec2 index = vec2uv(normalize(ws_position*-1.0));
    float lod = sb_roughness*8;
#ifdef GL_ARB_shader_texture_lod
    vec3 bg = SRGBtoLINEAR(texture2DLod(EnvSpecMap, index, lod)).rgb;
#else  // NO GL_ARB_shader_texture_lod
    vec3 bg = SRGBtoLINEAR(texture2D(EnvSpecMap, index, lod)).rgb;
#endif // GL_ARB_shader_texture_lod
    gl_FragColor = vec4(bg,1.0);
}
