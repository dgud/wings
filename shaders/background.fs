// $Id$
//
// Fragment shader for skybox drawing
//
// Author: Dan Gudmundsson
//

#version 120

#include "lib_base.glsl"
// #include "lib_normal.glsl"
#include "lib_envlight.glsl"
// #include "lib_material.glsl"

varying vec3 position;
uniform vec3 ws_eyepoint;
uniform float bg_blur;

void main(void)
{
  vec3 view_vec = normalize(-position);
  vec2 index = vec2uv(view_vec);
  float mipCount = 8.0; // resolution of 512x256
  float lod = (bg_blur * mipCount);

#ifdef GL_ARB_shader_texture_lod
  vec3 col = SRGBtoLINEAR(texture2DLod(EnvSpecMap, index, lod)).rgb;
#else  // NO GL_ARB_shader_texture_lod
  vec3 col = SRGBtoLINEAR(texture2D(EnvSpecMap, index, lod)).rgb;
#endif // GL_ARB_shader_texture_lod

  gl_FragColor = vec4(pow(col, vec3(1.0/2.2)), 1.0);
}
