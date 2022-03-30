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
uniform float Exposure;

void main(void)
{
  vec3 view_vec = normalize(-position);
  vec2 index = vec2uv(view_vec);
  index = vec2(index.x+bg_rotate, index.y);
  float scale = 3.0;  // ~ sqrt(NoOfmipmaps)  mipmaps = 10 for 2048x1024
  float lod = pow((0.1+bg_blur) * scale, 2.0);

#ifdef GL_ARB_shader_texture_lod
  vec3 col = SRGBtoLINEAR(texture2DLod(EnvSpecMap, index, lod)).rgb;
#else  // NO GL_ARB_shader_texture_lod
  vec3 col = SRGBtoLINEAR(texture2D(EnvSpecMap, index, lod)).rgb;
#endif // GL_ARB_shader_texture_lod
  col *= Exposure;
  gl_FragColor = vec4(pow(col, vec3(1.0/2.2)), 1.0);
}
