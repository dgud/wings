//
// Fragment shader for camera lighting
//
// Author: Dan Gudmundsson
//

#include "lib_base.glsl"

// Moved to lib_base.glsl
// #ifdef GL_ARB_shader_texture_lod
// #extension GL_ARB_shader_texture_lod : enable
// #endif

uniform sampler2D EnvBrdfMap;
uniform sampler2D EnvDiffMap;
uniform sampler2D EnvSpecMap;
uniform float bg_rotate;

vec2 vec2uv(vec3 vec)
{
  float u = atan(vec.x, vec.z);
  float v = acos(vec.y);
  u = 0.5f+0.5*u/M_PI;
  v = v/M_PI;
  // http://vcg.isti.cnr.it/~tarini/no-seams/
  // float u1 = fract(u+0.5)-0.5;
  // u = (fwidth(u)) < ((fwidth(u1)*2.1)) ?  u : u1;
  return vec2(u, v);
}

vec3 background_lighting(PBRInfo pbrInputs, vec3 N, vec3 reflection)
{
    vec3 refl = normalize(reflection);
    vec2 index = vec2uv(refl);
    index = vec2(index.x+bg_rotate, index.y);

    vec2 brdf = texture2D(EnvBrdfMap, vec2(pbrInputs.NdotV, pbrInputs.perceptualRoughness)).rg;
    vec3 difflight = SRGBtoLINEAR(texture2D(EnvDiffMap, index)).rgb;
    vec3 diffuse  = difflight * pbrInputs.diffuseColor;
    float mipCount = 10.0; // resolution of 2048x1024
    float lod = (pbrInputs.perceptualRoughness * mipCount);
#ifdef GL_ARB_shader_texture_lod
    vec3 speclight = SRGBtoLINEAR(texture2DLod(EnvSpecMap, index, lod)).rgb;
#else  // NO GL_ARB_shader_texture_lod
    vec3 speclight = SRGBtoLINEAR(texture2D(EnvSpecMap, index, lod)).rgb;
#endif // GL_ARB_shader_texture_lod

    vec3 specular = speclight * (pbrInputs.specularColor * brdf.x + brdf.y);
    return diffuse + specular;
}
