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

vec2 vec2uv(vec3 vec)
{
  float u = atan(vec.x, vec.z);
  float v = acos(vec.y);
  return vec2(0.5f+0.5*u/M_PI, v/M_PI);
}

vec3 background_ligthting(PBRInfo pbrInputs, vec3 N, vec3 reflection)
{
    vec3 refl = normalize(reflection);
    vec2 index = vec2uv(refl);

    vec2 brdf = texture2D(EnvBrdfMap, vec2(pbrInputs.NdotV, pbrInputs.perceptualRoughness)).rg;
    vec3 difflight = SRGBtoLINEAR(texture2D(EnvDiffMap, index)).rgb;
    vec3 diffuse  = difflight * pbrInputs.diffuseColor;
    float mipCount = 8.0; // resolution of 512x256
    float lod = (pbrInputs.perceptualRoughness * mipCount);
#ifdef GL_ARB_shader_texture_lod
    vec3 specligth = SRGBtoLINEAR(texture2DLod(EnvSpecMap, index, lod)).rgb;
#else  // NO GL_ARB_shader_texture_lod
    vec3 specligth = SRGBtoLINEAR(texture2D(EnvSpecMap, index, lod)).rgb;
#endif // GL_ARB_shader_texture_lod

    vec3 specular = specligth * (pbrInputs.specularColor * brdf.x + brdf.y);
    return diffuse + specular;
}
