//
// Base library functions and types
//
// Author: Dan Gudmundsson
//
// Extensions must always be on top of the file load them here
// and include this file first

#ifndef lib_base
#define lib_base 1

#ifdef GL_ARB_shader_texture_lod
#extension GL_ARB_shader_texture_lod : enable
#endif

const float M_PI = 3.141592653589793;

// Encapsulate the various inputs used by the various functions in the shading equation
struct PBRInfo
{
    float NdotL;                  // cos angle between normal and light direction
    float NdotV;                  // cos angle between normal and view direction
    float NdotH;                  // cos angle between normal and half vector
    float LdotH;                  // cos angle between light direction and half vector
    float VdotH;                  // cos angle between view direction and half vector
    float perceptualRoughness;    // roughness value, as authored by the model creator (input to shader)
    float metalness;              // metallic value at the surface
    vec3 reflectance0;            // full reflectance color (normal incidence angle)
    vec3 reflectance90;           // reflectance color at grazing angle
    float alphaRoughness;         // roughness mapped to a more linear change in the roughness (proposed by [2])
    vec3 diffuseColor;            // color contribution from diffuse lighting
    vec3 specularColor;           // color contribution from specular lighting
    float occlusion;
    float opaque;
};

vec4 SRGBtoLINEAR(vec4 srgbIn)
{
    vec3 bLess = step(vec3(0.04045),srgbIn.xyz);
    vec3 linOut = mix( srgbIn.xyz/vec3(12.92), pow((srgbIn.xyz+vec3(0.055))/vec3(1.055),vec3(2.4)), bLess );
    return vec4(linOut,srgbIn.w);;
}

PBRInfo calc_views(vec3 Norm, vec3 View, vec3 Ligth)
{
  PBRInfo pbr;
  vec3 Half = normalize(Ligth+View);           // halfv between l and v
  pbr.NdotL = clamp(dot(Norm, Ligth), 0.001, 1.0);
  pbr.NdotV = abs(dot(Norm, View)) + 0.001;
  pbr.NdotH = clamp(dot(Norm, Half), 0.0, 1.0);
  pbr.LdotH = clamp(dot(Ligth, Half), 0.0, 1.0);
  pbr.VdotH = clamp(dot(View, Half), 0.0, 1.0);
  return pbr;
}

// The following equation models the Fresnel reflectance term of the spec equation (aka F())
// Implementation of fresnel from [4], Equation 15
vec3 specularReflection(PBRInfo pbrInputs)
{
    return pbrInputs.reflectance0 +
      (pbrInputs.reflectance90 - pbrInputs.reflectance0)
      * pow(clamp(1.0 - pbrInputs.VdotH, 0.0, 1.0), 5.0);
}

// This calculates the specular geometric attenuation (aka G()),
// where rougher material will reflect less light back to the viewer.
// This implementation is based on [1] Equation 4, and we adopt their modifications to
// alphaRoughness as input as originally proposed in [2].
float geometricOcclusion(PBRInfo pbrInputs)
{
    float NdotL = pbrInputs.NdotL;
    float NdotV = pbrInputs.NdotV;
    float r = pbrInputs.alphaRoughness*pbrInputs.alphaRoughness;

    float attenuationL = NdotL / (NdotL + sqrt(r + (1.0 - r) * (NdotL * NdotL)));
    float attenuationV = NdotV / (NdotV + sqrt(r + (1.0 - r) * (NdotV * NdotV)));
    return 8.0 * attenuationL * attenuationV;
}

// The following equation(s) model the distribution of microfacet normals across the area being drawn (aka D())
// Implementation from "Average Irregularity Representation of a Roughened Surface for Ray Reflection" by T. S. Trowbridge, and K. P. Reitz
// Follows the distribution function recommended in the SIGGRAPH 2013 course notes from EPIC Games [1], Equation 3.
float microfacetDistribution(PBRInfo pbrInputs)
{
    float roughnessSq = pbrInputs.alphaRoughness * pbrInputs.alphaRoughness;
    float f = (pbrInputs.NdotH * roughnessSq - pbrInputs.NdotH) * pbrInputs.NdotH + 1.0;
    return roughnessSq / (M_PI * f * f);
}

// Basic Lambertian diffuse
// Implementation from Lambert's Photometria https://archive.org/details/lambertsphotome00lambgoog
// See also [1], Equation 1
vec3 diffuse(PBRInfo pbrInputs)
{
  return pbrInputs.diffuseColor / M_PI;
}

#endif
