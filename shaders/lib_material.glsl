//
// GLSL library routine: material handling
//
// Author: Dan Gudmundsson
//


uniform int UseDiffuseMap;
uniform int UsePBRMap;
uniform int UseEmissionMap;

uniform sampler2D DiffuseMap;
uniform sampler2D PBRMap;
uniform sampler2D EmissionMap;
uniform sampler2D OcculMap;

uniform float metallic;
uniform float roughness;
uniform vec4 emission;

varying vec4 v_basecolor;  // diffuse * vertex_color

vec4 get_basecolor() {
  if(UseDiffuseMap > 0)
      return v_basecolor*SRGBtoLINEAR(texture2D(DiffuseMap, gl_TexCoord[0].xy));
  else return v_basecolor;
}

vec3 get_emission() {
  vec3 emi = vec3(emission);
  if(UseEmissionMap > 0) return SRGBtoLINEAR(texture2D(EmissionMap, gl_TexCoord[0].xy)).rgb * emi;
  return emi;
}

vec4 get_pbr_omr() {  // red = occlusion blue = roughness green = metallic
  vec4 mrSample = vec4(1.0,roughness,metallic,1.0);
  if(UsePBRMap > 0) {
    mrSample *= texture2D(PBRMap, gl_TexCoord[0].xy);
  }
  return clamp(mrSample, 0.04, 0.96);
}

float get_occlusion() {
  if(UsePBRMap > 0) {
    return texture2D(PBRMap, gl_TexCoord[0].xy).x;
  }
  return 1.0;
}


PBRInfo calc_material(PBRInfo pbr) {
  vec4 baseColor = get_basecolor();
  vec3 f0 = vec3(0.04);
  vec4 omr = get_pbr_omr();
  float rgh = omr.g;
  float met = omr.b;
  vec3 diffuse  = mix(baseColor.rgb, vec3(0.0), met);
  vec3 specular = mix(f0, baseColor.rgb, met);

  float reflectance = max(max(specular.r, specular.g), specular.b);
  float reflectance90 = clamp(reflectance * 25.0, 0.0, 1.0);
  vec3 specularEnvironmentR0 = specular.rgb;
  vec3 specularEnvironmentR90 = vec3(1.0, 1.0, 1.0) * reflectance90;

  pbr.perceptualRoughness = rgh;
  pbr.alphaRoughness = rgh*rgh;

  pbr.reflectance0 = specularEnvironmentR0;
  pbr.reflectance90 = specularEnvironmentR90;

  pbr.diffuseColor = diffuse;
  pbr.specularColor = specular;
  pbr.opaque = baseColor.a;
  pbr.occlusion = omr.r;
  return pbr;
}
