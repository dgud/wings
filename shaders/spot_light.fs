// $Id$
//
// Fragment shader for point lights
//
// Author: Dan Gudmundsson
//

#version 120

#include "lib_base.glsl"
#include "lib_normal.glsl"
#include "lib_material.glsl"

varying vec3 ws_position;
uniform vec3 ws_eyepoint;
uniform vec4 ws_lightpos;
uniform vec4 light_diffuse;
uniform vec4 light_specular;
uniform vec3 light_att;
uniform vec3 light_dir;
uniform float light_angle;
uniform float light_exp;
uniform float Exposure;

void main(void)
{

  float dist;
  float att;
  vec3 n = get_normal();
  vec3 v = normalize(ws_eyepoint-ws_position);  // point to camera
  vec3 l = ws_lightpos.xyz-ws_position;
  dist = length(l);
  l = normalize(l);
  PBRInfo pbr = calc_views(n, v, l);
  pbr = calc_material(pbr);

  att = 1.0/(light_att.x+light_att.y*dist+light_att.z*dist*dist);
  float spotDot = dot(-l, normalize(light_dir));
  if(spotDot < light_angle)
    att = 0.0;
  else
    att *= pow(spotDot, light_exp);

  // Calculate the shading terms for the microfacet specular shading model
  vec3  F = specularReflection(pbr);
  float G = geometricOcclusion(pbr);
  float D = microfacetDistribution(pbr);

  // Calculation of analytical lighting contribution
  vec3 diffuseContrib = (1.0 - max(max(F.r, F.g), F.b)) * diffuse(pbr);
  diffuseContrib *= light_diffuse.xyz;
  vec3 specContrib = F * G * D / (4.0 * pbr.NdotL * pbr.NdotV);
  specContrib *= light_specular.xyz;
  // Obtain final intensity as reflectance (BRDF) scaled by the energy of the light (cosine law)
  vec3 frag_color = att * pbr.NdotL * (diffuseContrib + specContrib);
  frag_color *= Exposure;
  gl_FragColor = vec4(pow(frag_color, vec3(1.0/2.2)), pbr.opaque);
}


