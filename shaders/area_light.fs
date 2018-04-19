//
// Fragment shader for area lights
//
// See: Real-Time Polygonal-Light Shading with Linearly Transformed Cosines
//      https://eheitzresearch.wordpress.com/415-2/
//
// Author: Dan Gudmundsson (with code from above)
//

#version 120

#include "lib_base.glsl"
#include "lib_normal.glsl"
#include "lib_material.glsl"

varying vec3 ws_position;
uniform vec3 ws_eyepoint;
uniform vec4 light_diffuse;
uniform vec4 light_specular;
uniform vec3[4] light_points;
uniform vec3 light_att;

uniform sampler2D EnvBrdfMap;
uniform sampler2D AreaLTCMap;

// Linearly Transformed Cosines
///////////////////////////////

float IntegrateEdge(vec3 v1, vec3 v2)
{
  float cosTheta = dot(v1, v2);
  float theta = acos(cosTheta);
  float res = cross(v1, v2).z * ((theta > 0.001) ? theta/sin(theta) : 1.0);

  return res;
}

void ClipQuadToHorizon(inout vec3 L[5], out int n)
{
  // detect clipping config
  int config = 0;
  if (L[0].z > 0.0) config += 1;
  if (L[1].z > 0.0) config += 2;
  if (L[2].z > 0.0) config += 4;
  if (L[3].z > 0.0) config += 8;

  // clip
  n = 0;

  if (config == 0)
    {
      // clip all
    }
  else if (config == 1) // V1 clip V2 V3 V4
    {
      n = 3;
      L[1] = -L[1].z * L[0] + L[0].z * L[1];
      L[2] = -L[3].z * L[0] + L[0].z * L[3];
    }
  else if (config == 2) // V2 clip V1 V3 V4
    {
      n = 3;
      L[0] = -L[0].z * L[1] + L[1].z * L[0];
      L[2] = -L[2].z * L[1] + L[1].z * L[2];
    }
  else if (config == 3) // V1 V2 clip V3 V4
    {
      n = 4;
      L[2] = -L[2].z * L[1] + L[1].z * L[2];
      L[3] = -L[3].z * L[0] + L[0].z * L[3];
    }
  else if (config == 4) // V3 clip V1 V2 V4
    {
      n = 3;
      L[0] = -L[3].z * L[2] + L[2].z * L[3];
      L[1] = -L[1].z * L[2] + L[2].z * L[1];
    }
  else if (config == 5) // V1 V3 clip V2 V4) impossible
    {
      n = 0;
    }
  else if (config == 6) // V2 V3 clip V1 V4
    {
      n = 4;
      L[0] = -L[0].z * L[1] + L[1].z * L[0];
      L[3] = -L[3].z * L[2] + L[2].z * L[3];
    }
  else if (config == 7) // V1 V2 V3 clip V4
    {
      n = 5;
      L[4] = -L[3].z * L[0] + L[0].z * L[3];
      L[3] = -L[3].z * L[2] + L[2].z * L[3];
    }
  else if (config == 8) // V4 clip V1 V2 V3
    {
      n = 3;
      L[0] = -L[0].z * L[3] + L[3].z * L[0];
      L[1] = -L[2].z * L[3] + L[3].z * L[2];
      L[2] =  L[3];
    }
  else if (config == 9) // V1 V4 clip V2 V3
    {
      n = 4;
      L[1] = -L[1].z * L[0] + L[0].z * L[1];
      L[2] = -L[2].z * L[3] + L[3].z * L[2];
    }
  else if (config == 10) // V2 V4 clip V1 V3) impossible
    {
      n = 0;
    }
  else if (config == 11) // V1 V2 V4 clip V3
    {
      n = 5;
      L[4] = L[3];
      L[3] = -L[2].z * L[3] + L[3].z * L[2];
      L[2] = -L[2].z * L[1] + L[1].z * L[2];
    }
  else if (config == 12) // V3 V4 clip V1 V2
    {
      n = 4;
      L[1] = -L[1].z * L[2] + L[2].z * L[1];
      L[0] = -L[0].z * L[3] + L[3].z * L[0];
    }
  else if (config == 13) // V1 V3 V4 clip V2
    {
      n = 5;
      L[4] = L[3];
      L[3] = L[2];
      L[2] = -L[1].z * L[2] + L[2].z * L[1];
      L[1] = -L[1].z * L[0] + L[0].z * L[1];
    }
  else if (config == 14) // V2 V3 V4 clip V1
    {
      n = 5;
      L[4] = -L[0].z * L[3] + L[3].z * L[0];
      L[0] = -L[0].z * L[1] + L[1].z * L[0];
    }
  else if (config == 15) // V1 V2 V3 V4
    {
      n = 4;
    }

  if (n == 3)
    L[3] = L[0];
  if (n == 4)
    L[4] = L[0];
}


vec3 ltc_eval(vec3 N, vec3 V, vec3 P, mat3 Minv, vec3 points[4], bool twoSided)
{
  // construct orthonormal basis around N
  vec3 T1, T2;
  T1 = normalize(V - N*dot(V, N));
  T2 = cross(N, T1);

  // rotate area light in (T1, T2, N) basis
  Minv = Minv*transpose(mat3(T1, T2, N));

  // polygon (allocate 5 vertices for clipping)
  vec3 L[5];
  L[0] = Minv*(points[0] - P);
  L[1] = Minv*(points[1] - P);
  L[2] = Minv*(points[2] - P);
  L[3] = Minv*(points[3] - P);

  int n;
  ClipQuadToHorizon(L, n);

  if (n == 0)
    return vec3(0, 0, 0);

  // project onto sphere
  L[0] = normalize(L[0]);
  L[1] = normalize(L[1]);
  L[2] = normalize(L[2]);
  L[3] = normalize(L[3]);
  L[4] = normalize(L[4]);

  // integrate
  float sum = 0.0;

  sum += IntegrateEdge(L[0], L[1]);
  sum += IntegrateEdge(L[1], L[2]);
  sum += IntegrateEdge(L[2], L[3]);
  if (n >= 4)
    sum += IntegrateEdge(L[3], L[4]);
  if (n == 5)
    sum += IntegrateEdge(L[4], L[0]);

  sum = twoSided ? abs(sum) : max(0.0, sum);

  vec3 Lo_i = vec3(sum, sum, sum);

  return Lo_i;
}

vec2 ltc_coords(vec2 uv)
{
    float theta = acos(uv.x);
    vec2 coords = vec2(uv.y, theta/(0.5*3.14159));

    const float LUT_SIZE = 64.0;
    // scale and bias coordinates, for correct filtered lookup
    coords = coords*(LUT_SIZE - 1.0)/LUT_SIZE + 0.5/LUT_SIZE;

    return coords;
}

void main(void)
{
  float att;
  bool two_sided = false;
  vec3 n = get_normal();
  vec3 v = normalize(ws_eyepoint-ws_position);  // point to camera
  PBRInfo pbr;
  pbr.NdotV = dot(n,v);
  pbr = calc_material(pbr);
  vec2 st = vec2(pbr.NdotV, pbr.perceptualRoughness);;
  vec2 brdf = texture2D(EnvBrdfMap, st).rg;
  st = ltc_coords(st);
  vec4 t = texture2D(AreaLTCMap, st);
  mat3 Minv = mat3(vec3(  1,   0, t.y),
                   vec3(  0, t.z,   0),
                   vec3(t.w,   0, t.x) );
  vec3 specLigth = ltc_eval(n, v, ws_position, Minv, light_points, two_sided);
  specLigth *= light_specular.xyz;
  vec3 frag_color = specLigth*(pbr.specularColor*brdf.x+brdf.y);
  mat3 Mid = mat3(vec3( 1, 0, 0),
                  vec3( 0, 1, 0),
                  vec3( 0, 0, 1) );
  vec3 diffLigth = ltc_eval(n, v, ws_position, Mid, light_points, two_sided);
  diffLigth *= light_diffuse.xyz;
  if(light_att.y < 0.00005 && light_att.z < 0.00005)
      att = 1.0;
  else {
      float dist = length(ws_position-light_points[0]);
      dist = min(dist, length(ws_position-light_points[1]));
      dist = min(dist, length(ws_position-light_points[2]));
      dist = min(dist, length(ws_position-light_points[3]));
      att = 1.0/(light_att.x+light_att.y*dist+light_att.z*dist*dist);
  }
  frag_color += diffLigth*pbr.diffuseColor;
  frag_color *= att;
  gl_FragColor = vec4(pow(frag_color, vec3(1.0/2.2)), pbr.opaque);
}


