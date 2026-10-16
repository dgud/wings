// $Id$
//
// Fragment shader for MatCap light
//
// Author: 2026 Micheus
//

#version 120

#include "lib_base.glsl"
#include "lib_normal.glsl"
#include "lib_material.glsl"


varying vec3 vs_position;
varying vec3 vs_normal;
varying mat3 wv_matrix;

uniform float MatCapRot;
uniform int MatCapUseDiffuse;
uniform sampler2D MatCapMap;


vec2 matcapUV(vec3 vec)
{
    float ofs = 2.0*sqrt(vec.x*vec.x + vec.y*vec.y + (vec.z+1.0)*(vec.z+1.0));
    vec2 uv = vec.xy/ofs+0.5;
    return uv;
}

vec3 matcapRotate(vec3 v, float a) {
    float c = cos(a);
    float s = sin(a);
    vec3 n = vec3(0.0,0.0,1.0);
    return v*c + cross(n, v)*s + n*dot(n, v)*(1.0-c);
}

vec4 calcBasecolor(vec4 v_basecolor) {
  if(MatCapUseDiffuse > 0) {
      vec4 basecolor = get_basecolor();
      return v_basecolor*basecolor;
  } else
      return v_basecolor;
}

void main() {
    vec3 n = get_normal();

    vec3 vN = normalize(wv_matrix * n);      // convert world-space normal into view-space
    vec3 vP = normalize(-vs_position);        // view-space view vector (camera at origin)
    vec3 r = reflect(-vP, vN);                 // reflection vector in view space
    r = normalize(r);

    vec3 Rr = matcapRotate(r, MatCapRot);
    Rr = normalize(Rr);

    vec2 uv = matcapUV(Rr);
    vec4 basecolor = calcBasecolor(texture2D(MatCapMap, uv));
    gl_FragColor = vec4(basecolor.rgb, 1.0);
}