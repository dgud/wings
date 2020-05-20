//
// GLSL library routine: normal handling
//
// Author: Dan Gudmundsson
//

uniform int UseNormalMap;
uniform sampler2D NormalMap;

varying vec3 ws_normal;
varying vec4 ws_tangent;

vec3 get_normal() {
    vec3 T = ws_tangent.xyz;
    float backface = (2.0 * float(gl_FrontFacing) - 1.0);
    if(UseNormalMap == 0 || dot(T,T) < 0.1)
	return backface*normalize(ws_normal); // No normal-map or Tangents
    // Calc Bumped normal
    T = normalize(T);
    vec3 BumpMapNormal = texture2D(NormalMap, gl_TexCoord[0].xy).xyz;
    BumpMapNormal = 2.0 * BumpMapNormal - vec3(1.0, 1.0, 1.0);
    vec3 N = normalize(ws_normal);
    T = normalize(T - dot(T, N) * N);
    vec3 B = cross(T, N) * ws_tangent.w;
    mat3 TBN = mat3(T, B, N);
    vec3 NewNormal = TBN * BumpMapNormal;
    NewNormal = normalize(NewNormal);
    return backface*NewNormal;
}
