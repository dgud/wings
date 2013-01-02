// $Id$
//
// Fragment shader for hemispherical lighting
//
// Author: Dan Gudmundsson
//

uniform vec3 LightPosition;
uniform vec3 SkyColor;
uniform vec3 GroundColor;

uniform int UseDiffuseMap;
uniform int UseNormalMap;

uniform sampler2D DiffuseMap;
uniform sampler2D NormalMap;

varying vec3 normal;
varying vec3 ecPosition;
varying vec4 color;
varying vec4 tangent;

vec3 get_normal() {
    vec3 T = tangent.xyz;
    if(UseNormalMap == 0 || dot(T,T) < 0.1)
	return normalize(normal); // No normal-map or Tangents
    //return normalize(tangent.xyz);
    // Calc Bumped normal
    vec3 N = normalize(normal);
    T = normalize(T);
    T = normalize(T - dot(T, N) * N);
    vec3 B = cross(T, N) * tangent.w;
    vec3 BumpMapNormal = texture2D(NormalMap, gl_TexCoord[0].xy).xyz;
    BumpMapNormal = 2.0 * BumpMapNormal - vec3(1.0, 1.0, 1.0);
    vec3 NewNormal;
    mat3 TBN = mat3(T, B, N);
    NewNormal = TBN * BumpMapNormal;
    NewNormal = normalize(NewNormal);
    return NewNormal;
}

vec4 get_diffuse() {
    if(UseDiffuseMap > 0) return texture2D(DiffuseMap, gl_TexCoord[0].xy);
    else return vec4(1.0, 1.0, 1.0, 1.0);
}

void main(void)
{
    vec3  lightVec = normalize(LightPosition - ecPosition);
    float costheta = dot(get_normal(), lightVec);
    float a = 0.5 + 0.5 * costheta;
    vec4 difftex = get_diffuse();
    vec3 DiffuseColor = difftex.rgb * color.rgb;
    gl_FragColor = vec4(DiffuseColor * mix(GroundColor, SkyColor, a), difftex.a*color.a);
    //gl_FragColor = vec4(get_normal() * 0.5 + 0.5, 1.0);
}
