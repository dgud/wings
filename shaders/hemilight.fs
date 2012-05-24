// $Id$
//
// Fragment shader for hemispherical lighting
//
// Author: Dan Gudmundsson
//

uniform vec3 LightPosition;
uniform vec3 SkyColor;
uniform vec3 GroundColor;

uniform sampler2D DiffuseMap;
uniform sampler2D NormalMap;

varying vec3 normal;
varying vec3 ecPosition;
varying vec4 color;
varying vec4 tangent;

vec4 get_diffuse() {
    ivec2 dim = textureSize(DiffuseMap, 0);
    if(dim.x > 1 && dim.y > 1) return texture2D(DiffuseMap, gl_TexCoord[0].xy);
    else return vec4(1.0, 1.0, 1.0, 1.0);
}

vec3 get_normal() {
    ivec2 dim = textureSize(NormalMap, 0);
    vec3 T = tangent.xyz;
    if((dim.x <= 1 && dim.y <= 1) || dot(T,T) < 0.1)
	return normalize(normal); // No normal-map or Tangents
    // Calc Bumped normal
    vec3 N = normalize(normal);
    T = normalize(T);
    T = normalize(T - dot(T, N) * N);
    vec3 B = cross(T, N) * tangent.w;
    vec3 BumpMapNormal = texture(NormalMap, gl_TexCoord[0].xy).xyz;
    BumpMapNormal = 2.0 * BumpMapNormal - vec3(1.0, 1.0, 1.0);
    vec3 NewNormal;
    mat3 TBN = mat3(T, B, N);
    NewNormal = TBN * BumpMapNormal;
    NewNormal = normalize(NewNormal);
    return NewNormal;
}

void main(void)
{
    vec3  lightVec = normalize(LightPosition - ecPosition);
    float costheta = dot(get_normal(), lightVec);
    float a = 0.5 + 0.5 * costheta;
    vec3 DiffuseColor = get_diffuse() * color.rgb;
    gl_FragColor = vec4(DiffuseColor * mix(GroundColor, SkyColor, a), color.a);
    //gl_FragColor = vec4(get_normal() * 0.5 + 0.5, 1.0);
}
