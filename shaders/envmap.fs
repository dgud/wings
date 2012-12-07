// $Id$
//
// Fragment shader for environment mapping with an
// equirectangular 2D texture
//
// Authors: John Kessenich, Randi Rost
//
// Copyright (c) 2002-2006 3Dlabs Inc. Ltd.
//
// See 3Dlabs-License.txt for license information
//

const vec3 Xunitvec = vec3(1.0, 0.0, 0.0);
const vec3 Yunitvec = vec3(0.0, 1.0, 0.0);

vec3  BaseColor = vec3(1.0, 1.0, 1.0);
float MixRatio = 0.3;

uniform sampler2D EnvMap;
uniform sampler2D NormalMap;

varying vec3  Normal;
varying vec3  EyeDir;
varying vec4 tangent;

vec3 LightPos = vec3(0.0, 10.0, 0.0);

vec3 get_normal() {
    ivec2 dim = textureSize(NormalMap, 0);
    vec3 T = tangent.xyz;
    if((dim.x <= 1 && dim.y <= 1) || dot(T,T) < 0.1)
	return normalize(Normal); // No normal-map or Tangents
    //return normalize(tangent.xyz);
    // Calc Bumped normal
    vec3 N = normalize(Normal);
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

void main()
{
    // Compute reflection vector

    vec3 reflectDir = reflect(EyeDir, get_normal());

    // Compute altitude and azimuth angles

    vec2 index;

    index.y = dot(normalize(reflectDir), Yunitvec);
    reflectDir.y = 0.0;
    index.x = dot(normalize(reflectDir), Xunitvec) * 0.5;

    // Translate index values into proper range

    if (reflectDir.z >= 0.0)
        index = (index + 1.0) * 0.5;
    else
    {
        index.t = (index.t + 1.0) * 0.5;
        index.s = (-index.s) * 0.5 + 1.0;
    }

    // if reflectDir.z >= 0.0, s will go from 0.25 to 0.75
    // if reflectDir.z <  0.0, s will go from 0.75 to 1.25, and
    // that's OK, because we've set the texture to wrap.

    // Do a lookup into the environment map.

    vec3 envColor = vec3(texture2D(EnvMap, index));

    // Add lighting to base color and mix
    float LightIntensity = max(dot(normalize(LightPos - EyeDir), Normal), 0.0);
    vec3 base = LightIntensity * BaseColor;
    envColor  = mix(envColor, BaseColor, MixRatio);

    gl_FragColor = vec4(envColor, 1.0);
}
