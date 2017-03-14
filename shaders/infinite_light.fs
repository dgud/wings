// $Id$
//
// Fragment shader for infinite lights
//
// Author: Dan Gudmundsson
//

#version 120

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
    float costheta;
    float pf;
    vec3 amb  = vec3(0.0,0.0,0.0);
    vec3 diff = vec3(0.0,0.0,0.0);
    vec3 spec = vec3(0.0,0.0,0.0);
    vec3 emi  = vec3(0.0,0.0,0.0);
    vec3 normal = get_normal();
    vec4 difftex = get_diffuse();
    vec3 lightVec0 = normalize(vec3(gl_LightSource[0].position));

    // Amb
    amb = vec3(gl_FrontMaterial.ambient) * vec3(gl_LightSource[0].ambient);
    // Diffuse
    costheta = clamp(dot(normal, lightVec0), 0, 1);
    diff = color.rgb*costheta*vec3(gl_LightSource[0].diffuse);

    // Specular
    if(costheta == 0.0) {
        pf = 0.0;
    } else {
        vec3 halfDir = normalize(lightVec0-ecPosition);
        float nDotVP = clamp(dot(normal, halfDir), 0, 1);
        pf = min(pow(nDotVP, gl_FrontMaterial.shininess), 10000.0);
        spec = vec3(gl_FrontMaterial.specular) * vec3(gl_LightSource[0].specular) * pf;
    }
    // Emission
    emi = vec3(gl_FrontMaterial.emission);

    // Two sided lighting (calc only diffuse)
    if(!gl_FrontFacing) {
        costheta = clamp(dot(-normal, lightVec0), 0, 1);
        diff += color.rgb*costheta*vec3(gl_LightSource[0].diffuse);
    }
    diff = clamp(emi+diff+amb, vec3(0.0), vec3(1.0));
    gl_FragColor = vec4(diff*difftex.rgb+spec, difftex.a*color.a);
}


