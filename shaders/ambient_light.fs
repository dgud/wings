// $Id$
//
// Fragment shader for camera lighting
//
// Author: Dan Gudmundsson
//

#version 120

uniform int UseDiffuseMap;

uniform sampler2D DiffuseMap;

varying vec3 ecPosition;
varying vec4 color;

vec4 get_diffuse() {
    if(UseDiffuseMap > 0) return texture2D(DiffuseMap, gl_TexCoord[0].xy);
    else return vec4(1.0, 1.0, 1.0, 1.0);
}

void main(void)
{
    vec3 amb  = vec3(0.0,0.0,0.0);
    vec4 difftex = get_diffuse();
    vec3 diffuseColor = difftex.rgb * color.rgb;

    // Amb
    amb = vec3(gl_FrontMaterial.ambient) * vec3(gl_LightModel.ambient);
    gl_FragColor = vec4(amb, difftex.a*color.a);
}
