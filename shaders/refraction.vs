// $Id: envmap.fs 117 2007-07-19 16:44:01Z antoneos $
//
// Vertex shader for refraction
//
// Author: Randi Rost
//
// Copyright (c) 2003-2006: 3Dlabs, Inc.
//
// See 3Dlabs-License.txt for license information
//

const float Eta = 0.66;         // Ratio of indices of refraction 
const float FresnelPower = 5.0; 

const float F  = ((1.0-Eta) * (1.0-Eta)) / ((1.0+Eta) * (1.0+Eta));

varying vec3  Reflect;
varying vec3  Refract;
varying float Ratio;

void main()
{
    vec4 ecPosition  = gl_ModelViewMatrix * gl_Vertex;
    vec3 ecPosition3 = ecPosition.xyz / ecPosition.w;

    vec3 i = normalize(ecPosition3);
    vec3 n = normalize(gl_NormalMatrix * gl_Normal);

    Ratio   = F + (1.0 - F) * pow((1.0 - dot(-i, n)), FresnelPower);

    Refract = refract(i, n, Eta);
    Refract = vec3(gl_TextureMatrix[0] * vec4(Refract, 1.0));

    Reflect = reflect(i, n);
    Reflect = vec3(gl_TextureMatrix[0] * vec4(Reflect, 1.0));

    gl_Position = ftransform();
}
