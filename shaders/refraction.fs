// $Id: envmap.fs 117 2007-07-19 16:44:01Z antoneos $
//
// Fragment shader for refraction
//
// Author: Randi Rost
//
// Copyright (c) 2003-2006: 3Dlabs, Inc.
//
// See 3Dlabs-License.txt for license information
//

varying vec3  Reflect;
varying vec3  Refract;
varying float Ratio;

//uniform samplerCube Cubemap;
uniform sampler2D EnvMap;

void main()
{
    //vec3 refractColor = vec3(textureCube(Cubemap, Refract));
    //vec3 reflectColor = vec3(textureCube(Cubemap, Reflect));
    vec3 refractColor = vec3(texture2D(EnvMap, Refract.xy));
    vec3 reflectColor = vec3(texture2D(EnvMap, Reflect.xy));

    vec3 color   = mix(refractColor, reflectColor, Ratio);

    gl_FragColor = vec4(color, 1.0);
}
