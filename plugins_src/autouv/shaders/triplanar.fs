//
//  triplanar.fs --
//
//     Implemented the texture paint using triplanar approach by using one image
//
//  Copyright (c) 2021 Micheus
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: triplanar.fs,v 1.0 2021/02/20 10:00:0 micheus Exp $
//
#version 120

uniform sampler2D imagex;
uniform sampler2D imagey;
uniform sampler2D imagez;
uniform sampler2D auv_bg;
uniform float shapnessy;
uniform float hoffsety;
uniform float voffsety;
uniform float scaley;
uniform float shapnessxz;
uniform float hoffsetxz;
uniform float voffsetxz;
uniform float scalexz;

varying vec3 w3d_pos;
varying vec3 w3d_normal;

// "boxmap" code adapted from Inigo Quilez - BEGIN
//
// https://iquilezles.org/www/articles/biplanar/biplanar.htm
//
// "s" texture sampler
// "p" point apply texture to
// "n" normal at "p"
// "k" controls the sharpness of the blending in the
//     transitions areas.
vec4 boxmap(sampler2D sx, sampler2D sy, sampler2D sz, vec3 p,
            vec3 n, vec2 ofsy, vec2 ofsxz, vec2 scl, vec2 k)
{
    // inverting the texture in accord with axis direction
    vec3 inv = abs(n) / n;
    // project+fetch
    vec4 x = texture2D(sx, vec2(-inv.x*p.z, p.y) * scl.x + inv.x*ofsxz);
    vec4 y = texture2D(sy, vec2(-inv.y*p.z, -inv.y*p.x) * scl.y + ofsy);
    vec4 z = texture2D(sz, vec2(inv.z*p.x, p.y) * scl.x + inv.z*ofsxz);
    // and blend
    vec3 m = pow(abs(n), k.xyx);
    return (x*m.x + y*m.y + z*m.z) / (m.x + m.y + m.z);
}
// code adaptation - END

void main() {
    vec3 pos = w3d_pos;
    vec3 nor = w3d_normal;
    // packing paramters
    vec2 scl = vec2(scalexz/100.0, scaley/100.0);
    vec2 sharp = vec2(shapnessxz/10.0, shapnessy/10.0);
    vec2 ofsy = vec2(hoffsety/100.0, voffsety/100.0);
    vec2 ofsxz = vec2(hoffsetxz/100.0, voffsetxz/100.0);

    vec3 color = boxmap(imagex, imagey, imagez, pos, nor, ofsy, ofsxz, scl, sharp).xyz;
    gl_FragColor = vec4(clamp(color,0.0,1.0), 1.0);
}
