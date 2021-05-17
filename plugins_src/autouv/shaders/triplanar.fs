//  triplanar.fs --
//
//     Implemented the texture paint using triplanar
//     approach by using one image.
//
//  Copyright (c) 2021 Micheus
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: triplanar.fs,v 1.0 2021/02/20 10:00:0 micheus Exp $
//
#version 120

uniform sampler2D auv_bg;
// input parameters from wpc_image_mixer
uniform sampler2D image;

uniform float sharpness;
uniform float hscale;
uniform float vscale;
uniform float rot;
uniform float hshift;
uniform float vshift;
uniform int mixmode;
uniform float weight;

uniform vec3 auv_bbpos3d[2];
varying vec2 w3d_uv;
varying vec3 w3d_pos;
varying vec3 w3d_normal;

#define ZERO 0.0001
#define TO_RAD  0.0174532925277778 // 1 degree => radians;


mat2 rotateMtx2D(float rot) {
    rot *= TO_RAD;
    float cR = cos(rot);
    float sR = sin(rot);
    return mat2(cR,-sR,sR,cR);
}

vec2 transf2D(vec2 p, vec2 dom, vec2 scl, vec2 ofst, float rot) {
    mat2 rotMtx = rotateMtx2D(rot);
    vec2 center = vec2(0.5);  // finding the domain middle by dividing it by two
    p -= center;
    p *= scl;
    p *= rotMtx;
    p += (dom/2.0)*ofst;
    return p+center;
}

// "boxmap" code adapted from Inigo Quilez - BEGIN
//
// https://iquilezles.org/www/articles/biplanar/biplanar.htm
//
// "img..." texture sampler
// "p" point apply texture to (normalized)
// "n" normal at "p"
// "sharp" controls the sharpness of the blending in the
//     transitions areas.
vec4 boxmap(sampler2D img, vec3 p, vec3 n, vec4 scloft, float rot, float sharp)
{
    // inverting the texture in accord with axis direction
    vec3 inv = abs(n) / n;
    vec2 uv_y = vec2(-inv.y*p.z, -inv.y*p.x);
    vec2 uv_x = vec2(-inv.x*p.z, p.y);
    vec2 uv_z = vec2(inv.z*p.x, p.y);

    // sides: left-right/front-back
    uv_x = transf2D(uv_x, vec2(1.0,1.0), scloft.xy, scloft.zw, -rot);
    uv_z = transf2D(uv_z, vec2(1.0,1.0), scloft.xy, scloft.zw, -rot);
    // top-bottom
    uv_y = transf2D(uv_y, vec2(1.0,1.0), scloft.xy, scloft.zw, -rot);

    // project+fetch
    vec4 x = texture2D(img, uv_x);
    vec4 y = texture2D(img, uv_y);
    vec4 z = texture2D(img, uv_z);
    // and blend
    vec3 m = pow(abs(n), vec3(sharp));
    return vec4((x*m.x + y*m.y + z*m.z) / (m.x + m.y + m.z));
}
// code adaptation - END

vec4 pick_textel(sampler2D img, vec2 uv) {
    return texture2D(img, uv);
}


void main() {
    float mixVal = weight/100.0;
    vec4 bgCol = pick_textel(auv_bg, w3d_uv);
    // matching the 3D coordinate space to the UV space [0,1]
    vec3 pos = (w3d_pos-auv_bbpos3d[0]) / (auv_bbpos3d[1]-auv_bbpos3d[0]);
    vec3 nor = w3d_normal;

    // packing parameters
    vec2 scale = vec2(100.0/max(hscale,ZERO), 100.0/max(vscale,ZERO));
    vec2 offset = vec2(hshift/-100.0, vshift/-100.0);
    vec4 scloft = vec4(scale, offset);
    float sharp = sharpness/10.0;

    // computing the color
    vec4 mixCol = boxmap(image, pos, nor, scloft, rot, sharp);

    vec4 color;
    if (mixmode == 0) {  // Mix
        color = vec4(mix(bgCol.rgb, mixCol.rgb, mixVal), mixCol.a);
    } if (mixmode == 1) {  // Multiply
        color = vec4(mixCol.rgb*(bgCol.rgb*mixVal), mixCol.a);
    }
    gl_FragColor = clamp(color,0.0,1.0);
}
