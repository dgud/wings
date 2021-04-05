//
//  image_mixer.fs --
//
//     Allow us to mix an image to previous layer on texture
//
//  Copyright (c) 2021 Micheus
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: image_mixer.fs,v 1.0 2021/03/11 14:00:0 micheus Exp $
//

#version 120

uniform sampler2D image_mix;
uniform sampler2D auv_bg;

// input parameters from wpc_image_mixer
uniform float rot;
uniform float hshift;
uniform float vshift;
uniform float hscale;
uniform float vscale;
uniform bool tillable;
uniform int mixmode;
uniform float weight;

varying vec2 w3d_uv;

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

vec4 pick_textel(sampler2D img, vec2 uv) {
    return texture2D(img, uv);
}


void main( void ) {
    vec2 scale = vec2(100.0/max(hscale,ZERO), 100.0/max(vscale,ZERO));
    vec2 offset = vec2(hshift/-100.0, vshift/-100.0);
    vec2 uv = transf2D(w3d_uv, vec2(1.0,1.0)*scale, scale, offset, -rot);
    bool uni = (min(uv.x,uv.y) >= 0.0) && (max(uv.x,uv.y) <= 1.0);
    vec4 bgCol = pick_textel(auv_bg, w3d_uv);
    vec4 mixCol = pick_textel(image_mix, uv);
    float mixVal = weight/100.0;

    vec4 color;
    if (!tillable && !uni) {
        color = bgCol;
    } else {
        if (mixmode == 0) {  // Mix
            color = vec4(mix(bgCol.rgb, mixCol.rgb, mixVal), mixCol.a);
        } if (mixmode == 1) {  // Multiply
            color = vec4(mixCol.rgb*(bgCol.rgb*mixVal), mixCol.a);
        }
    }
    gl_FragColor = clamp(color,0.0,1.0);
}
