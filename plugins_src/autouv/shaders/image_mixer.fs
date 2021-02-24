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
//     $Id: image_mixer.fs,v 1.0 2020/05/11 10:00:0 micheus Exp $
//

uniform sampler2D image_mix;
uniform sampler2D auv_bg;
uniform float hoffset;
uniform float voffset;
uniform float scale;
uniform int mixmode;
uniform float weight;
uniform vec2 auv_texsz;

varying vec2 w3d_uv;

vec4 pick_textel(sampler2D Img, vec2 UV) {
    return texture2D(Img, UV.xy);
}


void main( void ) {
    vec4 color;
    float scl0 = scale/100.0;
    float hofs = auv_texsz.x * (hoffset/100.0);
    float vofs = auv_texsz.y * (voffset/100.0);

    vec2 coord = vec2(w3d_uv.x+hofs, w3d_uv.y+vofs) / scl0;
    vec4 col = pick_textel(image_mix,coord.xy) * (weight/100.0);
    vec4 p_col = pick_textel(auv_bg,w3d_uv.xy);
    if (mixmode == 0) {  // Add
        color = col+p_col;
    } else {  // Multiply
        color = col*p_col;
    }
    gl_FragColor = clamp(color,0.0,1.0);
}
