//
//  edge_filter.fs --
//
//     Image filters which apply only to edges for seam-removal purposes.
//
//  Copyright (c) 2009 Dan Gudmundsson, 2009 Ed Kolis
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
// Grabbed from tutorial By Jérôme Guinot jegx [at] ozone3d [dot] net
//
#version 120

varying vec2 w3d_uv;
uniform sampler2D auv_bg;
uniform vec2 auv_texsz;
uniform float alpha_limit;

int calc_threshold(float res)
{
	if (res <= 128.0) {
		return 3;
	} else if (res <= 256.0) {
		return 5;
	} else if (res <= 512.0) {
		return 7;
	} else if (res <= 1024.0) {
		return 9;
	} else if (res <= 2048.0) {
		return 11;
	} else if (res <= 4096.0) {
		return 13;
	} else {
		return 15;
	}
}

void main(void)
{
    float step_w = 1.0/(auv_texsz.x);
    float step_h = 1.0/(auv_texsz.y);

    vec4 sum = vec4(0.0), tmp, result;

    int f_sz = calc_threshold(max(auv_texsz.x, auv_texsz.y));
	float div = (f_sz + 1.0)/2.0;

    float orig_x  = w3d_uv.x;
    float orig_y  = w3d_uv.y;
    float orig_dw = orig_y - div*step_h; // Center texel
    float orig_lt = orig_x - div*step_w; // Center texel
    float tx_weight = 0.0;
    float tx, ty;

    ty=orig_dw;
    for(int i=0; i < f_sz; i++) {
	ty += step_w;
	tx=orig_lt;
	for(int j=0; j < f_sz; j++) {
	    tx += step_h;
	    vec2 coord = vec2(tx,ty);
	    tmp = texture2D(auv_bg, coord);
	    if(tmp.a > alpha_limit) {
		tx_weight += 1.0;
		sum += tmp;
	    }
	}
    }

    tmp = texture2D(auv_bg, vec2(orig_x,orig_y));
    if(tmp.a > alpha_limit) { // Inside texture, keep it
	result = tmp;
    } else {
	if(tx_weight > 0.0) { // Close Outside Edge
	    //result.rgb = vec3(1.0,0.0,0.0); // Good check
	    result.rgb = vec3(sum.rgb / tx_weight);
	    result.a   = 1.0; // 0.3 + tx_weight/f_sz*f_sz;
	} else {  // Other
	    result = tmp;
	}
    }
    gl_FragColor = result;
}
