//
//  tile.fs --
//
//     Simple tile shader
//
//  Copyright (c) 2015 Micheus
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: tile.fs,v 1.0 2015/10/16 21:40:0 micheus Exp $
//

uniform vec4  colorTile;
uniform float scalex;
uniform float scaley;
uniform float shift;
uniform vec4  colorMortar;
uniform float scaleMortar;
uniform vec2 auv_texsz;

#define size 1.0


void main( void ) {
    vec4 color;

	float bw = size*scalex;
	float bh = size*scaley;
	float sft = bw*shift;
	float lw = 1.0*scaleMortar;
	float x = gl_FragCoord.x;
	float y = gl_FragCoord.y;
	float bx = mod(y, bh*2.0) < bh ? x + sft : x;

	//mortar
	if ( mod(y+lw, bh) < lw || (mod(bx, bw)) < lw ) {
		color = colorMortar.rgba;
	} else {
	    color = colorTile.rgba;
	}
	gl_FragColor = color;
}

