//
//  camuflage_dots.fs --
//
//     Dotted camuflage shader stolen from GLSL SandBox: http://glslsandbox.com/e#25559.0
//
//  Copyright (c) 2015 Micheus
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: camuflage_dots.fs,v 1.0 2015/10/16 21:40:0 micheus Exp $
//

uniform vec4  colorPixels;
uniform float bright;
uniform float scale;

float rand(vec2 co){
  return fract(sin(dot(co.xy, vec2(152.9898, 778.233))) * 43758.5453);
}

void main (void) {
	// Divide the coordinates into a grid of squares
	vec2 v = (gl_FragCoord.xy  / 12.0) /scale;

	// Calculate a pseudo-random brightness value for each square
	float brightness = fract(rand(floor(v)));

	// Reduce brightness in pixels away from the square center
	brightness *= 1.0*(bright/100.0) - length(fract(v) - vec2(0.5, 0.5));

	gl_FragColor = vec4(colorPixels.r*brightness,colorPixels.g*brightness,colorPixels.b*brightness, 1.0-colorPixels.a);
}
