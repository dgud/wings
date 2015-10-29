//
//  camuflage_pixelated.fs --
//
//     Pixelated camuflage shader stolen from GLSL SandBox: http://glslsandbox.com/e#22476.0
//
//  Copyright (c) 2015 Micheus
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: camuflage_pixelated.fs,v 1.0 2015/10/16 21:40:0 micheus Exp $
//

uniform vec4  colorPixels;
uniform float bright;
uniform float frequency;
uniform vec2 auv_texsz;

float rand(vec2 n)
{
	return fract(cos(dot(n, vec2(12.9898, 4.1414))) * 43758.5453);
}

float noise(vec2 n)
{
	const vec2 d = vec2(1.0, 1.0);

	vec2 b = floor(n), f = smoothstep(vec2(2.0), vec2(1.0), fract(n));

	return mix(mix(rand(b), rand(b + d.yx), f.x), mix(rand(b + d.xy), rand(1.0 * b + d.yy), f.x), f.y);
}

float fbm(vec2 n)
{
	float total = 0.0, amplitude = bright/100.0;

	for (int i = 0; i < 4; i++) {
		total += noise(n) * amplitude;
		n += n;
		amplitude *= 0.8;
	}
	return total;
}

void main(void)
{
	vec2 p = gl_FragCoord.xy * 40.0 / auv_texsz.xx - vec2(10.0,1.0)  * 0.1;

	float q = fbm(p*frequency*0.1);

	gl_FragColor = vec4(colorPixels.rgb*q,1.0-colorPixels.a);
}
