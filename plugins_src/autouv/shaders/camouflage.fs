//
//  camouflage.fs --
//
//     Camuflage 3D shader based on three 2D I found at GLSL SandBox:
//     - Checker: http://glslsandbox.com/e#23824.0
//     - Dotted: http://glslsandbox.com/e#25559.0
//     - Pixelated: http://glslsandbox.com/e#22476.0
//
//  Copyright (c) 2015 Micheus
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: camouflage.fs,v 1.0 2015/12/02 21:40:0 micheus Exp $
//

uniform int type;
uniform vec4  color1;
uniform vec4  color2;
uniform bool exchange;
uniform float bright;
uniform float frequency;
uniform float rotx;
uniform float roty;
uniform float rotz;
uniform int mixmode;
uniform bool invmask;

uniform vec3 auv_bbpos3d[2];
varying vec3 w3d_pos;

#define cf_rad 0.0174532925277778 // 2x3.1415.../360.0;
#define MAX_FREQUENCY 500.0

float rand(vec2 pos) {
  return fract(sin(dot(pos.xy, vec2(152.9898, 778.233))) * 43758.5453);
}

float rand1(vec2 pos) {
    return fract(sin(dot(pos.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

float noise(vec2 n) {
	const vec2 d = vec2(1.0, 1.0);
	vec2 b = floor(n), f = smoothstep(vec2(0.0), vec2(1.0), fract(n));
	return mix(mix(rand1(b), rand1(b + d.yx), f.x), mix(rand1(b + d.xy), rand1(b + d.yy), f.x), f.y);
}

float fbm(vec3 n) {
	float total = 0.0, a = bright/100.0;

	for (int i = 0; i < 4; i++) {
		total += (noise(n.xy)+noise(n.yz)+noise(n.zx)) * a;
		n += n;
		a *= 0.15;
	}
	return total;
}

vec3 rotate(vec3 pos, float a, float b, float y) {
  vec3 posn = normalize(pos);
  float ca = cos(-a);  // alpha
  float cb = cos(b);  // beta
  float cy = cos(-y); // gama | [-] from left to righ hand
  float sa = sin(-a);
  float sb = sin(b);
  float sy = sin(-y);
  mat4 RotMtx = mat4(cb*cy, cb*sy, sb, 0.0,
                     -sa*sb*cy-ca*sy, -sa*sb*sy+ca*cy, sa*cb, 0.0,
                     -ca*sb*cy+sa*sy, -ca*sb*sy-sa*cy, ca*cb, 0.0,
                     0.0, 0.0, 0.0, 1.0);
  vec4 pos4 = vec4(posn,1.0);
  pos4 = RotMtx*pos4;
  return vec3(pos4.xyz*length(pos));
}

void main( void ) {
  vec3 ch_len = (auv_bbpos3d[1]-auv_bbpos3d[0]);
  float ch_scale  = frequency*1.41421/length(ch_len/2.);
  vec3 ch_center = (ch_len/2.0) + auv_bbpos3d[0];
  vec3 pos = vec3(w3d_pos-ch_center);
  float rX = rotx*cf_rad;
  float rY = roty*cf_rad;
  float rZ = rotz*cf_rad;
  pos = rotate(pos,rX,rY,rZ);
  pos = (ch_scale*pos);
  pos = pos / length(ch_len);
  float d;

  if (type == 0) {           // Checker
    float freq = frequency/MAX_FREQUENCY*4.0;
    d = fract(sin(dot(floor(pos*15.0*freq),vec3(5.364,6.357,7.350)))*4357.536);
  } else if (type == 1) {    // Dot
    d = rand(floor(pos.yx)+floor(pos.yz)+floor(pos.xz))*4.0;
    d *= 0.5 - length(fract(pos) - vec3(0.5, 0.5, 0.5));
  } else if (type == 2) {    // Pixelated
    pos = pos * 40.0 / vec3(10.0,10.0,10.0)  * 0.1;
    d = fbm(pos*frequency/10.0);
  }

	if (invmask) {
		d = 1.0-d;
	}

	d *=bright/100.0;

	vec4 c1, c2;
	if (exchange) {
		c1 = color2;
		c2 = color1;
	} else {
		c1 = color1;
		c2 = color2;
  }

  if (mixmode == 0) {
    gl_FragColor = vec4(c1.rgb*d + c2.rgb*(1.0-d), c1.a*d+c2.a*(1.0-d));
  } else if (mixmode == 1) {
    gl_FragColor = vec4(c1.rgb*d + c2.rgb*(1.0-d), d);
  }
}
