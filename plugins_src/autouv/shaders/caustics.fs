//  caustics.fs --
//
//     Caustics 3D shader based on a 2D I found at ShaderFrog:
//     - http://shaderfrog.com/app/view/76
//
//  Copyright (c) 2015 Micheus
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: caustics.fs,v 1.0 2015/11/25 10:00:0 micheus Exp $
//

uniform vec4 color1;
uniform vec4 color2;
uniform bool exchange;
uniform float brightness;
uniform float density;
uniform float speed;
uniform float sharpness;
uniform float rotx;
uniform float roty;
uniform float rotz;
uniform int mixmode;
uniform bool invmask;

uniform vec3 auv_bbpos3d[2];
varying vec3 w3d_pos;


#define cf_rad 0.0174532925277778 // 2x3.1415.../360.0;
#define TAU 6.28318530718
#define MAX_ITER 6

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

void main() {
    vec3 ch_center = (auv_bbpos3d[1]-auv_bbpos3d[0]);
    float ch_scale  = (1.41421/length(ch_center)-.04)*(1.1-(100.0-density)/100.);
    ch_center = (ch_center/2.0) + auv_bbpos3d[0];
    vec3 pos = vec3(w3d_pos-ch_center);
    float rX = rotx*cf_rad;
    float rY = roty*cf_rad;
    float rZ = rotz*cf_rad;
    pos = rotate(pos,rX,rY,rZ);
    pos = (ch_scale*pos)+0.5;

    vec3 p = mod(pos * TAU, TAU)+2.0;// - 250.0;
    vec3 i = p;
    float inten = sharpness/1500.0 +0.0001;
    float d = 1.0;

    for ( int n = 0; n < MAX_ITER; n++ )  {
        float t = speed * (1.0 - (3.5 / float(n + 1)));
        i = p + vec3(cos(t - i.x) + sin(t + i.y), (cos(t - i.x) + cos(t + i.z) + cos(t - i.z) +cos(t + i.x)), sin(t - i.y) + cos(t + i.z));
        d += 1.0 / length(vec3(p.x / (sin(i.x + t) / inten), p.y / (sin(i.y)+cos(i.y)), p.z / (cos(i.z + t) /  inten)));
    }

    d /= float(MAX_ITER);
    d = 1.17 - pow(d, brightness/70.0);

    d = pow(abs(d), 4.0);
	if (invmask) {
		d = 1.0-d;
    }

    vec4 c1, c2;
	if (exchange) {
		c1 = color2;
		c2 = color1;
	} else {
		c1 = color1;
		c2 = color2;
	}

    if (mixmode == 0) {
        gl_FragColor = vec4(c2.rgb*d + c1.rgb*(1.0-d), c2.a*d+c1.a*(1.0-d));
    } else if (mixmode == 1) {
        gl_FragColor = vec4(c2.rgb*d + c1.rgb*(1.0-d), d);
    }
}
