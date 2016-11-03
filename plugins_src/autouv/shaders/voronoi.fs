//
//  voronoi.fs --
//
//     Voronoi 3D shaders based on some I found at GLSL SandBox:
//     - http://glslsandbox.com/e#18294.0
//     - http://glslsandbox.com/e#17194.1
//
//  Copyright (c) 2015 Micheus
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: voronoi.fs,v 1.0 2015/10/27 21:40:0 micheus Exp $
//

uniform int type;
uniform vec4 color1;
uniform vec4 color2;
uniform bool exchange;
uniform float frequency;
uniform bool squared;
uniform float rotx;
uniform float roty;
uniform float rotz;
uniform int mixmode;
uniform bool invmask;

uniform vec3 auv_bbpos3d[2];
varying vec3 w3d_pos;

#define cf_rad 0.0174532925277778 // 2x3.1415.../360.0;

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

vec2 smooth2(vec2 uv) {
    return uv*uv*(3.-2.*uv);
}

vec3 hash3(vec3 pos) {
	float mode;
	if (squared) {
		mode = 9999999999999.0;
	} else {
		mode = 4555.23;
	}
	return fract(sin(mat3(15.23, 35.48, 74.26, 74.26, 159.37, 360.5, 360.5, 722.75, 1450.23) * pos) * mode);
}

float noise(in vec2 uv) {
    const float k = 257.;
    vec4 l  = vec4(floor(uv),fract(uv));
    float u = l.x + l.y * k;
    vec4 v  = vec4(u, u+1.,u+k, u+k+1.);
    v       = fract(fract(1.23456789*v)*v/.987654321);
    l.zw    = smooth2(l.zw);
    l.x     = mix(v.x, v.y, l.z);
    l.y     = mix(v.z, v.w, l.z);
    return    mix(l.x, l.y, l.w);
}

float voronoi3d(const in vec3 uv) {
    vec3 p = floor(uv);
    vec3 f = fract(uv);
	float d = 1.0;
	for(int i = -1; i <= 1; i++) {
		for(int j = -1; j <= 1; j++) {
		    for(int k = -1; k <= 1; k++) {
                vec3 b = vec3(i, j, k);
                vec3 v = b + hash3(p + b) - f;
                d = min(d, length(v));
            }
		}
	}
    return d;
}

//voronoi fbm
float vfbm(float a, float f, const in vec3 uv, const in int it) {
    float n = 0.;
    for(int i = 0; i < 32; i++)
    {
        if(i<it)
        {
            n += voronoi3d(uv*f)*a;
            f *= 2.;
            a *= .5;
        }
    }
    return n;
}

void main( void ) {
    vec3 ch_center = (auv_bbpos3d[1]-auv_bbpos3d[0]);
    float ch_scale  = frequency*1.41421/length(ch_center);
    ch_center = (ch_center/2.0) + auv_bbpos3d[0];
    vec3 pos = vec3(w3d_pos-ch_center);
    float rX = rotx*cf_rad;
    float rY = roty*cf_rad;
    float rZ = rotz*cf_rad;
    pos = rotate(pos,rX,rY,rZ);
    pos = (ch_scale*pos)+0.5;
	float d;

    if (type == 1) {
	    d = voronoi3d(pos);
    } else if (type == 2) {
        d = vfbm(1.0,1.0,pos,4);
    } else if (type == 3) {
        d = vfbm(1.0,1.0,pos,4);
    } else {
	    d = voronoi3d(pos);
    }

	d = sqrt(d);
	if (type == 1) {
		d *= sqrt(d);
    } else if (type == 3) {
        d = clamp(d,0.0,1.0);
	}

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
        gl_FragColor = vec4(c1.rgb*d + c2.rgb*(1.0-d), c1.a*d+c2.a*(1.0-d));
    } else if (mixmode == 1) {
        gl_FragColor = vec4(c1.rgb*d + c2.rgb*(1.0-d), d);
    }
}

