//
//  voronoi.fs --
//
//     Voronoi shader based from GLSL SandBox: http://glslsandbox.com/e#18294.0 and http://glslsandbox.com/e#17194.1
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
uniform float frequency;
uniform bool invert;
uniform bool squared;
uniform vec2 auv_texsz;

vec2 smooth2(vec2 uv) {
    return uv*uv*(3.-2.*uv);
}

vec2 hash2(vec2 uv) {
	float mode;
	if (squared) {
		mode = 9999999999999.0;
	} else {
		mode = 4555.23;
	}
	return fract(sin(mat2(15.23, 35.48, 74.26, 159.37) * uv) * mode);
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

float voronoi(const in vec2 uv)
{
    vec2 p = floor(uv);
    vec2 f = fract(uv);
	float d = 1.0;
	for(int i = -1; i <= 1; i++) {
		for(int j = -1; j <= 1; j++) {
			vec2 b = vec2(i, j);
			vec2 v = b + hash2(p + b) - f;
			d = min(d, length(v));
		}
	}
    return d;
}

//fractal brownian motion
float fbm(float a, float f, const in vec2 uv, const int it)
{
    float n = 0.;
    for(int i = 0; i < 32; i++)
    {
        if(i<it)
        {
            n += noise(uv*f)*a;
            a *= .5;
            f *= 2.;
        }
    }
    return n;
}

//voronoi fbm
float vfbm(float a, float f, const in vec2 uv, const in int it)
{
    float n = 0.;
    for(int i = 0; i < 32; i++)
    {
        if(i<it)
        {
            n += voronoi(uv*f)*a;
            f *= 2.;
            a *= .5;
        }
    }
    return n;
}

void main( void ) {
	float c;
	float d;
	vec2 uv = ( gl_FragCoord.xy / auv_texsz.xy );
	uv.x *= auv_texsz.x / auv_texsz.y;
	uv *= frequency;
	if (type == 1) {
	    d = voronoi(uv);
    } else if (type == 2) {
        d = vfbm(1.0,1.0,uv,4);
    } else if (type == 3) {
        d = fbm(1.0,1.0,uv,5);
    } else {
	    d = voronoi(uv);
    }

	c = sqrt(d);
	if (type == 1) {
		c *= sqrt(d);
	}

	if (invert) {
		c = 1.0-c;
	}

	gl_FragColor = vec4(color1.rgb*c + color2.rgb*(1.0-c), 1.0-min(color1.a*c +color2.a*(1.0-c),1.0));
}
