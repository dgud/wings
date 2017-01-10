//
//  marble.fs --
//
//     Marble 3D shader based in the article "Building a better marble" by Christian Marten
//     - http://www.tinysg.de/techGuides/tg1_proceduralMarble.html
//
//  Copyright (c) 2015 Micheus
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: marble.fs,v 1.0 2015/11/07 21:40:0 micheus Exp $
//

uniform int type;
uniform vec4 color1;
uniform vec4 color2;
uniform bool exchange;
uniform float frequency;
uniform float amplitude;
uniform float rougness;
uniform float rotx;
uniform float roty;
uniform float rotz;
uniform int mixmode;
uniform bool invmask;

uniform vec3 auv_bbpos3d[2];
varying vec3 w3d_pos;

#define cf_rad 0.0174532925277778 // 2x3.1415.../360.0;

// ***************************************************************************
// Fragment code from: Noise for GLSL 1.20 by Ashima
//
// https://github.com/ashima/webgl-noise/wiki
// Source: https://github.com/ashima/webgl-noise/blob/master/src/noise3D.glsl
// ***************************************************************************
vec4 permute(vec4 x) {
  return mod(((x*34.0)+1.0)*x, 289.0);
}

vec4 taylorInvSqrt(vec4 r) {
  return 1.79284291400159 - 0.85373472095314 * r;
}

float snoise(vec3 v) {
  const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
  const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

// First corner
  vec3 i  = floor(v + dot(v, C.yyy) );
  vec3 x0 =   v - i + dot(i, C.xxx) ;

// Other corners
  vec3 g = step(x0.yzx, x0.xyz);
  vec3 l = 1.0 - g;
  vec3 i1 = min( g.xyz, l.zxy );
  vec3 i2 = max( g.xyz, l.zxy );

  //   x0 = x0 - 0.0 + 0.0 * C.xxx;
  //   x1 = x0 - i1  + 1.0 * C.xxx;
  //   x2 = x0 - i2  + 2.0 * C.xxx;
  //   x3 = x0 - 1.0 + 3.0 * C.xxx;
  vec3 x1 = x0 - i1 + C.xxx;
  vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
  vec3 x3 = x0 - D.yyy;      // -1.0+3.0*C.x = -0.5 = -D.y

// Permutations
  i = mod(i, 289.0);
  vec4 p = permute( permute( permute(
             i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
           + i.y + vec4(0.0, i1.y, i2.y, 1.0 ))
           + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

// Gradients: 7x7 points over a square, mapped onto an octahedron.
// The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
  float n_ = 0.142857142857; // 1.0/7.0
  vec3  ns = n_ * D.wyz - D.xzx;

  vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  //  mod(p,7*7)

  vec4 x_ = floor(j * ns.z);
  vec4 y_ = floor(j - 7.0 * x_ );    // mod(j,N)

  vec4 x = x_ *ns.x + ns.yyyy;
  vec4 y = y_ *ns.x + ns.yyyy;
  vec4 h = 1.0 - abs(x) - abs(y);

  vec4 b0 = vec4( x.xy, y.xy );
  vec4 b1 = vec4( x.zw, y.zw );

  //vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0;
  //vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0;
  vec4 s0 = floor(b0)*2.0 + 1.0;
  vec4 s1 = floor(b1)*2.0 + 1.0;
  vec4 sh = -step(h, vec4(0.0));

  vec4 a0 = b0.xzyw + s0.xzyw*sh.xxyy ;
  vec4 a1 = b1.xzyw + s1.xzyw*sh.zzww ;

  vec3 p0 = vec3(a0.xy,h.x);
  vec3 p1 = vec3(a0.zw,h.y);
  vec3 p2 = vec3(a1.xy,h.z);
  vec3 p3 = vec3(a1.zw,h.w);

//Normalise gradients
  vec4 norm = taylorInvSqrt(vec4(dot(p0,p0), dot(p1,p1), dot(p2, p2), dot(p3,p3)));
  p0 *= norm.x;
  p1 *= norm.y;
  p2 *= norm.z;
  p3 *= norm.w;

// Mix final noise value
  vec4 m = max(0.6 - vec4(dot(x0,x0), dot(x1,x1), dot(x2,x2), dot(x3,x3)), 0.0);
  m = m * m;
  return 42.0 * dot( m*m, vec4( dot(p0,x0), dot(p1,x1),
                                dot(p2,x2), dot(p3,x3) ) );
}
// ***************************************************************************

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

float rand(vec3 pos) {
    return fract(sin(dot(pos.xy ,vec2(12.9898,78.233))) * 43758.5453);
}

float turbulence2 (vec3 P, int numFreq) {
    float val = 0.0;
    float freq = 1.0;
    for (int i=0; i<numFreq; i++) {
        val += abs (snoise (P*freq) / freq);
        freq *= 1.25;
    }
    return val;
}

// Expects -1<x<1
float marble_color (float x, int level) {
    vec3 col;
    x = 0.5*(x+1.);          // transform -1<x<1 to 0<x<1
    for (int i=0; i<level; i++) {
        x = sqrt(x);         // hi level value make x fall of rapidly...
    }
    return x;
}

float calc_marble(vec3 pos, float f, float a, int r) {
     const float PI = 3.1415;

     float asc = f* snoise (vec3(1.5));  // use this as m in t=my+x, rather than just using t=x.
     float t = 2.0*PI*(pos.x + (asc*pos.z));

     t += a*turbulence2(pos, r);
     t = cos(t);

     float c = marble_color(t,4);
     return c;
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

    vec4 c1, c2;
    float d = calc_marble(pos,frequency,amplitude,int(floor(rougness)));
    if (type == 0) {
        c1 = vec4(0.95,0.95,0.9025,1.0);        // scale x from 0<x<1 to 0.2<x<0.95 // b -> slightly reduce blue component (make color "warmer"):
        c2 = vec4(0.05,0.05,0.05,1.0);
    } else {
        c1 = color1;
        c2 = color2;
    }

    if (!invmask) {
        d = 1.0-d;
    }

    if (exchange) {
        vec4 ct = c1;
        c1 = c2;
        c2 = ct;
    }

    if (mixmode == 0) {
        gl_FragColor = vec4(c2.rgb*d + c1.rgb*(1.0-d), c1.a*(1.0-d)+c2.a*d);
    } else if (mixmode == 1) {
        gl_FragColor = vec4(c2.rgb*d + c1.rgb*(1.0-d), d);
    }
}

