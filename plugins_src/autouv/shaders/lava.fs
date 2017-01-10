//  lava.fs --
//
//     Lava and Fire 3D shader based on 2D I found at Shader Frog:
//     - Lava: http://shaderfrog.com/app/view/76
//     - Oil or Fire: http://shaderfrog.com/app/view/35
//
//  Copyright (c) 2015 Micheus
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: lava.fs,v 1.0 2015/11/25 10:00:0 micheus Exp $
//

uniform int type;
uniform vec4 color1;
uniform bool invert;
uniform float brightness;
uniform int smoothlevel;
uniform float frequency;
uniform float convolution;
uniform float rotx;
uniform float roty;
uniform float rotz;

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

float apply_smooth(float d, int level) {
  for (int i=0; i < level; i++) {
    d = sqrt(d);
  }
  return d;
}

vec4 permute(vec4 x) {
  return mod(((x * 34.0) + 1.0) * x, 289.0);
}

vec4 taylorInvSqrt(vec4 r) {
  return 1.79284291400159 - 0.85373472095314 * r;
}

float snoise(vec3 v) {
  const vec2  C = vec2(1.0/6.0, 1.0/3.0) ;
  const vec4  D = vec4(0.0, 0.5, 1.0, 2.0);

// First corner
  vec3 i  = floor(v + dot(v, C.yyy));
  vec3 x0 =   v - i + dot(i, C.xxx) ;

// Other corners
  vec3 g = step(x0.yzx, x0.xyz);
  vec3 l = 1.0 - g;
  vec3 i1 = min(g.xyz, l.zxy);
  vec3 i2 = max(g.xyz, l.zxy);

  //   x0 = x0 - 0.0 + 0.0 * C.xxx;
  //   x1 = x0 - i1  + 1.0 * C.xxx;
  //   x2 = x0 - i2  + 2.0 * C.xxx;
  //   x3 = x0 - 1.0 + 3.0 * C.xxx;
  vec3 x1 = x0 - i1 + C.xxx;
  vec3 x2 = x0 - i2 + C.yyy; // 2.0*C.x = 1/3 = C.y
  vec3 x3 = x0 - D.yyy;      // -1.0+3.0*C.x = -0.5 = -D.y

// Permutations
  i = mod(i, 289.0);
  vec4 p = permute(permute(permute(
             i.z + vec4(0.0, i1.z, i2.z, 1.0))
           + i.y + vec4(0.0, i1.y, i2.y, 1.0))
           + i.x + vec4(0.0, i1.x, i2.x, 1.0));

// Gradients: 7x7 points over a square, mapped onto an octahedron.
// The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
  float n_ = 0.142857142857; // 1.0/7.0
  vec3  ns = n_ * D.wyz - D.xzx;

  vec4 j = p - 49.0 * floor(p * ns.z * ns.z);  //  mod(p,7*7)

  vec4 x_ = floor(j * ns.z);
  vec4 y_ = floor(j - 7.0 * x_);    // mod(j,N)

  vec4 x = x_ *ns.x + ns.yyyy;
  vec4 y = y_ *ns.x + ns.yyyy;
  vec4 h = 1.0 - abs(x) - abs(y);

  vec4 b0 = vec4(x.xy, y.xy);
  vec4 b1 = vec4(x.zw, y.zw);

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
  return 42.0 * dot(m*m, vec4(dot(p0,x0), dot(p1,x1),
                                dot(p2,x2), dot(p3,x3)));
}

float rand(vec2 n) {
	return fract(cos(dot(n, vec2(12.9898, 4.1414))) * 43758.5453);
}

float noise(vec2 n) {
	const vec2 d = vec2(0.0, 1.0);
	vec2 b = floor(n), f = smoothstep(vec2(0.0), vec2(1.0), fract(n));
	return mix(mix(rand(b), rand(b + d.yx), f.x), mix(rand(b + d.xy), rand(b + d.yy), f.x), f.y);
}

float fbm(float a, vec3 n) {
	float total = 0.0;
	for (int i = 0; i < 7; i++) {
		total += (noise(n.xy)+noise(n.yz)+noise(n.zx)) * a;
		n += n;
		a *= 0.5;
	}
	return total;
}

float heightMap(vec3 coord) {
  float n = abs(snoise(coord));

  n += 0.25   * abs(snoise(coord * 2.0));
  n += 0.25   * abs(snoise(coord * 4.0));
  n += 0.125  * abs(snoise(coord * 8.0));
  n += 0.0625 * abs(snoise(coord * 16.0));

  return n;
}

vec3 flame(float q, vec3 pos) {
  const vec3 c1 = vec3(0.1, 0.0, 0.0);
  const vec3 c2 = vec3(0.7, 0.0, 0.0);
  const vec3 c3 = vec3(0.2, 0.0, 0.0);
  const vec3 c4 = vec3(1.0, 0.9, 0.0);
  const vec3 c5 = vec3(0.1);
  const vec3 c6 = vec3(0.9);

	vec3 r = vec3(fbm(1.0,pos + q + 0.7 - pos.x - pos.y),
	              fbm(1.0,pos + q - 0.4),
	              fbm(1.0,pos + q + 0.1 - pos.z - pos.x));
	return vec3(mix(c1, c2, fbm(1.0,pos + r)) + mix(c3, c4, r.x) - mix(c5, c6, r.y));

}

void main(void) {
  vec3 ch_center = (auv_bbpos3d[1]-auv_bbpos3d[0]);
  float ch_scale = frequency*1.41421/length(ch_center);
  ch_center = (ch_center/2.0) + auv_bbpos3d[0];
  vec3 pos = vec3(w3d_pos-ch_center);
  float rX = rotx*cf_rad;
  float rY = roty*cf_rad;
  float rZ = rotz*cf_rad;
  pos = rotate(pos,rX,rY,rZ);
  pos = (ch_scale*pos);
  float d;
  vec3 c1;

  if (type == 0) {
    d = heightMap(pos);
    d *= 1.0 +7.0 *convolution/100.0;
    d = apply_smooth(d,smoothlevel);
  } else if (type == 3) {
    d = heightMap(pos);
    d *= 1.0 +7.0 *convolution/100.0;
    d = apply_smooth(d,smoothlevel);
  } else {
    d = fbm(1.0+convolution/100.0, pos*0.50 - 0.1);
    d = apply_smooth(d,smoothlevel);
  }

  if (invert) {
    d = 1.0-d;
  }

  if (type == 1) {
    c1 = flame(d,pos);
    c1 = c1*vec3(brightness/300.0);
  } else if ((type == 2) || (type == 3)) {
    c1 = flame(d,pos);
    float f = ((c1.r+c1.g+c1.b)/3.0);
    c1 = (f+color1.rgb);
    c1 = c1*vec3(brightness/200.0);
  } else {
    c1 = color1.rgb*1.5 - vec3(d);
    c1 = c1*vec3(brightness/100.0);
  }

  gl_FragColor = vec4(c1, color1.a);
}
