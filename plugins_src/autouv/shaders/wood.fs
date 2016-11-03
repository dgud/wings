//
//  wood.fs --
//
//     Wood 3D shaders based on some 2D version I found at ShaderFrog:
//     - Perin and Simplex: http://shaderfrog.com/app/view/123
//     - Tree Trunk: http://shaderfrog.com/app/view/282
//
//  Copyright (c) 2006 Dan Gudmundsson
//                2015 Micheus (Added Tree Trunk, Perlin and Simples noise variant)
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: wood.fs,v 1.4 2006/01/27 15:17:56 dgud Exp $
//

uniform int type;
uniform vec4  liteWood;
uniform vec4  darkWood;
uniform bool exchange;
uniform float frequency;
uniform float noiseScale;
uniform float scale;
uniform float ringScale;
uniform float contrast;
uniform float rotx;
uniform float roty;
uniform float rotz;
uniform float offsetx;
uniform float offsety;
uniform float offsetz;
uniform float stretchy;

uniform vec3 auv_bbpos3d[2];
varying vec3 w3d_pos;


#define cf_rad 0.0174532925277778 // 2x3.1415.../360.0;


// From : Ian McEwan, Ashima Arts.
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
  i = mod(i, 289.0 );
  vec4 p = permute( permute( permute(
             i.z + vec4(0.0, i1.z, i2.z, 1.0 ))
           + i.y + vec4(0.0, i1.y, i2.y, 1.0 ))
           + i.x + vec4(0.0, i1.x, i2.x, 1.0 ));

// Gradients: 7x7 points over a square, mapped onto an octahedron.
// The ring size 17*17 = 289 is close to a multiple of 47 (49*6 = 294)
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
// https://github.com/ashima/webgl-noise/blob/master/src/classicnoise3D.glsl
// ***************************************************************************
vec3 fade(vec3 t) {
  return t*t*t*(t*(t*6.0-15.0)+10.0);
}

// Classic Perlin noise
float cnoise(vec3 P) {
  vec3 Pi0 = floor(P); // Integer part for indexing
  vec3 Pi1 = Pi0 + vec3(1.0); // Integer part + 1
  Pi0 = mod(Pi0, 289.0 );
  Pi1 = mod(Pi1, 289.0 );
  vec3 Pf0 = fract(P); // Fractional part for interpolation
  vec3 Pf1 = Pf0 - vec3(1.0); // Fractional part - 1.0
  vec4 ix = vec4(Pi0.x, Pi1.x, Pi0.x, Pi1.x);
  vec4 iy = vec4(Pi0.yy, Pi1.yy);
  vec4 iz0 = Pi0.zzzz;
  vec4 iz1 = Pi1.zzzz;

  vec4 ixy = permute(permute(ix) + iy);
  vec4 ixy0 = permute(ixy + iz0);
  vec4 ixy1 = permute(ixy + iz1);

  vec4 gx0 = ixy0 * (1.0 / 7.0);
  vec4 gy0 = fract(floor(gx0) * (1.0 / 7.0)) - 0.5;
  gx0 = fract(gx0);
  vec4 gz0 = vec4(0.5) - abs(gx0) - abs(gy0);
  vec4 sz0 = step(gz0, vec4(0.0));
  gx0 -= sz0 * (step(0.0, gx0) - 0.5);
  gy0 -= sz0 * (step(0.0, gy0) - 0.5);

  vec4 gx1 = ixy1 * (1.0 / 7.0);
  vec4 gy1 = fract(floor(gx1) * (1.0 / 7.0)) - 0.5;
  gx1 = fract(gx1);
  vec4 gz1 = vec4(0.5) - abs(gx1) - abs(gy1);
  vec4 sz1 = step(gz1, vec4(0.0));
  gx1 -= sz1 * (step(0.0, gx1) - 0.5);
  gy1 -= sz1 * (step(0.0, gy1) - 0.5);

  vec3 g000 = vec3(gx0.x,gy0.x,gz0.x);
  vec3 g100 = vec3(gx0.y,gy0.y,gz0.y);
  vec3 g010 = vec3(gx0.z,gy0.z,gz0.z);
  vec3 g110 = vec3(gx0.w,gy0.w,gz0.w);
  vec3 g001 = vec3(gx1.x,gy1.x,gz1.x);
  vec3 g101 = vec3(gx1.y,gy1.y,gz1.y);
  vec3 g011 = vec3(gx1.z,gy1.z,gz1.z);
  vec3 g111 = vec3(gx1.w,gy1.w,gz1.w);

  vec4 norm0 = taylorInvSqrt(vec4(dot(g000, g000), dot(g010, g010), dot(g100, g100), dot(g110, g110)));
  g000 *= norm0.x;
  g010 *= norm0.y;
  g100 *= norm0.z;
  g110 *= norm0.w;
  vec4 norm1 = taylorInvSqrt(vec4(dot(g001, g001), dot(g011, g011), dot(g101, g101), dot(g111, g111)));
  g001 *= norm1.x;
  g011 *= norm1.y;
  g101 *= norm1.z;
  g111 *= norm1.w;

  float n000 = dot(g000, Pf0);
  float n100 = dot(g100, vec3(Pf1.x, Pf0.yz));
  float n010 = dot(g010, vec3(Pf0.x, Pf1.y, Pf0.z));
  float n110 = dot(g110, vec3(Pf1.xy, Pf0.z));
  float n001 = dot(g001, vec3(Pf0.xy, Pf1.z));
  float n101 = dot(g101, vec3(Pf1.x, Pf0.y, Pf1.z));
  float n011 = dot(g011, vec3(Pf0.x, Pf1.yz));
  float n111 = dot(g111, Pf1);

  vec3 fade_xyz = fade(Pf0);
  vec4 n_z = mix(vec4(n000, n100, n010, n110), vec4(n001, n101, n011, n111), fade_xyz.z);
  vec2 n_yz = mix(n_z.xy, n_z.zw, fade_xyz.y);
  float n_xyz = mix(n_yz.x, n_yz.y, fade_xyz.x);
  return 2.2 * n_xyz;
}
// ***************************************************************************

float rand( vec2 p ) {
    return fract( sin( dot( p.xy, vec2(12.898, 78.233) ) ) * 43758.5453 );
}

float noise(vec2 _v, vec2 _freq) {
    float fl1 = rand(floor(_v * _freq));
    float fl2 = rand(floor(_v * _freq) + vec2(1.0, 0.0));
    float fl3 = rand(floor(_v * _freq) + vec2(0.0, 1.0));
    float fl4 = rand(floor(_v * _freq) + vec2(1.0, 1.0));
    vec2 fr = fract(_v * _freq);
    float r1 = mix(fl1, fl2, fr.x);
    float r2 = mix(fl3, fl4, fr.x);
    return mix(r1, r2, fr.y);
}

float perlin_noise(vec2 _pos, float _freq_start, float _amp_start, float _amp_ratio) {
    float freq = _freq_start;
    float amp = _amp_start;
    float pn = noise(_pos, vec2(freq, freq)) * amp;
    for (int i = 0; i < 4; i++) {
        freq *= 2.0;
        amp *= _amp_ratio;
        pn += (noise(_pos, vec2(freq, freq)) * 2.0 - 1.0) * amp;
    }
    return pn;
}


float new_wood(vec3 pos, float noise) {
  float n = noise;
  float ring = fract(pos.z *frequency *scale + noiseScale *n);
  ring *= (contrast*.1+0.05) * (1.0 - ring);
  return (pow(ring, abs(ringScale*0.1+0.1)) + n);
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

void main(void) {
  float variation = noiseScale;
  float secondaryVariation = 4.0;
  float ringCount = 15.0+(40.0*frequency/20.0);
  float ringDarkness = contrast*0.1;

  vec3 ch_center = (auv_bbpos3d[1]-auv_bbpos3d[0]);
  float ch_scale  = scale*1.41421/length(ch_center);
  ch_center = (ch_center/2.0) + auv_bbpos3d[0];
  vec3 pos = vec3(w3d_pos-ch_center);
  pos.y *= (1.0+(-stretchy/300.0));
  pos = vec3(pos.x+(-offsetx/300.0),pos.y+(-offsety/300.0),pos.z+(-offsetz/300.0));
  float len = length(pos.xz);
  float rX = rotx*cf_rad;
  float rY = roty*cf_rad;
  float rZ = rotz*cf_rad;
  pos = rotate(pos,rX,rY,rZ);
  pos = (ch_scale*pos);
  float noise, lpr;
  float d1, d2;

  if (type == 0) {
    float noise = perlin_noise(pos.xz + vec2(20.0, 20.0), 20.0, 0.5, 0.4)+perlin_noise(pos.xy + vec2(20, 20), 5.0, 0.5, 0.4)+perlin_noise(pos.zy + vec2(20, 20), 5.0, 0.5, 0.4);
    d1 = smoothstep(0.7, 1.6, sin(ringCount * (len + variation * 0.01 * noise)));
    d2 = smoothstep(0.8, 1.6, sin(ringCount * (4.0+4.0*(200.0-frequency)/200.0) * (len + secondaryVariation * 0.01 * noise)));
  } else if (type == 1) {
      noise = cnoise(pos);
      lpr = new_wood(pos, noise);
  } else if (type == 2) {
      noise = snoise(pos);
      lpr = new_wood(pos, noise);
  }

	vec4 c1, c2;
	if (exchange) {
		c1 = darkWood;
		c2 = liteWood;
	} else {
		c1 = liteWood;
		c2 = darkWood;
  }

  if (type == 0) {
    gl_FragColor = vec4(vec3(liteWood-(d1 *0.6 *ringDarkness)-(d2 *0.3 *ringDarkness)), liteWood.a);
  } else {
    gl_FragColor = mix(c2, c1, lpr);
  }
}
