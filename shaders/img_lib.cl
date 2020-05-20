// -*- c++ -*-
// Author: Dan Gudmundsson
// Copyright (C) 2018 Dan Gudmundsson
// See the file "license.terms" for information on usage and redistribution
// of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//

// See http://blog.selfshadow.com/publications/s2013-shading-course/karis/s2013_pbs_epic_notes_v2.pdf
// And also from "inspired by" IBLBaker Matt Davidson
#define FLOATMASK 0x00ffffffu

#define PI 3.1415926

typedef struct {
	unsigned int s1, s2, s3;
} Seed;

float2 sample_hemisphere2d(float2 uv, Seed *seed);
float3 sample_hemisphere3d(float2 uv, float3 n, float a);
float3 sample_sphere3d(float2 uv);
float2 hammersley(uint i, uint n);
uint revbits(uint bits);
float G_schlick(float NoV, float r2);

float3 uv2vec(float2 uv);
float2 vec2uv(float3 vec);
float4 srgb2linear(float4 srgbIn);

// Random
uint tausworthe(const uint s, const uint a, const uint b, const uint c,	const uint d);
uint lcg(const uint x);
uint validseed(const uint x, const uint m);
void initrandomgenerator(uint seed, Seed *s);
unsigned long rndUint(Seed *s);
float rndFloat(Seed *s);
// Random

// Build an irradiance map equirectangular to equirectangular
// For each normal sample it's hemisphere
__kernel void make_diffuse(__read_only image2d_t inImg,
                           const int w, const int h,
                           __global float4 *prevImg,
                           __global float4 *outImg,
                           const int pass, const int passes)
{
  const sampler_t sampler=CLK_NORMALIZED_COORDS_TRUE|CLK_ADDRESS_REPEAT|CLK_FILTER_LINEAR;
  const int samples = 100;
  int x = (int) get_global_id(0);
  int y = (int) get_global_id(1);
  if(x < w && y < h) {
    int pos = x+y*w;
    float2 uv0 = (float2) (((float)x + 0.5)/ w, ((float)y + 0.5)/ h);
    float3 N = uv2vec(uv0);
    float4 c = (float4) 0.0f;
    int total  = passes*samples;
    int sample = pass*samples;
    for(int i = 0; i < samples; i++, sample++) {
        float2 xi = hammersley(sample, total);
	float3 V = sample_hemisphere3d(xi, N, 1.0f);
        float2 uv = vec2uv(V);
        float NoV = fabs(dot(N, V));
        float4 col = srgb2linear(read_imagef(inImg, sampler, uv));
        col *= NoV;
        col.w = NoV;
        c += col;
    };
    outImg[pos] = prevImg[pos] + c;
  }
}

__kernel void make_specular(__read_only image2d_t inImg,
                            const int w, const int h,
                            const float roughness,
                            __global float4 *prevImg,
                            __global float4 *outImg,
                            const int pass, const int passes)
{
  const sampler_t sampler=CLK_NORMALIZED_COORDS_TRUE|CLK_ADDRESS_REPEAT|CLK_FILTER_LINEAR;
  const int samples = 100;
  int x = (int) get_global_id(0);
  int y = (int) get_global_id(1);
  if(x < w && y < h) {
    int pos = x+y*w;
    float2 uv0 = (float2) (((float)x + 0.5)/ w, ((float)y + 0.5)/ h);
    float3 N = uv2vec(uv0);
    float4 c = (float4) 0.0f;
    float r2 = roughness*roughness;
    int total  = passes*samples;
    int sample = pass*samples;
    for(int i = 0; i < samples; i++, sample++) {
        float2 xi = hammersley(sample, total);
	float3 H = sample_hemisphere3d(xi, N, r2);
        float3 L = normalize(2 * dot( N, H ) * H - N);
        float NoL = dot( N, L );
        if( NoL > 0) {
            float2 uv = vec2uv(L);
            float4 col = srgb2linear(read_imagef(inImg, sampler, uv));
            col *= NoL;
            col.w = NoL;
            c += col;
        }
    };
    outImg[pos] = prevImg[pos] + c;
  }
}

// Calc schlick brdf texture lookup
__kernel void schlick_brdf(__global float2 *outImg,
                           const int w, const int h)
{
  int x = (int) get_global_id(0);
  int y = (int) get_global_id(1);
  const int samples = 200;
  if (x < w && y < h) {
      float2 c = (float2) 0.0f;
      float roughness = ((float)y + 0.5)/ h;
      float roughness2 = roughness*roughness;
      float NoV = ((float)x + 0.5)/ w;
      float3 N = (float3) (0.0f, 0.0f, 1.0f);
      float3 V = normalize((float3) (sqrt(1.0f - NoV * NoV), 0.0f, NoV));
      float vis = G_schlick(dot(N,V), roughness2);
      for(int i = 0; i < samples; i++) {
          float2 xi = hammersley(i, samples);
          float3 H = sample_hemisphere3d(xi, N, roughness2);
          float VoH = dot(V, H);
          float3 l = 2.0f * VoH * H - V;
          float lz = clamp(l.z, 0.0f, 1.0f);
          float G = G_schlick(lz, roughness2);
          float F = pow(1.0f-VoH, 5.0f);
          // sum it up
          float gvis = G *vis * VoH / (dot(N,H)*NoV);
          c.x += (1.0 - F) * gvis;
          c.y += F * gvis;
      };
      outImg[x+w*y] = clamp(c/samples, 0.0f, 1.0f);
  }
}

// prepare for rgb conversion
// Simple tonemapping and gamma correct
__kernel void color_convert(__global float4 *inImg, __global float4 *outImg, const int w, const int h)
{
  int x = (int) get_global_id(0);
  int y = (int) get_global_id(1);
  if (x < w && y < h) {
      // Color convert low def (RGB8) images
      int pos = x+y*w;
      float4 c = inImg[pos];
      c /= c.w;
      c.xyz *= 1.8f;
      float lum = c.x * 0.2126 + c.y * 0.72152 + c.z * 0.0722;
      c.xyz = c.xyz / (1.0f + lum);
      c.xyz = pow(c.xyz, 1.0/2.2f);
      c = clamp(c, 0.0f, 1.0f);
      c.w = 1.0;
      outImg[pos] = c;
  }
}

__kernel void height2normal(__read_only image2d_t inImg,
                            const int w, const int h,
                            const float s_x, const float s_y,
                            __global float4 *outImg
                            )
{
  const sampler_t sampler=CLK_NORMALIZED_COORDS_FALSE|CLK_ADDRESS_CLAMP|CLK_FILTER_LINEAR;
  int x = (int) get_global_id(0);
  int y = (int) get_global_id(1);
  if(x < w && y < h) {
    float4 cx0 = read_imagef(inImg, sampler, (float2)((x+w-1)%w, y));
    float4 cx1 = read_imagef(inImg, sampler, (float2)((x+1)%w, y));
    float4 cy0 = read_imagef(inImg, sampler, (float2)(x, (y+h-1)%h));
    float4 cy1 = read_imagef(inImg, sampler, (float2)(x, (y+1)%h));

    float nx = (cx1.x - cx0.x)*-s_x;
    float ny = (cy1.x - cy0.x)*-s_y;
    float3 dir = normalize((float3) (nx, ny, 1.0));
    outImg[x+y*w] = (float4) (dir, 1.0);
  }
}


__kernel void mm_normalmap(__global float4 *inImg, const int w, const int h, __global float4 *outImg)
{ // Make mipmap to normalmaps
  int x = (int) get_global_id(0);
  int y = (int) get_global_id(1);
  int w2 = w*2;
  if(x < w && y < h)
  {
    int x1 = x*2;
    int x2 = x*2+1;
    int y1 = y*2*w2;
    int y2 = (y*2+1)*w2;
    float3 v0 = inImg[y1+x1].xyz;
    float3 v1 = inImg[y1+x2].xyz;
    float3 v2 = inImg[y2+x1].xyz;
    float3 v3 = inImg[y2+x2].xyz;
    outImg[y*w+x] = (float4) (normalize(v0+v1+v2+v3), 0.0);
  }
}

__kernel void rgba_to_normal(__read_only image2d_t inImg, const int w, const int h, __global float4 *outImg)
{
  const sampler_t sampler=CLK_NORMALIZED_COORDS_FALSE|CLK_ADDRESS_CLAMP|CLK_FILTER_LINEAR;
  int x = (int) get_global_id(0);
  int y = (int) get_global_id(1);
  if(x < w && y < h) {
    float4 cx0 = read_imagef(inImg, sampler, (float2)(x+0.5f, y+0.5f));
    outImg[y*w+x] = cx0*2.0f-1.0f;
  }
}

__kernel void normal_to_rgba(__global float4 *inImg, const int w, const int h, __write_only image2d_t outImg)
{
  int x = (int) get_global_id(0);
  int y = (int) get_global_id(1);
  if(x < w && y < h) {
    float4 n = inImg[y*w+x];
    n = (n+1)/2;
    n.w = 1.0;
    write_imagef(outImg, (int2)(x, y), n);
  }
}

// Converters ***********************

float3 uv2vec(float2 uv)
{
  float lon = uv.x * 2.0f * PI;
  float lat = (uv.y - 0.5f) * PI;
  float3 vec = (float3) (sin(lon)*cos(lat),sin(lat), -1.0*cos(lon)*cos(lat));
  return normalize(vec);
}

float2 vec2uv(float3 vec)
{
  float u = atan2pi(vec.x, -vec.z);
  float v = asinpi(vec.y);
  if(u < 0.0f)
    return (float2) (0.5*u, 0.5+v);
  else
    return (float2) (1.0f+0.5*u, 0.5+v);
}

float4 srgb2linear(float4 srgbIn)
{
  float3 bLess  = step((float3) 0.04045,srgbIn.xyz);
  float3 linOut = mix( srgbIn.xyz/12.92f, pow((srgbIn.xyz+0.055f)/1.055f,2.4f), bLess );
  return (float4)(linOut,srgbIn.w);
}

// Samplers ********************************************************
float2 hammersley(uint i, uint n)
{
  float ri = revbits(i) * 2.3283064365386963e-10f;
  return (float2) ((float) i / (float) n, ri);
}

float3 sample_hemisphere3d(float2 uv, float3 n, float r2)
{
  float phi = uv.x * 2.0 * PI;
  float cosTheta;
  float a = r2*r2;
  if(r2 >= 1.0f) {
      cosTheta = sqrt(1.0f - uv.y);
  } else {
      cosTheta = sqrt((1.0f - uv.y) / (1.0f + (a - 1.0f)*uv.y));
  }
  float sinTheta = clamp(sqrt(1.0f - cosTheta * cosTheta), 0.0f, 1.0f);
  float3 vec = (float3) (cos(phi) * sinTheta, sin(phi) * sinTheta, cosTheta);

  float3 up = fabs(n.y) < 0.999f ? (float3) (0.0f,1.0f,0.0) : (float3) (0.0f,0.0f,1.0f);
  float3 x = normalize(cross(up, n));
  float3 y = normalize(cross(n, x));
  float3 res = x*vec.x + y*vec.y + n*vec.z;
  return normalize(res);
}

float3 sample_sphere3d(float2 uv)
{
  float phi = uv.y * 2.0 * PI;
  float cosTheta = 1.0f - uv.x;
  float sinTheta = sqrt(1.0f - cosTheta * cosTheta);
  return (float3) (cos(phi) * sinTheta, sin(phi) * sinTheta, cosTheta);
}

uint revbits(uint bits)
{
  bits = (bits << 16u) | (bits >> 16u);
  bits = ((bits & 0x55555555u) << 1u) | ((bits & 0xAAAAAAAAu) >> 1u);
  bits = ((bits & 0x33333333u) << 2u) | ((bits & 0xCCCCCCCCu) >> 2u);
  bits = ((bits & 0x0F0F0F0Fu) << 4u) | ((bits & 0xF0F0F0F0u) >> 4u);
  bits = ((bits & 0x00FF00FFu) << 8u) | ((bits & 0xFF00FF00u) >> 8u);
  return bits;
}


// http://graphicrants.blogspot.se/2013/08/specular-brdf-reference.html
float G_schlick(float NoV, float r2)
{
    float k = r2 / 2.0f;
    return NoV / (NoV * (1.0f - k) + k);
}

// Random number generator ****************************************************
// maximally equidistributed combined Tausworthe generator

uint tausworthe(const uint s, const uint a, const uint b, const uint c,	const uint d)
{
  return ((s&c)<<d) ^ (((s << a) ^ s) >> b);
}

uint lcg(const uint x) { return x * 69069; }

uint validseed(const uint x, const uint m) {
  return (x < m) ? (x + m) : x;
}

void initrandomgenerator(uint seed, Seed *s) {
  // Avoid 0 value
  seed = (seed == 0) ? (seed + 0xffffffu) : seed;

  s->s1 = validseed(lcg(seed), 1);
  s->s2 = validseed(lcg(s->s1), 7);
  s->s3 = validseed(lcg(s->s2), 15);
}

unsigned long rndUint(Seed *s) {
  s->s1 = tausworthe(s->s1, 13, 19, 4294967294UL, 12);
  s->s2 = tausworthe(s->s2, 2, 25, 4294967288UL, 4);
  s->s3 = tausworthe(s->s3, 3, 11, 4294967280UL, 17);

  return ((s->s1) ^ (s->s2) ^ (s->s3));
}

float rndFloat(Seed *s) {
  return (rndUint(s) & FLOATMASK) * (1.f / (FLOATMASK + 1UL));
}
