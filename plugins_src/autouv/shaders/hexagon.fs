//
//  hexagon.fs --
//
//     Hexagon shader adapted from GLSL SandBox: http://glslsandbox.com/e#23229.0
//
//  Copyright (c) 2015 Micheus
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: hexagon.fs,v 1.0 2015/10/16 21:40:0 micheus Exp $
//


uniform float typeHex;
uniform vec4  colorHex;
uniform vec4  colorThick;
uniform float scaleHex;
uniform float scaleThick;
uniform float shrink;

#define hexaGridSize 2.0
#define hexaGridThick 0.10

vec4 kcolor;
vec4 kbkcolor;

// return 1.0 for hexagonal grid
float hexagonalGrid (in vec2 position,
	                 in float gridSize,
	                 in float gridThickness)
{
  vec2 p = position / gridSize;
  p.x *= 0.57735 * (2.0+shrink/10.0);
  p.y += mod(floor(p.x), 2.0)*0.5;
  p = abs((mod(p, 1.0) - 0.5));
  float d = abs(max(p.x*1.5 + p.y, p.y*2.0) - 1.0);
  return d;
}

vec4 hexagon0(in float d,
	          in float gridThickness)
{
  float k = abs(d);
  float tk = ((1.0/scaleHex)+(0.1 *min(gridThickness,99.8)));
  if (k >= tk) {
    return colorHex;
  } else {
    return colorThick;
  }
}

vec4 hexagon1(in float d)
{
  float k = d;
  float ik = (1.0-k);
  kcolor = vec4(colorHex.r*k,colorHex.g*k,colorHex.b*k,1.0-colorHex.a);
  kbkcolor = vec4(colorThick.r*ik,colorThick.g*ik,colorThick.b*ik,1.0-colorThick.a);
  return vec4(kbkcolor+kcolor);
}

vec4 hexagon2(in float d)
{
  float k = min(d,1.0);
  float ik = (1.0-k);
  kcolor = vec4(colorHex.r*k,colorHex.g*k,colorHex.b*k,1.0-colorHex.a);
  kbkcolor = vec4(colorThick.r*ik,colorThick.g*ik,colorThick.b*ik,1.0-colorThick.a);
  if (k >= 1.0) {
    return kcolor;
  } else if (ik >= 0.90) {
    return kbkcolor;
  } else {
    return vec4(kbkcolor+kcolor);
  }
}

vec4 hexagon3(in float d,
	          in float gridThickness)
{
  float k = min(abs(d*(3.0/sqrt(gridThickness))),1.0);
  float ik = (1.0-k);
  kcolor = vec4(colorHex.r*k,colorHex.g*k,colorHex.b*k,1.0-colorHex.a);
  kbkcolor = vec4(colorThick.r*ik,colorThick.g*ik,colorThick.b*ik,1.0-colorThick.a);
  if (k >= 0.70) {
    return kcolor;
  } else if (ik <= 1.0) {
    return kbkcolor;
  } else {
    return vec4(kbkcolor+kcolor);
  }
}

void main(void)
{
  vec4 color;
  float d;

  d = hexagonalGrid(gl_FragCoord.xy,
                    hexaGridSize+(1.*scaleHex),
                    hexaGridThick+(0.1*scaleThick));
  if (typeHex == 1.0) {
    color = hexagon1(d);
  } else if (typeHex == 2.0) {
    color = hexagon2(d);
  } else if (typeHex == 3.0) {
    color = hexagon3(d,hexaGridThick+(0.1*scaleThick));
  } else {
    color = hexagon0(d,hexaGridThick+(0.1*scaleThick));
  }
  gl_FragColor = color;
}

