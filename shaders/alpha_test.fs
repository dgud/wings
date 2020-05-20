// $Id$
//
// Fragment shader for alpha test to depth
// Hack to handle alpha-masks, near opaque values get draw as usual
//
// Author: Dan Gudmundsson
//
#include "lib_base.glsl"
#include "lib_material.glsl"

uniform float alpha;

void main(void)
{
  vec4 basecolor = get_basecolor();
  if (basecolor.a < alpha) { discard; }
}
