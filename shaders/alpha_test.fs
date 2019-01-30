// $Id$
//
// Fragment shader for alpha test to depth
// Hack to handle alpha-masks, near opaque values get draw as usual
//
// Author: Dan Gudmundsson
//
#include "lib_base.glsl"
#include "lib_material.glsl"

void main(void)
{
  vec4 basecolor = get_basecolor();
  gl_FragColor = vec4(gl_FragCoord.z, basecolor.a, 0.0, 1.0);
  if (basecolor.a < 0.8) {
    discard;
  }
}
