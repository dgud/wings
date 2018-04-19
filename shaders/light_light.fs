//
// Fragment shader for rendering lights
//
// See: Real-Time Polygonal-Light Shading with Linearly Transformed Cosines
//      https://eheitzresearch.wordpress.com/415-2/
//
// Author: Dan Gudmundsson (with code from above)
//

#version 120

uniform vec4 light_color;

void main(void)
{
  float backface = (float(gl_FrontFacing)+1.0)/2.0;
  gl_FragColor = vec4(backface*light_color.rgb, light_color.a);
}
