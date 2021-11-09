//
// Fragment shader for grid
//
// Author: Dan Gudmundsson
//

#version 120

uniform vec4 scale;
uniform vec3 color;

varying vec3 pos3d;
varying vec2 pos2d;

float grid(vec3 fragPos3D, float scale) {
  vec2 coord = fragPos3D.xz * scale; // use the scale variable to set the distance between the lines
  vec2 derivative = fwidth(coord);
  vec2 grid = abs(fract(coord - 0.5) - 0.5) / derivative;
  float line = min(grid.x, grid.y);
  return 1.0 - min(line, 1.0);
}

void main(void)
{
  // fade with radius from center
  float fade = 1.0-min(1.0,length(pos2d));
  // calc and with scale.x & y and multiply with respective alpha scale.z & w
  float alpha = grid(pos3d, scale.x)*scale.z + grid(pos3d, scale.y)*scale.w;
  gl_FragColor = vec4(color, alpha * fade * fade);
}
