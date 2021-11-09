//
// Grid rendering
//
// Copyright (c) 2021 Dan Gudmundsson
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//


#version 120

uniform mat4 view;
uniform mat4 proj;

uniform float dist;
uniform vec3 ws_eyepoint;
uniform vec3 ws_origin;
uniform int  along;


varying vec3 pos3d;
varying vec2 pos2d;

void main(void)
{
  // Convert xy screenspace to {-1.0,-1.0,1.0,1.0}
  vec4 p = gl_ModelViewProjectionMatrix * vec4(gl_Vertex.xyz, 1.0);
  pos2d = p.xy;
  // Make a large plane in world space
  float scale;
  switch (along) {
  case 1:
    scale = max((abs(ws_eyepoint.x)+abs(dist))*2, 25.0);
    p = vec4(-ws_origin.x, p.x*scale-ws_origin.y, p.y*scale-ws_origin.z, 1.0);
    pos3d = p.zxy;
    break;
  case 3:
    scale = max((abs(ws_eyepoint.z)+abs(dist))*2, 25.0);
    p = vec4(p.x*scale-ws_origin.x, p.y*scale-ws_origin.y, -ws_origin.z, 1.0);
    pos3d = p.xzy;
    break;
  case 2:
    scale = max((abs(ws_eyepoint.y)+abs(dist))*2, 25.0);
    p = vec4(p.x*scale-ws_origin.x, -ws_origin.y, p.y*scale-ws_origin.z, 1.0);
    pos3d = p.xyz;
    break;

  default:  // Not along any axis, default case
    scale = max((abs(ws_eyepoint.y)+abs(dist))*2, 25.0);
    p = vec4(p.x*scale-ws_origin.x, -0.000, p.y*scale-ws_origin.z, 1.0);
    pos3d = p.xyz;
  }

  // Translate plane to 3d space according to camera and proj
  gl_Position = proj * view * p;
}
