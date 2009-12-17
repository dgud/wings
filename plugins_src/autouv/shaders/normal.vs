// 
// normal.vs
//
//      Pass through vertex shader.
//
// Copyright (c) 2009 Dan Gudmundsson
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
 
varying vec3 w3d_normal;

uniform vec2 auv_texsz;

void main(void)
{
    w3d_normal = gl_Normal.xyz;

    vec4 Position = gl_Vertex;
    gl_Position   = (gl_ModelViewProjectionMatrix * Position);
}
