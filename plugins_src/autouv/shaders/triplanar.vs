//
// triplanar.vs
//
//      Pass through vertex shader.
//
// Copyright (c) 2006 Dan Gudmundsson (standard.vs)
//               2021 Micheus (adapting for triplanar)
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: standard.vs,v 1.2 2006/01/27 15:17:56 dgud Exp $
//          triplanar.vs,v 1.0 2021/02/22 13:41:35 micheus Exp $
//

varying vec2 w3d_uv;
varying vec3 w3d_pos;
varying vec3 w3d_normal;   // world space - used by triplanar

uniform vec2 auv_texsz;

void main(void)
{
    // UV coords comes here since we are actually drawing on a texture
    w3d_uv    = gl_Vertex.xy;

    // The vertex positions comes here in world space
    w3d_pos   = gl_MultiTexCoord1.xyz;
    w3d_normal = gl_Normal.xyz;

    vec4 Position = gl_Vertex;
    gl_Position   = (gl_ModelViewProjectionMatrix * Position);
}
