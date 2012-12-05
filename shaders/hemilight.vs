// $Id$
//
// Vertex shader for hemispherical lighting
//
// Author: Randi Rost
//
// Copyright (C) 2005 3Dlabs, Inc.
//
// See 3Dlabs-License.txt for license information
//

in vec4 wings_tangent;

varying vec3 normal;
varying vec3 ecPosition;
varying vec4 color;
varying vec4 tangent;

void main(void)
{
    ecPosition = vec3(gl_ModelViewMatrix * gl_Vertex);
    color	= gl_FrontMaterial.diffuse * gl_Color;
    normal	= gl_NormalMatrix * gl_Normal;
    tangent.xyz = gl_NormalMatrix * wings_tangent.xyz;
    tangent.w = wings_tangent.w;

    gl_TexCoord[0]	= gl_MultiTexCoord0;
#ifdef __GLSL_CG_DATA_TYPES // Fix clipping for Nvidia and ATI
    gl_ClipVertex   = gl_ModelViewMatrix * gl_Vertex;
#endif
    gl_Position 	= ftransform();
}
