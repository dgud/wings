// $Id:$
//
// Vertex shader for cartoon-style shading
//
// Author: Philip Rideout
//
// Copyright (c) 2005-2006 3Dlabs Inc. Ltd.
//
// See 3Dlabs-License.txt for license information
//

varying vec3 Normal;

void main(void)
{
    Normal = normalize(gl_NormalMatrix * gl_Normal);
    gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
    gl_TexCoord[0] = gl_MultiTexCoord0;
}

