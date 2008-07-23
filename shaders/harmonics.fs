// $Id: harmonics.vs 124 2007-07-30 21:16:59Z antoneos $
//
// Fragment shader for spherical harmonics lighting
// Author: Randi Rost
// Copyright (C) 2005 3Dlabs, Inc.
// See 3Dlabs-License.txt for license information

varying vec3  DiffuseColor;

void main(void)
{
    gl_FragColor = vec4(DiffuseColor, 1.0);
}

