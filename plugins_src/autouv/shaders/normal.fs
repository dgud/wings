//
//  normal.fs --
//
//     Simple object normal shader
//
//  Copyright (c) 2009 Dan Gudmundsson
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//

varying vec3 w3d_normal;

void main(void)
{
   vec3 color; 
   // Scale normals range from: -1,1 to 0,1
   color = (normalize(w3d_normal)+1.0)*0.5;
   gl_FragColor = vec4(color, 1.0);
}

