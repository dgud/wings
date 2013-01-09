// $Id$
//
// Vertex shader for environment mapping with an
// equirectangular 2D texture
//
// Authors: John Kessenich, Randi Rost
//
// Copyright (c) 2002-2006 3Dlabs Inc. Ltd.
//
// See 3Dlabs-License.txt for license information
//

attribute vec4 wings_tangent;

varying vec3  Normal;
varying vec3  EyeDir;
varying float LightIntensity;
varying vec4  tangent;

void main()
{
	gl_Position    = ftransform();
	#ifdef __GLSL_CG_DATA_TYPES // Fix clipping for Nvidia and ATI
	gl_ClipVertex = gl_ModelViewMatrix * gl_Vertex;
	#endif
	Normal		   = normalize(gl_NormalMatrix * gl_Normal);
	vec4 pos	   = gl_ModelViewMatrix * gl_Vertex;
	EyeDir		   = pos.xyz;

	gl_TexCoord[0]	= gl_MultiTexCoord0;
	tangent.xyz = gl_NormalMatrix * wings_tangent.xyz;
	tangent.w = wings_tangent.w;
}
