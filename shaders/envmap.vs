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

varying vec3  Normal;
varying vec3  EyeDir;
varying float LightIntensity;

vec3 LightPos = vec3(0.0, 10.0, 0.0);

void main()
{
	gl_Position    = ftransform();
	#ifdef __GLSL_CG_DATA_TYPES // Fix clipping for Nvidia and ATI
	gl_ClipVertex = gl_ModelViewMatrix * gl_Vertex;
	#endif
	Normal		   = normalize(gl_NormalMatrix * gl_Normal);
	vec4 pos	   = gl_ModelViewMatrix * gl_Vertex;
	EyeDir		   = pos.xyz;
	LightIntensity = max(dot(normalize(LightPos - EyeDir), Normal), 0.0);
}
