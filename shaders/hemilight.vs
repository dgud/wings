// $Id:$
//
// Vertex shader for hemispherical lighting
//
// Author: Randi Rost
//
// Copyright (C) 2005 3Dlabs, Inc.
//
// See 3Dlabs-License.txt for license information
//


/*
uniform vec3 LightPosition;
uniform vec3 SkyColor;
uniform vec3 GroundColor;
*/

vec3 LightPosition = vec3(3.0, 10.0, 1.0);
vec3 SkyColor      = vec3(0.95, 0.95, 0.90);
vec3 GroundColor   = vec3(0.026, 0.024, 0.021);

varying vec3  DiffuseColor;  // GLSLdemo requires vertex/fragment shader pair

void main(void)
{
    vec3 ecPosition = vec3(gl_ModelViewMatrix * gl_Vertex);
    vec3 tnorm      = normalize(gl_NormalMatrix * gl_Normal);
    vec3 lightVec   = normalize(LightPosition - ecPosition);
    float costheta  = dot(tnorm, lightVec);
    float a         = 0.5 + 0.5 * costheta;

	vec4 color		= gl_FrontMaterial.diffuse * gl_Color;
	DiffuseColor	= color.rgb * mix(GroundColor, SkyColor, a);

    gl_Position     = ftransform();
}
