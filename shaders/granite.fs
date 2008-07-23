// $Id$
//
// Fragment shader for producing a granite effect
//
// Author: Randi Rost
//
// Copyright (c) 2002-2006 3Dlabs Inc. Ltd.
//
// See 3Dlabs-License.txt for license information
//

varying float LightIntensity;
varying vec3  MCposition;

uniform sampler3D Noise;
uniform float NoiseScale;

void main(void)
{
    vec4  noisevec  = texture3D(Noise, NoiseScale * MCposition);
    float intensity = min(1.0, noisevec[3] * 18.0);
    vec3  color     = vec3 (intensity * LightIntensity);

    color = clamp(color, 0.0, 1.0); // clamp not needed for book shader

    gl_FragColor    = vec4 (color, 1.0);
}
