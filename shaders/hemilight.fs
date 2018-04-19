// $Id$
//
// Fragment shader for hemispherical lighting
//
// Author: Dan Gudmundsson
//

#include "lib_base.glsl"
#include "lib_normal.glsl"
#include "lib_material.glsl"

uniform vec3 ws_lightpos;
uniform vec3 SkyColor;
uniform vec3 GroundColor;

varying vec3 ws_position;

void main(void)
{
    vec3  lightVec = normalize(ws_lightpos);
    float costheta = dot(get_normal(), lightVec);
    float a = 0.5 + 0.5 * costheta;
    vec4 basecolor = get_basecolor();
    vec3 DiffuseColor = clamp(basecolor.rgb * mix(GroundColor, SkyColor, a),
                              vec3(0.0), vec3(1.0));
    DiffuseColor *= get_occlusion();
    DiffuseColor += get_emission();
    gl_FragColor = vec4(pow(DiffuseColor, vec3(1.0/2.2)), basecolor.a);
}
