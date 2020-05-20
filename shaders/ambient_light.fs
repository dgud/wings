//
// Fragment shader for camera lighting
//
// Author: Dan Gudmundsson
//

#version 120

#include "lib_base.glsl"
#include "lib_normal.glsl"
#include "lib_envlight.glsl"
#include "lib_material.glsl"

varying vec3 ws_position;
uniform vec3 ws_eyepoint;
uniform vec4 light_diffuse;

void main(void)
{
    vec3 n = get_normal();
    vec3 v = normalize(ws_eyepoint-ws_position);  // point to camera
    PBRInfo pbr = calc_views(n, v, vec3(0.0));
    pbr = calc_material(pbr);
    vec3 frag_color = light_diffuse.rgb * background_ligthting(pbr, n, normalize(reflect(v, n)));
    frag_color += get_emission();
    gl_FragColor = vec4(pow(frag_color, vec3(1.0/2.2)), pbr.opaque); // Should be 2.2
}
