// $Id$
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
uniform vec3 ws_lightpos;

void main(void)
{
    vec4 baseColor = get_basecolor();
    vec3 n = get_normal();
    vec3 v = normalize(ws_eyepoint-ws_position);  // point to camera
    vec3 l = normalize(ws_lightpos-ws_position);  // point to ligth
    PBRInfo pbr = calc_views(n, v, vec3(0.0));
    pbr = calc_material(pbr);

    // Calculate the shading terms for the microfacet specular shading model
    vec3  F = specularReflection(pbr);
    float G = geometricOcclusion(pbr);
    float D = microfacetDistribution(pbr);

    // Calculation of analytical lighting contribution
    vec3 diffuseContrib = (1.0 - max(max(F.r, F.g), F.b)) * diffuse(pbr);
    vec3 specContrib = F * G * D / (4.0 * pbr.NdotL * pbr.NdotV);
    // Obtain final intensity as reflectance (BRDF) scaled by the energy of the light (cosine law)
    vec3 frag_color = pbr.NdotL * (diffuseContrib + specContrib);
    frag_color += 0.6*background_ligthting(pbr, n, normalize(reflect(v, n)));
    frag_color = mix(frag_color, frag_color * pbr.occlusion, 0.7);
    frag_color += get_emission();
    gl_FragColor = vec4(pow(frag_color, vec3(1.0/2.2)), baseColor.a); // Should be 2.2
}


