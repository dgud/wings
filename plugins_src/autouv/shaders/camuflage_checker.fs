//
//  camuflage_checker.fs --
//
//     Checker camuflage shader stolen from GLSL SandBox: http://glslsandbox.com/e#23824.0
//
//  Copyright (c) 2015 Micheus
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: camuflage_checker.fs,v 1.0 2015/10/16 21:40:0 micheus Exp $
//

uniform vec4  colorPixels;
uniform float scale;
uniform vec2  auv_texsz;

void main( void ) {
    vec2 uv = ( gl_FragCoord.xy / auv_texsz.y );
    vec3 color = vec3(fract(sin(dot(floor(uv.xy*32.0+4.0),vec2(5.364,6.357)))*357.536));

    gl_FragColor = vec4(color*colorPixels.rgb, 1.0-colorPixels.a);
}
