//
//  brick.fs --
//
//     Simple brick shader stolen from GLSL SandBox: http://glslsandbox.com/e#28152.0
//
//  Copyright (c) 2015 Micheus
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: brick.fs,v 1.0 2015/10/16 21:40:0 micheus Exp $
//

uniform vec4  colorBrick;
uniform float scalex;
uniform float scaley;
uniform vec4  colorMortar;
uniform float scaleMortar;
uniform vec2 auv_texsz;

#define width 16.0
#define height 8.0


//simple brick scene
//intended to be scaled with nearest neighbour filtering
//mattdesl - http://devmatt.wordpress.com/

float rand(vec2 co){
    return fract(sin(dot(co, vec2(12.9898,78.233))) * 43758.5453);
}

void main( void ) {
	float bw = width*scalex/10.0;
	float bh = height*scaley/10.0;
	float lw = 1.0*scaleMortar/10.0;
	float x = gl_FragCoord.x;
	float y = gl_FragCoord.y;
	float bx = mod(y, bh*2.0) < bh ? x + bw/2.0 : x;
	vec3 color3 = colorBrick.rgb;
	float blend = colorBrick.a;

	float xbw = mod(bx, bw);
	float ybh = mod(y, bh);
	float TW = auv_texsz.x/bw+1.0;
	float TH = auv_texsz.y/bh+1.0;

	vec3 normals = vec3(0.0, 0.0, 1.0);

	//bit of faux randomization
	float xpos = floor(mod(floor(bx/bw), TW));
	float ypos = floor(mod(floor(y/bh), TH));
	vec3 color = vec3(.25 + rand(vec2(xpos, ypos))*.25);

	normals.x = ((xbw/bw)*2.0-1.0)*.25;
	normals.y = ((ybh/bh)*2.0-1.0)*.25;

	//adapted from a software solution.. lots of ifs and shit
	if ( xbw >= bw-2.0)
		normals.x += 0.25;
	else if ( xbw <= 2.0)
		normals.x -= 0.5;
	else if ( ybh >= bh-3.0)
		normals.y += 0.25;
	if ( ybh <= 2.0)
		normals.y -= 0.25;

	color *= color3;

	//mortar
	if ( mod(y+lw, bh) < lw || mod(bx, bw) < lw ) {
		color = vec3(colorMortar.rgb);
		blend = colorMortar.a;
		normals = vec3(0.0, 0.0, 1.0);
	}

	vec3 lightPos = vec3((gl_FragCoord.xy/auv_texsz.xy), 0.2); // vec3(mouse - vec2(0.5, 0.5), 0.5);
	vec3 L = normalize(lightPos);
	vec3 N = normalize(normals);

	float dist = sqrt(dot(lightPos, lightPos));
	vec3 att = vec3(2.3,-16.,37.5);

	float lambert = clamp(dot(N, L), 0.0, 1.0);
	float shadow = 1.0/(att.x + (att.y*dist) + (att.z*dist*dist));
	vec3 finalColor = vec3(0.0);

	finalColor += color ;

	gl_FragColor = vec4(finalColor, blend);
}
