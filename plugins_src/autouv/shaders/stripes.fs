//
//  stripes.fs --
//
//     Diagonal stripes shader based from GLSL SandBox: http://glsl.heroku.com/e#17822.2
//
//  Copyright (c) 2015 Micheus
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: stripes.fs,v 1.0 2015/10/26 21:40:0 micheus Exp $
//

uniform vec4  color1;
uniform vec4  color2;
uniform float angle;
uniform float thickness;
uniform float frequency;
uniform vec2 auv_texsz;

#define MAX_FREQUENCE 100.0
#define pi 3.1415926535

vec2 getRotatedCoords( vec2 coords, vec2 rotationAnchor, float rotation ){
	
	float Angle = atan(coords.y - rotationAnchor.y, coords.x - rotationAnchor.x); // get angle between current uv coord and center
	float newAngle = Angle + rotation; // offset original angle by certain rotation
	
	float len = length(coords - rotationAnchor); // radius
	vec2 rotatedCoords = vec2( len * sin(newAngle), len * cos(newAngle) ); // new rotated coords
	
	return rotatedCoords;	
}

void main( void ) {
	vec4 color;
	float frequency1;
	vec2 position = ( gl_FragCoord.xy / auv_texsz.xy );
	vec2 center = vec2( 0.5, 0.5 );
	
	vec2 rotatedCoords = getRotatedCoords( position, center, ((180.0-angle) * pi/180.0));
	float offset = 0.5;
	if (frequency > MAX_FREQUENCE) {
		frequency1 = 100.0;
	} else if (frequency < 0.0) {
		frequency1 = 0.0;
	} else {
		frequency1 = frequency;
	}

	float aa = frequency1 / MAX_FREQUENCE * 0.00125;
	float sine = float(abs( sin( ( rotatedCoords + offset ) * frequency1 * 10.0) ));
	sine = step( sine, 0.1 +(0.9 *thickness/(100.0 + 0.00001)));
	if (sine > (0.5 +aa)) {
            color = color1;
	} else if (sine < 0.5 -aa) {
            color = color2;
	} else {
            // that will produce a AA effect
            color = color1*(1.0-sine) + color2*sine;
	}
	gl_FragColor = color;
}
