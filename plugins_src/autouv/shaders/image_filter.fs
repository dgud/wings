//
//  image_filter.fs --
//
//     Image filters 
//
//  Copyright (c) 2006 Dan Gudmundsson
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: image_filter.fs,v 1.3 2006/01/27 15:17:56 dgud Exp $
//
// Grabbed from tutorial By Jérôme Guinot jegx [at] ozone3d [dot] net
//

#define KERNEL_SIZE 9

varying vec2 w3d_uv;
uniform sampler2D auv_bg;
uniform float kernel[KERNEL_SIZE];
uniform vec2 auv_texsz;

void main(void)
{
    float alpha = 1.0;
    float step_w = 1.0/(auv_texsz.x);
    float step_h = 1.0/(auv_texsz.y);

    vec4 sum = vec4(0.0), tmp;
    float scale = kernel[0]+kernel[1]+kernel[2]+kernel[4]+kernel[5]+
	kernel[6]+kernel[7]+kernel[8];
    scale = max(1.0,scale);
    
    float orig_x  = w3d_uv.x;
    float orig_y  = w3d_uv.y;
    float orig_up = orig_y + step_h;
    float orig_dw = orig_y - step_h;
    float orig_lt = orig_x - step_w;
    float orig_rt = orig_x + step_w;

    tmp = texture2D(auv_bg, vec2(orig_lt,orig_up));
    sum += tmp * kernel[0];
    tmp = texture2D(auv_bg, vec2(orig_x, orig_up));
    sum += tmp * kernel[1];
    tmp = texture2D(auv_bg, vec2(orig_rt,orig_up));
    sum += tmp * kernel[2];
   
    tmp = texture2D(auv_bg, vec2(orig_lt,orig_y));
    sum += tmp * kernel[3];
    tmp = texture2D(auv_bg, vec2(orig_x, orig_y));
    sum += tmp * kernel[4]; 
    alpha = tmp.w;
    tmp = texture2D(auv_bg, vec2(orig_rt,orig_y));
    sum += tmp * kernel[5];
    
    tmp = texture2D(auv_bg, vec2(orig_lt,orig_dw));
    sum += tmp * kernel[6]; 			   
    tmp = texture2D(auv_bg, vec2(orig_x, orig_dw));  
    sum += tmp * kernel[7]; 			   
    tmp = texture2D(auv_bg, vec2(orig_rt,orig_dw));
    sum += tmp * kernel[8];
    
    sum = sum/scale;  
    sum.w = alpha;
    //    vec2 where   = vec2(auv_pos2d.x, 0.5+auv_pos2d.y);
    gl_FragColor = sum; // texture2D(auv_bg, where); //sum;
}
