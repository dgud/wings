// $Id$

varying vec3 normal;
varying vec4 position;

void main()
{
  vec3 n = normalize (normal);
  vec3 l = normalize (gl_LightSource[0].position.xyz - position.xyz);
  float I = max (dot (n, l), 0.0);
  vec4 c = gl_FrontMaterial.ambient * gl_LightSource[0].ambient;
  if (I > 0.99)
    c += gl_FrontMaterial.specular * gl_LightSource[0].specular;
  else if (I > 0.75)
    c += gl_FrontMaterial.diffuse * gl_LightSource[0].diffuse;
  else if (I > 0.50)
    c += gl_FrontMaterial.diffuse * gl_LightSource[0].diffuse * .83333333333333;
  else if (I > 0.25)
    c += gl_FrontMaterial.diffuse * gl_LightSource[0].diffuse * .66666666666666;
  else
    c += gl_FrontMaterial.diffuse * gl_LightSource[0].diffuse * .16666666666666;
  gl_FragColor = c;
}

