// $Id:$

varying vec3 normal;
varying vec4 position;

void main()
{
  normal = gl_NormalMatrix * gl_Normal;
  position = gl_ModelViewMatrix * gl_Vertex;
  gl_Position = gl_ModelViewProjectionMatrix * gl_Vertex;
}

