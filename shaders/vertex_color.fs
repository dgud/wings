// $Id: VertexColor.vs 117 2007-07-19 16:44:01Z antoneos $
// Colors based on vertex-normal or face-normals

varying vec3 VertexColor;

void main()
{
	gl_FragColor = vec4(VertexColor, 1.0);
}
