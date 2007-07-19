// $Id:$

void main()
{
    float w = abs(gl_FragCoord.w);
    float z = sqrt(sqrt(gl_FragCoord.z * w));
    gl_FragColor = vec4(z, z, z, 1.0);
}
