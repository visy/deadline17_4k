#version 130
uniform sampler2D o;
out vec4 i;
void main() {
	vec4 j = vec4(0.);
	float s1;
	vec2 uv = gl_FragCoord.xy/vec2(1280.,720.);
    float right = texture(o,uv+0.001).w*128.;
    float left = texture(o,uv-0.001).w*128.;
    float blr = pow(max(right*left*0.0002,0.0),2.0);
	for(int t = 0; t < 99; t++){
        float s2 = s1;
		s1 = fract(sin(dot(float(1-t)+dot(uv,uv+right), 12.9898)) * 43758.5453);
		vec2 f = 0.01*(-1.0+2.0*vec2(s1,s2));
		j += texture(o, uv+(blr+0.001)/2.*vec2(s1-.5,s2-.5));
	}
	j /= vec4(66.);
    j -= s1/16.;
	i = (j - blr);

}