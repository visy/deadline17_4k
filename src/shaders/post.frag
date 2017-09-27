#version 130
uniform sampler2D o;
out vec4 i;
float hash(float c){return fract(sin(dot(c, 12.9898)) * 43758.5453);}
void main(){
	vec4 j = vec4(0.);
	float s1;
	vec2 uv = gl_FragCoord.xy/vec2(1280,720);
    float blr = pow(max(texture(o,uv).w*texture(o,uv).w*0.0002,0.0),2.0);
	for(int t = 0; t < 11; t++){
        float s2 = s1;
		s1 = hash(float(1-t)+dot(uv,uv));
		vec2 f = 0.01*(-1.0+2.0*vec2(s1,s2));
		j += texture(o, uv+max(blr/2.-.004,0.)*vec2(s1-.5,s2-.5));
	}
	j /= vec4(11.);
    i = j - blr*4.;
}