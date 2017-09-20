// FXAA
#version 130
uniform sampler2D o;
out vec4 i;

vec3 FxaaPixelShader( vec4 uv, sampler2D tex, vec2 rcpFrame) {
    
    vec3 rgbNW = textureLod(tex, uv.zw, 0.).xyz;
    vec3 rgbNE = textureLod(tex, uv.zw + vec2(1,0)*rcpFrame.xy, 0.).xyz;
    vec3 rgbSW = textureLod(tex, uv.zw + vec2(0,1)*rcpFrame.xy, 0.).xyz;
    vec3 rgbSE = textureLod(tex, uv.zw + vec2(1,1)*rcpFrame.xy, 0.).xyz;
    vec3 rgbM  = textureLod(tex, uv.xy, 0.).xyz;

    vec3 luma = vec3(.299, .587, .114);
    float lumaNW = dot(rgbNW, luma);
    float lumaNE = dot(rgbNE, luma);
    float lumaSW = dot(rgbSW, luma);
    float lumaSE = dot(rgbSE, luma);
    float lumaM  = dot(rgbM,  luma);

    float lumaMin = min(lumaM, min(min(lumaNW, lumaNE), min(lumaSW, lumaSE)));
    float lumaMax = max(lumaM, max(max(lumaNW, lumaNE), max(lumaSW, lumaSE)));

    vec2 dir;
    dir.x = -((lumaNW + lumaNE) - (lumaSW + lumaSE));
    dir.y =  ((lumaNW + lumaSW) - (lumaNE + lumaSE));

    float dirReduce = max(
        (lumaNW + lumaNE + lumaSW + lumaSE) * (.25 * .0625),
        (1./128.));
    float rcpDirMin = 1./(min(abs(dir.x), abs(dir.y)) + dirReduce);
    
    dir = min(vec2( 16.),
          max(vec2(-16.),
          dir * rcpDirMin)) * rcpFrame.xy;

    vec3 rgbA = (.5) * (
        textureLod(tex, uv.xy + dir * (1./3. - .5), 0.).xyz +
        textureLod(tex, uv.xy + dir * (2./3. - .5), 0.).xyz);
    vec3 rgbB = rgbA * (.5) + (.25) * (
        textureLod(tex, uv.xy + dir * (0./3. - .5), 0.).xyz +
        textureLod(tex, uv.xy + dir * (3./3. - .5), 0.).xyz);
    
    float lumaB = dot(rgbB, luma);
    
    if((lumaB < lumaMin) || (lumaB > lumaMax)) return rgbA;
    
    return rgbB; 
}


void main() {
	vec2 res = vec2(1280.,720.);
    vec2 rcpFrame = 1./res;
  	vec2 uv2 = gl_FragCoord.xy / res;
        
    vec3 col;

   	vec4 uv = vec4( uv2, uv2 - (rcpFrame * (.75)));
	col = FxaaPixelShader( uv, o, 1./res );
        
    i = vec4( col, 1. );
}
