#version 130
uniform int m;
out vec4 o;
float t = m/float(44100);
float NEAR_CLIPPING_PLANE=.005;
float FAR_CLIPPING_PLANE=1000.;
int NUMBER_OF_MARCH_STEPS=500;
float EPSILON=.1;
float DISTANCE_BIAS=.2;

float fly = 1.;

void pR(inout vec2 p, float a) {
	p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}

void r(inout vec3 pos) {
    if (pos.z < FAR_CLIPPING_PLANE) {
        pR(pos.yx,cos(pos.z*.1+pos.z*.1)+t);
        pR(pos.xy,0.2*sin(pos.z*.2*t*.1));
    }
    else pR(pos.yx,cos(pos.z*.1)+t);
    
}

void r2(inout vec3 pos) {
   if (pos.z < 2800.+cos(pos.z*2.1+t*5.)*10.) pR(pos.yx,cos(pos.z*cos(t*.002+sin(pos.z*.001+abs(pos.y*.0001)))*.5)+t*.05);
}

void r3(inout vec3 pos) {
   pR(pos.xy,sin(pos.z*cos(t*.002+sin(pos.z*.001+(pos.z*.01)))*.5)+t*.5);
}

float celli(vec3 p){ p = fract(p)-.5; return dot(p, p); }

float hex(vec2 p) {
    p.x *= 0.57735*2.;
	p.y += mod(floor(p.x), 2.)*.5;
	p = abs((mod(p, 1.) - .5));
	return abs(max(p.x*1.5 + p.y, p.y*2.) - 1.);
}

float cellTile2(vec3 p){
    vec4 d; 
    d.x = celli(p - vec3(.81, .62, .53));
    p.xy = vec2(p.y-p.x, p.y + p.x)+hex(p.xy*0.2);
    d.y = celli(p - vec3(.39, .2, .11));
    p.yz = vec2(p.z-p.y, p.z + p.y)+hex(p.yz*0.2);
    d.z = celli(p - vec3(.62, .24, .06));
    p.xz = vec2(p.z-p.x, p.z + p.x)+hex(p.xz*0.2);
    d.w = celli(p - vec3(.2, .82, .64));
    d.xy = min(d.xz, d.yw);
    return min(d.x, d.y)*.5; 
}
float bump(vec3 pos) {
    float re = 0.;
	re += cellTile2(pos*.25) * cellTile2(pos*1.1) * 3. + cellTile2(pos*1.2) * cellTile2(pos*4.4) * .5;
    return re;
}
vec3 hit;

float sp(vec3 opos, vec3 pos) {
	//return sdSphere(cos(opos*.2),1.1+cos(pos.z*.01+cos(pos.x*1.1)*.1)*.2)*3.91;
    return (length(cos(opos*.2)) - (1.1+cos(pos.z*.01+cos(pos.x*1.1)*.1)*.2))*3.91;
}

void rota(inout vec3 pos) {
    if (fly == 1.) r(pos);
    if (fly == 0.) r2(pos);
    if (fly == 2.) r3(pos);
}

float scene1(vec3 pos)
{
	rota(pos);
    float of = .3*sin(pos.z*5.5);
	return sp(pos - vec3(of, -of, cos(pos.z)*.5),pos);
}

float scene2(vec3 pos)
{
    vec3 translate = vec3(-0.5*cos(pos.z*.01), -.2*sin(.005*pos.z*cos(pos.z*4.5+pos.z*.5+pos.z*5.)*.1), 0.);
	rota(pos);
    hit = pos;
	return sp(pos - translate,pos);
}

float scene(vec3 pos) {
	return min(scene1(pos),scene2(pos));
}

float sceneb(vec3 pos) {
	return min(scene1(pos),scene2(pos))-bump(hit+cos(pos.z*.3)*3.)*1.5;
}

vec2 raymarch_in(vec3 position, vec3 direction)
{
    float total_distance = NEAR_CLIPPING_PLANE;
    float acc = 0.;
    for(int i = 0 ; i < NUMBER_OF_MARCH_STEPS/8 ; ++i)
    {
        vec3 pos = position + direction * total_distance;
        float result = -scene(pos);
        acc+=bump(pos*12.);
        total_distance += result * DISTANCE_BIAS;
        
    }
    return vec2(max(1.0,total_distance), acc);
}
float reslast;
vec2 raymarch(vec3 position, vec3 direction, int steps)
{
    float total_distance = NEAR_CLIPPING_PLANE;
    float acc = 0.;
    for(int i = 0 ; i < NUMBER_OF_MARCH_STEPS ; ++i)
    {
        vec3 pos = position + direction * total_distance;
        float result = scene(pos);
        reslast = result;
        acc+=cos(result*1.)*.05;

        if(result < EPSILON)
        {
            return vec2(total_distance, acc);
        }
        
        total_distance += result * DISTANCE_BIAS;
        
        
    }
    return vec2(FAR_CLIPPING_PLANE, acc);
}
vec3 normal( vec3 pos )
{
    vec3 eps = vec3(EPSILON,0.,0.);
	vec3 nor = vec3(
	    sceneb(pos+eps.xyy) - sceneb(pos-eps.xyy),
	    sceneb(pos+eps.yxy) - sceneb(pos-eps.yxy),
	    sceneb(pos+eps.yyx) - sceneb(pos-eps.yyx) );
	return normalize(nor);
}



float orenNayarDiffuse(
  vec3 lightDirection,
  vec3 viewDirection,
  vec3 surfaceNormal,
  float roughness,
  float albedo) {
  
  float LdotV = dot(lightDirection, viewDirection);
  float NdotL = dot(lightDirection, surfaceNormal);
  float NdotV = dot(surfaceNormal, viewDirection);

  float s = LdotV - NdotL * NdotV;

  float sigma2 = roughness * roughness;
  return albedo * max(0., NdotL) * (1. + sigma2 * (albedo / (sigma2 + .13) + .5 / (sigma2 + .33)) + .45 * sigma2 / (sigma2 + .09) * s /  mix(1., max(NdotL, NdotV), step(0., s))) / 3.14159;
}
vec3 materialMap(vec2 result) {
	vec3 materialColor = vec3(1.3-result.x*.01*.5,.9-cos(result.x*.1)*.5,1.*.5);
	materialColor -= vec3(.4,4.7,8.0)*(bump(hit)+bump(hit*.2*vec3(1.,1.,4.))*1.5);
    return materialColor;
}
float fader = 1.0;
float fader2 = 1.0;
vec3 surfColorResul(vec3 pos, vec3 rd, vec3 nrml, vec3 ref, vec2 result, vec3 materialColor, vec3 light_dir) {
    
    float dom = smoothstep( -.1, .9, ref.y);
    float spe = pow(clamp( dot( ref, light_dir ), 0., 1. ),6.0);

    float diffuse = orenNayarDiffuse(light_dir,rd,nrml,.3*fly,.6*fly)-result.y*.05;
    
    vec3 light_color = vec3(1.);
    vec3 ambient_color = light_color;
    vec3 diffuseLit = materialColor * (diffuse * light_color + ambient_color);
    vec3 outColor = diffuseLit*(pow(1. / (1. + result.x), 0.23 + min(max(t-60.0,0.0)*0.1,1.0) + min(max(t-90.0,0.0)*0.1,1.0) ))+dom*.2+spe*.6;
    return mix(vec3(diffuse),outColor,fader)*fader2;
}


void main()
{
    vec2 res = vec2(1280.,720.);
    // pixel coordinates
    vec2 uv = (-res + 2.*(gl_FragCoord.xy))/res.y;
    vec3 direction = normalize(vec3(uv, 0.));
    
    if (t < 30.) fly = 0.;
	if (t > 81.) fader=1.-(t-81.)*0.335;
    if (t > 84.) fly = 2.;
    if (fly >= 1.) t-=30.;

    float cz = t*5.9;
    
    if (fly == 0.) { 
        cz = 2779.;
    	NUMBER_OF_MARCH_STEPS=200;
		DISTANCE_BIAS=.6;
    }

    if (fly == 2.) {
       	cz=1000.-((t-54.)*0.5);
    	NUMBER_OF_MARCH_STEPS=300;
		DISTANCE_BIAS=.3;
       	fader=(t-54.)*0.04;
    }

    if (t > 90.) { fader2-=(t-90.)*.1;}
    
    uv*=fader;
    uv*=fader2;

    fader = clamp(fader,0.,1.);
    float FOV = t*.1;
        
    if (fly == 0.) { FOV = 12.-sin(t+length(uv)*3.14159*2.)*(32.0-t)*0.1+t*2.; }
    if (fly == 2.) FOV = 5.+(uv.x-uv.y)*0.1;

    if (t > 25. && t <= 30.) NUMBER_OF_MARCH_STEPS-=int((t-25.)*40.);

	vec3 camera_origin = vec3(0., 1., cz);
    
    vec3 forward = normalize(vec3(0.,1.,cz+1.)-camera_origin);
    vec3 right = normalize(vec3(forward.z, 0., -forward.x ));
    vec3 up = normalize(cross(forward,right));

    
    vec3 ro = camera_origin;
    vec3 rd = normalize(forward + FOV*uv.x*right + FOV*uv.y*up);

    vec2 result = raymarch(ro, rd, NUMBER_OF_MARCH_STEPS);
    
	vec3 materialColor = materialMap(result);
    vec3 intersection = ro + rd*result.x;
    vec3 nrml = normal(intersection);
	
    vec2 result_in = raymarch_in(intersection + reslast * rd * max(32.-t/4.0,7.0) / (1.0 + result.x * 0.01) , rd);
    /*
    vec3 intersection_in = ro_in + rd*result_in.x;
    
    vec3 nrml_in = normal(intersection_in);
    vec3 light_dir_in = nr(vec3(sin(result_in.x*.1),.3,-1.+fly));
    vec3 ref_in = reflect( rd, nrml_in );
    vec3 ro_out = ro_in - nrml * (result.x * 0.3 + EPSILON);
    vec3 rd_out = rd;
    vec2 result_out = raymarch(ro_out, rd_out, NUMBER_OF_MARCH_STEPS / 4);
	vec3 materialColor_out = materialMap(result_out);
    vec3 intersection_out = ro_out + rd_out*result_out.x;
    vec3 nrml_out = normal(intersection_out);
    vec3 ref_out= reflect( rd, nrml_out );
    vec3 light_dir_out = nr(vec3(sin(result_out.x*.1),.3,-1.+fly));
    */
    if (t > 45.) { materialColor=mix(materialColor,vec3(.6,.6,1.0),clamp((t-45.)*0.2,0.,1.8)); }

    vec3 outColor = surfColorResul(ro, rd, nrml, reflect( rd, nrml ), result, materialColor, normalize(vec3(sin(result.x*.1),.3,-1.+fly)));
    //vec3 outColor_out = surfColorResul(ro_out, rd_out, nrml_out, ref_out, result_out, materialColor_out, light_dir_out, fog);
    outColor *= (1.7 / (1.0 + 0.5 * min(result_in.x + cellTile2(hit/0.001) * 7.,7.)  ));
    //outColor += outColor_out * extinction * extinction / 4.0;
	if( -abs(uv.y)+0.8>0.0)
		o = vec4(outColor, result.x);
}
