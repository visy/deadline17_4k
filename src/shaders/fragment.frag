#version 130
uniform int m;
out vec4 o;
float t = m/float(44100);
int NUMBER_OF_MARCH_STEPS=350;
float EPSILON=.1;
float DISTANCE_BIAS=.4;
float beat, beat1, beat2, beat3, beatsin;

float fly = 1.;
void pR(inout vec2 p, float a) {
    p = cos(a)*p + sin(a)*vec2(p.y, -p.x);
}
vec3 hit;

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
    re += cellTile2(pos*.25) * cellTile2(pos*1.1) * 3. + cellTile2(pos*0.2) * cellTile2(pos*4.4) * .5;
    return re;
}


float sp(vec3 opos, vec3 pos) {
    return (length(cos(opos*.2)) - (1.1+cos(pos.z*.01+cos(pos.x*1.1)*.1)*.2))*3.91;
}

void r(inout vec3 pos) {
        pR(pos.yx,cos(pos.z*.1+pos.z*.1)+t);
        pR(pos.xy,0.2*sin(pos.z*.2*t*.1));
    
}

void r2(inout vec3 pos) {
    pR(pos.yx,cos(pos.z*cos(t*.002+sin(pos.z*.001+abs(pos.y*.0001)))*.5)+t*.05);
}

void r3(inout vec3 pos) {
   pR(pos.xy,sin(pos.z*cos(t*.002+sin(pos.z*.001+(pos.z*.01)))*.5)+t*.5);
}

void rota(inout vec3 pos) {
    if (fly == 1.) r(pos);
    if (fly == 0.) r2(pos);
    if (fly == 2.) r3(pos);
}

float scene2(vec3 pos)
{
    vec3 translate = vec3(-0.5*cos(pos.z*.01), -.2*sin(.005*pos.z*cos(pos.z*4.5+pos.z*.5+pos.z*5.)*.1), 0.);
    if (fly != 1.) rota(pos);
    hit = pos;
    return sp(pos - translate,pos);
}

float sceneb(vec3 pos) {
    rota(pos);
    float of = .3*sin(pos.z*5.5);
    float R1= sp(pos - vec3(of, -of, cos(pos.z)*.5),pos);
    return min(R1,scene2(pos))-bump(hit+cos(pos.z*.3)*3.)*1.5;
}

float reslast;
float im = 0.;
vec2 raymarch(vec3 position, vec3 direction, int steps)
{
    float total_distance = .005;
    float acc = 0.;
    for(int i = 0 ; i < steps ; ++i)
    {
        vec3 pos = position + direction * total_distance;
        float result = sceneb(pos);
        reslast = result;
        result = result * (1.-2.*im);
        acc+=cos(result*1.)*.05;

        if(result + 1e9 * im < EPSILON)
            return vec2(total_distance, acc);
            
        total_distance += result * DISTANCE_BIAS;
        
        
    }
    return vec2(max(1.0,total_distance)+(1.-im)*700., acc);
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
float fog = 0.5;
vec3 surfColorResul(vec3 pos, vec3 rd, vec3 nrml, vec3 ref, vec2 result, vec3 materialColor, vec3 light_dir) {
    
    float dom = smoothstep( -.1, .9, ref.y);
    float spe = pow(clamp( dot( ref, light_dir ), 0., 1. ),12.0);

    float diffuse = orenNayarDiffuse(light_dir,rd,nrml,.3*fly,.6*fly)-result.y*.05;
    
    vec3 diffuseLit = materialColor * (diffuse * vec3(1.) + vec3(1.));
    vec3 outColor = diffuseLit*fog+dom*.2+spe*.6;
    return mix(vec3(diffuse),outColor,fader)*fader2;
}


void main()
{
    // pixel coordinates
    vec2 uv = (-vec2(1280.,720.) + 2.*(gl_FragCoord.xy))/720.;
    if( -abs(uv.y)+0.8>0.0){
    t+=0.25;
    float t2 = t;
    float t3 = t-28.;
    beat = 132.0/60.0 * t;
    beat1 = mod(beat,1.);
    beat2 = mod(beat,2.);
    beat3 = mod(beat*8.,1.);
    beatsin = sin(beat1*3.141591*2.);
    if (t < 29.25) fly = 0.;
    if (t > 81.) fader=1.-(t-81.)*0.335;
    if (t > 84.) fly = 2.;
    if (fly >= 1.) t-=25.;

    float cz = t*9.9;
    if (fly == 2.) {
		t-=5.;
        cz=1000.-((t-54.)*0.5+t*0.15);
        fader=(t-54.)*0.04;
    }
    
    if (fly==1. && t > 15. && t < 20.) cz += hex(uv*10.)*sin((t-30.)*1.9)*(1.+(t-15.)*0.1);
    


    if (t > 90.) { fader2-=(t-90.)*.1;}
    
    uv*=fader;
    uv*=fader2;

    fader = clamp(fader,0.,1.);
    float FOV = t*.1;
    if (fly == 0.) { 
        cz = 2779.;
    	NUMBER_OF_MARCH_STEPS=100;
		DISTANCE_BIAS=5.0-t*0.1;
		EPSILON=.15;
		FOV=12.-t*0.41;
		fog = distance(vec2(0.),vec2(uv.x,uv.y))*0.4;
    }
    if (t > 49.) cz -= cos(hex(uv*cos(cz*0.01+uv.x*.1)*8.)*mod(t*.01,1.)+cz)*1.;

    if (fly == 2.) FOV = 5.+(uv.x-uv.y)*0.1;
	
    if (fly>0.) { 
	FOV *= min(max(t - 4.25,0.),1.);
	FOV += min(max(t - 15.,0.),10.) * 8. * max(1.-max(t-21.,0.)*0.3,0.);
	}
	
    vec3 camera_origin = vec3(0., 1., cz);
    
    vec3 forward = normalize(vec3(0.,1.,cz+1.)-camera_origin);
    vec3 right = normalize(vec3(forward.z, 0., -forward.x ));
    vec3 up = normalize(cross(forward,right));

    
    vec3 ro = camera_origin;
    vec3 rd = normalize(forward + FOV*uv.x*right + FOV*uv.y*up);
        
    vec2 result = raymarch(ro, rd, NUMBER_OF_MARCH_STEPS);
        
    if (fly == 1.) fog = pow(1. / (1. + result.x), beat3*beat2*min(max(t - 22.,0.),10.) * 18. * max(1.-max(t-25.,0.)*0.3,0.) * fly +(4.+beat2)*max(1.-min(max(t2-40.,0.0),1.),0.)*(cellTile2(hit*(1.+fly*4.))+cellTile2(hit*0.1))+ 0.17 + min(max(t-30.0,0.0)*0.1,0.4) + min(max(t-30.,0.)*0.1,1.) * ( abs(cos(t+result.x))*result.x*5.*distance(cellTile2(hit),cellTile2(hit*0.1+0.1))))-cellTile2(hit*0.1+0.1);
    else if (fog == 2.) result.x*0.03;
    vec3 materialColor = materialMap(result);
    if (fly == 2.) materialColor = vec3(3.0-result.x*0.05);
    vec3 intersection = ro + rd*result.x;
    vec3 nrml = normal(intersection);
    im++;
    float thick = min(max(t2-30.,0.0),1.)*(-1.+min(raymarch(intersection + reslast * rd * max(32.-t/4.0,7.0) / (1.0 + result.x * 0.01), rd, NUMBER_OF_MARCH_STEPS/22).x+cellTile2(hit*1e7)*3.,7.));
    
    o = vec4(
        surfColorResul(
            ro, 
            rd, 
            nrml,
            reflect( rd, nrml ), 
            result, 
            materialColor, 
            normalize(vec3(sin(result.x*.1),.3,-1.+fly))) / (1.0 + thick/4. ), result.x/128.);
    }

}