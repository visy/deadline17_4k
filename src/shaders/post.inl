#pragma data_seg(".shader")
const char* post =
 "#version 130\n"
 "uniform sampler2D m;"
 "out vec4 v;"
 "void main()"
 "{"
   "vec4 f=vec4(0);"
   "float t;"
   "vec2 s=gl_FragCoord.xy/vec2(1280,720);"
   "float d=texture(m,s+.001).w*128,w=texture(m,s-.001).w*128,i=pow(max(d*w*.0002,0),2);"
   "for(int o=0;o<55;o++)"
     "{"
       "float e=t;"
       "t=fract(sin(dot(float(1-o)+dot(s,s+d),12.9898))*43758.5);"
       "f+=texture(m,s+(i+.002)/8*vec2(t-.5,e-.5));"
     "}"
   "f/=vec4(44);"
   "f-=t/16;"
   "v=f-i;"
 "}";
