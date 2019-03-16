// Fragment shader for FPSM shadow pass
#version 120

void main(void)
{
  float depth = gl_FragCoord.z;
  float moment1 = depth;
  float moment2 = depth * depth;
  
  // Adjusting moments using derivative
  float dx = dFdx(depth);
  float dy = dFdy(depth);
  moment2 += 0.25*(dx*dx + dy*dy);
  
  gl_FragColor = vec4(moment1, moment2, 0.0, 0.0);
}
