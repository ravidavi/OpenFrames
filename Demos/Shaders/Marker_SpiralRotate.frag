// FRAGMENT SHADER
// Draws a rotating double-arm spiral

// Use GLSL 1.20 (OpenGL 2.1)
#version 120

uniform float osg_FrameTime;
#define FOUR_PI_INV 0.07957747154
#define PI 3.1415926536
#define TWO_PI 6.2831853072
#define R_EPS 0.1

void main(void)
{
  // Move origin to point center
  vec2 v = gl_PointCoord - vec2(0.5);

  // Compute polar coordinates for first spiral arm
  float dist = length(v);
  float theta1 = mod(atan(v.y, v.x) + TWO_PI*osg_FrameTime, TWO_PI);
  float r1 = theta1*FOUR_PI_INV*(1.0 - R_EPS); // Shrink arm to account for thickness
  
  // Reverse polar coordinates for second spiral arm
  float theta2 = mod(theta1 + PI, TWO_PI);
  float r2 = theta2*FOUR_PI_INV*(1.0 - R_EPS);

  // Thickness parameters for first spiral arm
  float a_r1 = smoothstep(r1 - R_EPS, r1, dist);
  float b_r1 = smoothstep(r1, r1 + R_EPS, dist);
  
  // Thickness parameters for second spiral arm
  float a_r2 = smoothstep(r2 - R_EPS, r2, dist);
  float b_r2 = smoothstep(r2, r2 + R_EPS, dist);
  
  // Attenuate fragment alpha based on whether it lies on spiral arms
  gl_FragColor = gl_Color;
  gl_FragColor.a *= max(a_r1 - b_r1, a_r2 - b_r2);
}