// FRAGMENT SHADER
// Draws a pulsing disk (filled circle)

// Use GLSL 1.20 (OpenGL 2.1)
#version 120

uniform float osg_FrameTime;

void main(void)
{
  // Move origin to point center
  vec2 v = gl_PointCoord - vec2(0.5);

  // Compute fragment distance from center
  float dist = length(v);

  gl_FragColor = gl_Color;

  // Don't draw fragments outside the disk
  if(dist > 0.5)
  {
    gl_FragColor.a = 0.0;
  }
  
  // Fragments inside the disk should fade in a pulsing pattern
  else
  {
    dist = mod(dist - 1.0*osg_FrameTime, 0.5);
    float y1 = smoothstep(0, 0.25, dist);
    float y2 = smoothstep(0.25, 0.5, dist);
    gl_FragColor.a *= y1 - y2;
  }
}