// FRAGMENT SHADER
// Draws a circle (not filled in)

// Use GLSL 1.20 (OpenGL 2.1)
#version 120

vec2 v;
float dist;

void main(void)
{
  // Move origin to point center
  v = gl_PointCoord - vec2(0.5);

  // Discard fragments not on the circle border
  dist = dot(v,v);
  if((dist > 0.25) || (dist < 0.16))
  {
    discard;
  }

  gl_FragColor = gl_Color;
}
