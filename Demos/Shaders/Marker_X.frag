// FRAGMENT SHADER
// Draws an X centered on the origin
// The X is defined by the equations:
//    y = x
//    y = 1 - x
// Fragments not on either of those lines are discarded

// Use GLSL 1.20 (OpenGL 2.1)
#version 120

vec2 dist;

void main(void)
{
  // Compute distance to y=x line
  dist.x = abs(gl_PointCoord.t - gl_PointCoord.s);

  // Compute distance to y=1-x line
  dist.y = abs(gl_PointCoord.t + gl_PointCoord.s - 1.0);

  // Discard fragments outside both lines
  if((dist.x > 0.1) && (dist.y > 0.1))
  {
    discard;
  }

  gl_FragColor = gl_Color;
}
