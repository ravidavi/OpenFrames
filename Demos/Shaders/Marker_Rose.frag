// FRAGMENT SHADER
// Draws a polar-coordinate 4-point rose
// Equation is r = 0.5*sin(2*theta) = sin(theta)*cos(theta)

// Use GLSL 1.20 (OpenGL 2.1)
#version 120

vec2 v;
float r;
float dist;

void main(void)
{
  // Move origin to point center
  v = gl_PointCoord - vec2(0.5);

  // Length of current fragment coordinate
  r = length(v);

  // Make sure origin fragment is accepted
  if(r <= 0.001)
  {
    dist = 0.0;
  }
  else
  {
    // Compute whether fragment is inside rose
    dist = r - abs((v.y*v.x)/(r*r));
  }

  // Discard fragments outside the rose
  if(dist > 0)
  {
    discard;
  }

  gl_FragColor = gl_Color;
}
