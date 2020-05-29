// FRAGMENT SHADER
// Draws a pulsing line

// Use GLSL 1.20 (OpenGL 2.1)
#version 120

uniform float osg_FrameTime;     // From OSG
varying float vertexLocation;    // From CustomLineSegments vertex shader
const float pulseDuration = 2.0; // In seconds
const float pulseWidth = 0.1;    // In fraction of total line length


void main(void)
{
  vec4 color = gl_Color; // Input fragment color
  vec4 colorInv = vec4(1.0 - gl_Color.rgb, 1.0); // Inverted color
  
  // Create a smooth 0-1-0 transition centered at pulse
  float pulseCenter = fract(osg_FrameTime/pulseDuration);
  float y1 = smoothstep(pulseCenter - pulseWidth, pulseCenter, vertexLocation);
  float y2 = smoothstep(pulseCenter, pulseCenter + pulseWidth, vertexLocation);
  float pulseVal = y1 - y2;
  
  // Mix input and inverted colors
  gl_FragColor = mix(color, colorInv, pulseVal);
}
