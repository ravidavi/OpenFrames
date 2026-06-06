// FRAGMENT SHADER
// Draws a pulse that travels from the start of the trajectory to the end

// Use GLSL 1.20 (OpenGL 2.1)
#version 120

uniform float osg_FrameTime;      // From OSG
uniform float of_NumVertices;     // Total vertex count, set by CurveArtist
varying float vertexRawID;        // Raw vertex index from vertex shader

const float pulseDuration = 2.0;  // Seconds for one full pass along the line
const float pulseWidth = 0.05;    // Width of pulse as fraction of total line length

void main(void)
{
  vec4 color = gl_Color;
  vec4 colorInv = vec4(1.0 - gl_Color.rgb, 1.0);

  // Normalize vertex index to [0, 1] over the full line
  float linePos = vertexRawID / max(of_NumVertices - 1.0, 1.0);

  // Pulse center travels from 0 (start) to 1 (end) over pulseDuration seconds
  float pulseCenter = fract(osg_FrameTime / pulseDuration);

  // Create a smooth 0-1-0 bump centered at the pulse location
  float y1 = smoothstep(pulseCenter - pulseWidth, pulseCenter, linePos);
  float y2 = smoothstep(pulseCenter, pulseCenter + pulseWidth, linePos);
  float pulseVal = y1 - y2;

  gl_FragColor = mix(color, colorInv, pulseVal);
}
