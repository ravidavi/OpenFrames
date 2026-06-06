// FRAGMENT SHADER
// Creates a pulsing intensity effect that simulates line thickness variation

// Use GLSL 1.20 (OpenGL 2.1)
#version 120

uniform float osg_FrameTime;     // From OSG
varying float vertexLocation;    // From vertex shader (alternates 0/1 for line endpoints)

const float pulseSpeed = 2.0;      // Speed of pulsing (higher = faster)
const float minIntensity = 0.4;    // Minimum brightness (0-1)
const float maxIntensity = 1.0;    // Maximum brightness (0-1)

void main(void)
{
  vec4 baseColor = gl_Color;
  
  // Create smooth pulsing wave using sine
  float pulse = sin(osg_FrameTime * pulseSpeed) * 0.5 + 0.5; // 0 to 1
  
  // Scale pulse between min and max intensity
  float intensity = mix(minIntensity, maxIntensity, pulse);
  
  // Apply intensity to color and alpha for a "glowing" pulse effect
  vec4 finalColor = baseColor * intensity;
  finalColor.a = baseColor.a * intensity; // Also pulse the alpha
  
  gl_FragColor = finalColor;
}
