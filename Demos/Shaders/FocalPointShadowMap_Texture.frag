// Fragment shader when there is a base texture
//
// Use GLSL 1.20 (OpenGL 2.1)
#version 120

uniform sampler2D osgShadow_baseTexture;
uniform sampler2D osgShadow_penumbraDepthTexture;
uniform sampler2D osgShadow_umbraDepthTexture;
uniform vec2  osgShadow_ambientBias;
uniform float osgShadow_umbraDistance;       // Distance from umbral focal point to center of shadowing body
uniform vec2  osgShadow_umbraZNearFarInv;    // Inverse of umbra near and far planes
uniform float osgShadow_penumbraDistance;    // Distance from penumbral focal point to center of shadowing body
uniform vec2  osgShadow_penumbraZNearFarInv; // Inverse of penumbra near and far planes
uniform float osgShadow_lightDistance;       // Distance from shadowing body center to light center
uniform vec2  osgShadow_texelSize;           // Size of each texel in normalized device coordinates (NDC)

#define PI  3.14159265359
#define TWOPI 6.28318530718
#define GOLDENANGLE 2.39996323

// Reference: https://thebookofshaders.com/10/
float random(vec2 p)
{
  return fract(sin(dot(p, vec2(12.9898,78.233))) * 43758.5453123);
}

// Reference: https://www.gamedev.net/articles/programming/graphics/contact-hardening-soft-shadows-made-fast-r4906/
vec2 VogelDiskSample(int sampleIndex, int samplesCount, float phi)
{
  float sampleIndexf = float(sampleIndex);
  float samplesCountf = float(samplesCount);
  
  float r = sqrt((sampleIndexf + 0.5) / samplesCountf);
  float theta = sampleIndexf * GOLDENANGLE + phi;
  
  return vec2(r * sin(theta), r * cos(theta));
}

float Distance2Depth(float dist, vec2 zNearFarInv)
{
  return (1.0/dist - zNearFarInv.x)/(zNearFarInv.y - zNearFarInv.x);
}

float Depth2Distance(float depth, vec2 zNearFarInv)
{
  float distinv = depth*(zNearFarInv.y - zNearFarInv.x) + zNearFarInv.x;
  return 1.0/distinv;
}

// Compute center and radius of shadow coverage circle for given texture coordinate and test plane
// TODO: Should this be converted to elliptical form?
// NOTE: Distances are signed and increase from umbra/penumbra focal point towards light
// - texture coordinate (x,y) in NDC coordinates [-1,+1]
// - fragDist/planeDist/lightDist are actual depth distances from focal point
// - planeSign is +1.0 for umbra (faces towards light), -1.0 for penumbra (faces away from light)
// + Returns vec3 = [vec2(center point), float(radius)] in texture coordinates [0, 1]
vec3 ShadowCoverageCircle(vec2 texCoordNDC, float fragDist, float planeDist, float lightDist, float planeSign)
{
  // Ratio of fragment distance to test plane distance
  float zF_zP = fragDist / planeDist;
  float zFabs_zP = abs(fragDist) / planeDist;

  // Ratio of fragment distance to light distance
  float zF_zL = fragDist / lightDist;
  float zFabs_zL = abs(zF_zL); // Assumes lightDist > 0, which is true for our coordinate system
  
  // Distance to upper and lower shadow coverage texture coordinates
  float dist = (fragDist == 0.0) ? 0.0 : length(texCoordNDC.xy);
  float coeff = (1.0 - zF_zP)/(1.0 - zF_zL);
  float CoverageDistUpper = planeSign*(dist*zFabs_zP + coeff*(1.0 - dist*zFabs_zL));
  float CoverageDistLower = -planeSign*(-dist*zFabs_zP + coeff*(1.0 + dist*zFabs_zL));
  
  // Distance to center of shadow coverage circle
  float CoverageDistCenter = 0.5 * (CoverageDistUpper + CoverageDistLower);
  
  // Shadow coverage center and radius
  vec2 dir = (dist == 0.0) ? vec2(1.0, 0.0) : normalize(texCoordNDC.xy);
  vec2 center = CoverageDistCenter * dir * 0.5 + 0.5; // [-1,1] NDC -> [0,1] texcoords
  float radius = (CoverageDistUpper - CoverageDistCenter)*0.5; // NDC -> texcoords radius compression
  return vec3(center, radius);
}

vec2 BlockerDistance(sampler2D depthTex, vec4 texCoord, vec3 coverageCircle, vec2 zNearFarInv, float planeSign)
{
  int numBlockerSearchSamples = 4;
  int numBlockers = 0;
  float avgBlockerDepth = 0.0;
  vec2 center = coverageCircle.xy; // Extract shadow coverage circle center
  float radius = coverageCircle.z; // Extract shadow coverage circle radius
  float phi = random(coverageCircle.xy)*TWOPI;
  vec3 sampleTexCoord = texCoord.xyz;
  float depth;
  
  for (int i = 0; i < numBlockerSearchSamples; ++i)
  {
    // Get sample coordinates
    vec2 offset = VogelDiskSample(i, numBlockerSearchSamples, phi) * radius;
    sampleTexCoord.xy = center + offset;
    
    // Lookup sample depth and compare it to fragment depth
    depth = texture2D(depthTex, sampleTexCoord.xy).r;
    if ((depth != (0.5 - planeSign*0.5)) && (planeSign*depth > planeSign*sampleTexCoord.z))
    {
      ++numBlockers;
      avgBlockerDepth += depth;
    }
  }
  
  if(numBlockers == 0) return vec2(-1.0, numBlockerSearchSamples);
  else if (numBlockers == numBlockerSearchSamples) return vec2(-2.0, 0.0);
  else return vec2(Depth2Distance(avgBlockerDepth / numBlockers, zNearFarInv), numBlockerSearchSamples - numBlockers);
}

// Compute visibility of given texture coordinate subject to given shadow coverage circle
// - depthTex: depth texture
// - texCoord: texture coordinates [0, 1]
vec2 Visibility_ModifiedPCSS(sampler2D depthTex, vec4 texCoord, vec3 coverageCircle, float planeSign)
{
  vec2 center = coverageCircle.xy; // Extract shadow coverage circle center
  float radius = coverageCircle.z; // Extract shadow coverage circle radius
  int numSamples = 16;
  
  float blockers = 0.0;
  
  vec3 sampleTexCoord = texCoord.xyz;
  float phi = random(coverageCircle.xy)*TWOPI;

  // Iterate over all sample points
  for(int i = 0; i < numSamples; ++i)
  {
    vec2 offset = VogelDiskSample(i, numSamples, phi) * radius;
    sampleTexCoord.xy = center + offset;
    
    float depth = texture2D(depthTex, sampleTexCoord.xy).r;
    blockers += ((depth != (0.5 - planeSign*0.5)) && (planeSign*depth > planeSign*sampleTexCoord.z)) ? 1.0 : 0.0;
  }
  
  return vec2(numSamples-blockers, numSamples);
}

void main(void)
{
  // Get base fragment color, which will be attenuated based on visible light
  vec4 color = gl_Color * texture2D(osgShadow_baseTexture, gl_TexCoord[0].xy);
  
  // TexCoords are in range [0, w], so recover NDC coordinates in range [-1, 1]
  vec4 pTexCoord = gl_TexCoord[1];
  vec4 uTexCoord = gl_TexCoord[2];
  pTexCoord.xyz = pTexCoord.xyz / pTexCoord.w; // [0, w] -> [0, 1]
  uTexCoord.xyz = uTexCoord.xyz / uTexCoord.w; // [0, w] -> [0, 1]
  vec2 pTexCoordNDC = pTexCoord.xy * 2.0 - 1.0;   // [0, 1] -> [-1, 1]
  vec2 uTexCoordNDC = uTexCoord.xy * 2.0 - 1.0;   // [0, 1] -> [-1, 1]

  // In antumbra the umbra texture lookup is reversed, so reverse texcoords preemptively
  if(uTexCoord.w < 0.0)
  {
    uTexCoord.xyz = -uTexCoord.xyz + vec3(1.0, 1.0, 0.0);
    uTexCoordNDC *= -1.0;
  }

  // Umbra blocker search (use umbra far plane)
  vec3 uCoverageCircle = ShadowCoverageCircle(uTexCoordNDC, uTexCoord.w, 1.0/osgShadow_umbraZNearFarInv.y, osgShadow_lightDistance + osgShadow_umbraDistance, 1.0);
  vec2 uBlockerDistance = BlockerDistance(osgShadow_umbraDepthTexture, uTexCoord, uCoverageCircle, osgShadow_umbraZNearFarInv, 1.0);
  
  // Umbra shadowing
  vec4 uVisibility = (uBlockerDistance.x == -2.0) ? vec4(0.0) : vec4(1.0);
  if(uBlockerDistance.x >= 0.0)
  {
    uCoverageCircle = ShadowCoverageCircle(uTexCoordNDC, uTexCoord.w, uBlockerDistance.x, osgShadow_lightDistance + osgShadow_umbraDistance, 1.0);
    vec2 visibility = Visibility_ModifiedPCSS(osgShadow_umbraDepthTexture, uTexCoord, uCoverageCircle, 1.0);
    uVisibility = vec4((visibility.x + uBlockerDistance.y)/(visibility.y + uBlockerDistance.y));
  }
  
  // Penumbra blocker search (use penumbra near plane and adjust for z increasing towards light)
  vec3 pCoverageCircle = ShadowCoverageCircle(pTexCoordNDC, -pTexCoord.w, -1.0/osgShadow_penumbraZNearFarInv.x, osgShadow_lightDistance - osgShadow_penumbraDistance, -1.0);
  vec2 pBlockerDistance = BlockerDistance(osgShadow_penumbraDepthTexture, pTexCoord, pCoverageCircle, osgShadow_penumbraZNearFarInv, -1.0);
  
  // Penumbra shadowing
  vec4 pVisibility = (pBlockerDistance.x == -2.0) ? vec4(0.0) : vec4(1.0);
  if(pBlockerDistance.x >= 0.0)
  {
    pCoverageCircle = ShadowCoverageCircle(pTexCoordNDC, -pTexCoord.w, -pBlockerDistance.x, osgShadow_lightDistance - osgShadow_penumbraDistance, -1.0);
    vec2 visibility = Visibility_ModifiedPCSS(osgShadow_penumbraDepthTexture, pTexCoord, pCoverageCircle, -1.0);
    pVisibility = vec4((visibility.x + pBlockerDistance.y)/(visibility.y + pBlockerDistance.y));
  }
  
  //uVisibility.g = 1.0; // Umbra shadow in green
  //pVisibility.r = 1.0; // Penumbra shadow in red
  
  //uVisibility = pVisibility;
  //pVisibility = uVisibility;
  
  float minVisibility = osgShadow_ambientBias.x;
  vec4 shadowedVisibility = 0.5 * (uVisibility + pVisibility) * osgShadow_ambientBias.y;
  gl_FragColor = color * (minVisibility + shadowedVisibility);
}
