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

vec2 SearchBlockers(sampler2D depthTex, vec4 texCoord, vec3 coverageCircle, vec2 zNearFarInv, float planeSign, int numSearchSamples)
{
  float phi = random(coverageCircle.xy)*TWOPI;
  vec3 sampleTexCoord = texCoord.xyz;
  float clearDepth = 0.5 - planeSign*0.5; // Depth value if there is no blocker
  int numBlockers = 0;
  float avgBlockerDepth = 0.0;
  float depth;
  
  for (int i = 0; i < numSearchSamples; ++i)
  {
    // Get sample coordinates
    vec2 offset = VogelDiskSample(i, numSearchSamples, phi) * coverageCircle.z;
    sampleTexCoord.xy = coverageCircle.xy + offset;
    
    // Lookup sample depth and compare it to fragment depth
    depth = texture2D(depthTex, sampleTexCoord.xy).r;
    if ((depth != clearDepth) && (planeSign*depth > planeSign*sampleTexCoord.z))
    {
      ++numBlockers;
      avgBlockerDepth += depth;
    }
  }
  
  avgBlockerDepth /= max(numBlockers, 1.0);
  return vec2(Depth2Distance(avgBlockerDepth, zNearFarInv), numBlockers);
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

  int numBlockerSamples = 4;
  int numShadowSamples = 16;
  
  // Umbra blocker search (using umbra far plane)
  vec3 uCoverageCircle = ShadowCoverageCircle(uTexCoordNDC, uTexCoord.w, 1.0/osgShadow_umbraZNearFarInv.y, osgShadow_lightDistance + osgShadow_umbraDistance, 1.0);
  vec2 uBlockers = SearchBlockers(osgShadow_umbraDepthTexture, uTexCoord, uCoverageCircle, osgShadow_umbraZNearFarInv, 1.0, numBlockerSamples);
  
  // Penumbra blocker search (using penumbra near plane and adjusting for z increasing towards light)
  vec3 pCoverageCircle = ShadowCoverageCircle(pTexCoordNDC, -pTexCoord.w, -1.0/osgShadow_penumbraZNearFarInv.x, osgShadow_lightDistance - osgShadow_penumbraDistance, -1.0);
  vec2 pBlockers = SearchBlockers(osgShadow_penumbraDepthTexture, pTexCoord, pCoverageCircle, osgShadow_penumbraZNearFarInv, -1.0, numBlockerSamples);
  
  // Adjust umbra blocker distance based on info from penumbra blocker search
  float uBlockerDistanceFromPenumbra = (osgShadow_umbraDistance + osgShadow_penumbraDistance) - pBlockers.x;
  vec2 uBlockersAdjusted = vec2(uBlockers.x*uBlockers.y + uBlockerDistanceFromPenumbra*pBlockers.y, uBlockers.y + pBlockers.y);
  uBlockersAdjusted.x /= uBlockersAdjusted.y;
  //uBlockersAdjusted = uBlockers;
  
  // Adjust penumbra blocker distance based on info from umbra blocker search
  float pBlockerDistanceFromUmbra = (osgShadow_umbraDistance + osgShadow_penumbraDistance) - uBlockers.x;
  vec2 pBlockersAdjusted = vec2(pBlockers.x*pBlockers.y + pBlockerDistanceFromUmbra*uBlockers.y, uBlockers.y + pBlockers.y);
  pBlockersAdjusted.x /= pBlockersAdjusted.y;
  //pBlockersAdjusted = pBlockers;
  
  // Compute umbra shadowing if there are any blockers
  vec4 uVisibility = vec4(1.0);
  if(uBlockersAdjusted.y > 0.0)
  {
    uCoverageCircle = ShadowCoverageCircle(uTexCoordNDC, uTexCoord.w, uBlockersAdjusted.x, osgShadow_lightDistance + osgShadow_umbraDistance, 1.0);
    vec2 uShadows = SearchBlockers(osgShadow_umbraDepthTexture, uTexCoord, uCoverageCircle, osgShadow_umbraZNearFarInv, 1.0, numShadowSamples);
    uVisibility = vec4(1.0 - uShadows.y/numShadowSamples); // TODO: Add blocker info when computing visibility
  }
  
  // Compute penumbra shadowing if there are any blockers
  vec4 pVisibility = vec4(1.0);
  if(pBlockersAdjusted.y > 0.0)
  {
    pCoverageCircle = ShadowCoverageCircle(pTexCoordNDC, -pTexCoord.w, -pBlockersAdjusted.x, osgShadow_lightDistance - osgShadow_penumbraDistance, -1.0);
    vec2 pShadows = SearchBlockers(osgShadow_penumbraDepthTexture, pTexCoord, pCoverageCircle, osgShadow_penumbraZNearFarInv, -1.0, numShadowSamples);
    pVisibility = vec4(1.0 - pShadows.y/numShadowSamples); // TODO: Add blocker info when computing visibility
  }
  
  //uVisibility.g = 1.0; // Umbra shadow in green
  //pVisibility.r = 1.0; // Penumbra shadow in red
  
  //uVisibility = pVisibility;
  //pVisibility = uVisibility;
  
  float minVisibility = osgShadow_ambientBias.x;
  vec4 shadowedVisibility = 0.5 * (uVisibility + pVisibility) * osgShadow_ambientBias.y;
  gl_FragColor = color * (minVisibility + shadowedVisibility);
}
