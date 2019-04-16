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

float phi;

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
// + Returns vec3 = [center point (vec2), radius (float)] in texture coordinates [0, 1]
vec4 ShadowCoverageCircle(vec2 texCoordNDC, float fragDist, float planeDist, float lightDist, float planeSign)
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
  
  // Shadow coverage center and radius (in NDC)
  vec2 dir = (dist == 0.0) ? vec2(1.0, 0.0) : normalize(texCoordNDC.xy);
  vec2 center = CoverageDistCenter * dir;
  float radius = CoverageDistUpper - CoverageDistCenter;
  
  // If part of coverage circle is outside the texture, then only search samples inside the texture
  // and attenuate using percentage of coverage circle inside texture
  // See D.Assencio, "The intersection area of two circles", 7/12/2017
  // https://diego.assencio.com/?index=8d6ca3d82151bad815f78addf9b5c1c6
  float r1 = 1.0;           // Radius of texture, assumes circular light
  float r2 = max(radius, 0.001); // Radius of coverage circle
  float d = length(center); // Distance from texture center (at origin) to coverage circle center
  
  float A1 = PI;
  float A2 = PI*r2*r2;
  
  float Aintersect = 0.0;
  if(d <= abs(r1 - r2)) Aintersect = min(A1, A2);
  else if(d < r1 + r2)
  {
    float r1_sq = 1.0;
    float r2_sq = r2*r2;
    float d1 = (r1_sq - r2_sq + d*d)/(2.0*d);
    float d2 = d - d1;
    Aintersect =  r1_sq*acos(d1/r1) - d1*sqrt(r1_sq - d1*d1);
    Aintersect += r2_sq*acos(d2/r2) - d2*sqrt(r2_sq - d2*d2);
  }
  
  // Convert center and radius from NDC [-1,1] -> texcoords [0, 1]
  center = center * 0.5 + 0.5;
  radius = radius * 0.5;
  
  return vec4(center, radius, Aintersect/A2);
}

vec2 SearchBlockers(sampler2D depthTex, vec4 texCoord, vec4 coverageCircle, vec2 zNearFarInv, float planeSign, const int numSearchSamples)
{
  //phi = random(texCoord.xy)*TWOPI;
  vec3 sampleTexCoord = texCoord.xyz;
  float clearDepth = (planeSign == 1.0) ? 0.0 : 1.0; // Depth value if there is no blocker (0 for umbra, 1 for penumbra)
  vec2 blockerData = vec2(0.0, 0.0); // (average blocker depth, number of blockers)
  float depth;
  
  // Get coverage circle in NDC [-1, 1] coordinates
  vec2 centerNDC = 2.0*coverageCircle.xy - 1.0;
  float radiusNDC = 2.0*coverageCircle.z;
  
  // Compute min/max distances to intersection area between coverage circle and texture (assuming circular light source)
  float rplus = length(centerNDC) + radiusNDC;
  float rminus = rplus - 2.0*radiusNDC;
  rplus = min(rplus, 1.0);
  rminus = max(rminus, -1.0);
  float dCenter = 0.5*(rplus + rminus);
  float rAverage = abs(rplus - dCenter);
  vec2 centerAverage = dCenter * normalize(centerNDC);
  centerAverage = centerAverage * 0.5 + 0.5;
  rAverage = rAverage*0.5;

  // Sample texture
  for (int i = 0; i < numSearchSamples; ++i)
  {
    // Get sample coordinates
    vec2 currsample = VogelDiskSample(i, numSearchSamples, phi);
    sampleTexCoord.xy = coverageCircle.xy + currsample*coverageCircle.z;
    
    if(length(sampleTexCoord.xy - vec2(0.5, 0.5)) > 0.5)
    {
      sampleTexCoord.xy = centerAverage + currsample*rAverage;
    }
    
    // Lookup sample depth and compare it to fragment depth
    depth = texture2D(depthTex, sampleTexCoord.xy).r;
    if ((depth != clearDepth) && (planeSign*depth > planeSign*sampleTexCoord.z))
    {
      blockerData.x += depth;
      ++blockerData.y;
    }
  }
  blockerData.x /= max(blockerData.y, 1.0); // Compute average, this is safe even if there are no blockers
  blockerData.x = Depth2Distance(blockerData.x, zNearFarInv); // Convert depth to absolute distance

  return blockerData;
}

void main(void)
{
  phi = random(gl_FragCoord.xy)*TWOPI;
  
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

  const int numBlockerSamples = 4;
  const int numShadowSamples = 4;
  
  // Umbra blocker search (using umbra far plane)
  float uBlockerDistGuess = Depth2Distance(texture2D(osgShadow_umbraDepthTexture, uTexCoord.xy).r, osgShadow_umbraZNearFarInv);
  uBlockerDistGuess = max(uBlockerDistGuess, uTexCoord.w);
  uBlockerDistGuess = min(uBlockerDistGuess, 1.0/osgShadow_umbraZNearFarInv.y);
  vec4 uCoverageCircle = ShadowCoverageCircle(uTexCoordNDC, uTexCoord.w, uBlockerDistGuess, osgShadow_lightDistance + osgShadow_umbraDistance, 1.0);
  vec2 uBlockers = SearchBlockers(osgShadow_umbraDepthTexture, uTexCoord, uCoverageCircle, osgShadow_umbraZNearFarInv, 1.0, numBlockerSamples);
  
  // Penumbra blocker search (using penumbra near plane and adjusting for z increasing towards light)
  float pBlockerDistGuess = Depth2Distance(texture2D(osgShadow_penumbraDepthTexture, pTexCoord.xy).r, osgShadow_penumbraZNearFarInv);
  pBlockerDistGuess = min(pBlockerDistGuess, pTexCoord.w);
  pBlockerDistGuess = max(pBlockerDistGuess, 1.0/osgShadow_penumbraZNearFarInv.x);
  vec4 pCoverageCircle = ShadowCoverageCircle(pTexCoordNDC, -pTexCoord.w, -pBlockerDistGuess, osgShadow_lightDistance - osgShadow_penumbraDistance, -1.0);
  vec2 pBlockers = SearchBlockers(osgShadow_penumbraDepthTexture, pTexCoord, pCoverageCircle, osgShadow_penumbraZNearFarInv, -1.0, numBlockerSamples);
  
  // Adjust umbra blocker distance based on info from penumbra blocker search
  float uBlockerDistanceFromPenumbra = (osgShadow_umbraDistance + osgShadow_penumbraDistance) - pBlockers.x;
  vec2 uBlockersAdjusted = vec2(uBlockers.x*uBlockers.y + uBlockerDistanceFromPenumbra*pBlockers.y, uBlockers.y + pBlockers.y);
  uBlockersAdjusted.x /= max(uBlockersAdjusted.y, 1.0);
  
  // Adjust penumbra blocker distance based on info from umbra blocker search
  float pBlockerDistanceFromUmbra = (osgShadow_umbraDistance + osgShadow_penumbraDistance) - uBlockers.x;
  vec2 pBlockersAdjusted = vec2(pBlockers.x*pBlockers.y + pBlockerDistanceFromUmbra*uBlockers.y, uBlockers.y + pBlockers.y);
  pBlockersAdjusted.x /= max(pBlockersAdjusted.y, 1.0);
  
  // Compute umbra shadowing if there are any blockers
  float uVisibility = 1.0;
  if(uBlockersAdjusted.y > 0.0)
  {
    uCoverageCircle = ShadowCoverageCircle(uTexCoordNDC, uTexCoord.w, uBlockersAdjusted.x, osgShadow_lightDistance + osgShadow_umbraDistance, 1.0);
    vec2 uShadows = SearchBlockers(osgShadow_umbraDepthTexture, uTexCoord, uCoverageCircle, osgShadow_umbraZNearFarInv, 1.0, numShadowSamples);
    uVisibility = 1.0 - (uShadows.y+uBlockersAdjusted.y)/(numShadowSamples+2.0*numBlockerSamples);
  }
  
  // Compute penumbra shadowing if there are any blockers
  float pVisibility = 1.0;
  if(pBlockersAdjusted.y > 0.0)
  {
    pCoverageCircle = ShadowCoverageCircle(pTexCoordNDC, -pTexCoord.w, -pBlockersAdjusted.x, osgShadow_lightDistance - osgShadow_penumbraDistance, -1.0);
    vec2 pShadows = SearchBlockers(osgShadow_penumbraDepthTexture, pTexCoord, pCoverageCircle, osgShadow_penumbraZNearFarInv, -1.0, numShadowSamples);
    pVisibility = 1.0 - (pShadows.y+pBlockersAdjusted.y)/(numShadowSamples+2.0*numBlockerSamples);
  }
  
  //uVisibility = pVisibility;
  //pVisibility = uVisibility;
  
  uVisibility = 1.0 - uCoverageCircle.w + uCoverageCircle.w*uVisibility;
  pVisibility = 1.0 - pCoverageCircle.w + pCoverageCircle.w*pVisibility;

  float minVisibility = osgShadow_ambientBias.x;
  float shadowedVisibility = 0.5 * (uVisibility + pVisibility);
  if(shadowedVisibility == 1.0) shadowedVisibility -= 0.5*fwidth(shadowedVisibility);
  shadowedVisibility *= osgShadow_ambientBias.y;
  gl_FragColor = color * (minVisibility + shadowedVisibility);
}
