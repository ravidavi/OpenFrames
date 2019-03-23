// Fragment shader when there is a base texture
//
uniform sampler2D osgShadow_baseTexture;
uniform sampler2DShadow osgShadow_penumbraDepthTexture;
uniform sampler2D osgShadow_umbraDepthTexture;
uniform vec2  osgShadow_ambientBias;
uniform float osgShadow_umbraDistance;       // Distance from umbral focal point to center of shadowing body
uniform vec2  osgShadow_umbraZNearFarInv;    // Inverse of umbra near and far planes
//uniform float osgShadow_umbraZFarLightRatio; // Ratio of umbra far plane to light distance
uniform float osgShadow_lightDistance;       // Distance from shadowing body center to light center
uniform float osgShadow_sizeRatio;           // Ratio of shadowing body radius to light radius
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

float Distance2Depth(float dist)
{
  return (1.0/dist - osgShadow_umbraZNearFarInv.x)/(osgShadow_umbraZNearFarInv.y - osgShadow_umbraZNearFarInv.x);
}

float Depth2Distance(float depth)
{
  float distinv = depth*(osgShadow_umbraZNearFarInv.y - osgShadow_umbraZNearFarInv.x) + osgShadow_umbraZNearFarInv.x;
  return 1.0/distinv;
}

// Compute center and radius of shadow coverage circle for given texture coordinate and depth
// TODO: Should this be converted to elliptical form?
// - texture coordinate (x,y) in NDC coordinates [-1,+1]
// - depth is actual depth distance from umbra focal point, assumed <= far plane distance
// + Returns vec3 = [vec2(center point), float(radius)] in texture coordinates [0, 1]
vec3 ShadowCoverageCircle(vec2 uTexCoordNDC, float fragDepth, float blockerDepth, float lightDepth)
{
  // Ratio of fragment distance to blocker distance
  float zF_zBlocker = fragDepth / blockerDepth;

  // Ratio of fragment distance to light distance
  float zF_zL = fragDepth / lightDepth;
  
  // Distance to upper and lower shadow coverage texture coordinates
  float dist = length(uTexCoordNDC.xy);
  float coeff = (1.0 - zF_zBlocker)/(1.0 - zF_zL);
  float CoverageDistUpper = dist*zF_zBlocker + coeff*(1.0 - dist*zF_zL);
  float CoverageDistLower = -(-dist*zF_zBlocker + coeff*(1.0 + dist*zF_zL));
  
  // Distance to center of shadow coverage circle
  float CoverageDistCenter = 0.5 * (CoverageDistUpper + CoverageDistLower);
  
  // Shadow coverage center and radius
  vec2 dir = (dist == 0.0) ? vec2(1.0, 0.0) : normalize(uTexCoordNDC.xy);
  vec2 center = CoverageDistCenter * dir * 0.5 + 0.5; // [-1,1] NDC -> [0,1] texcoords
  float radius = (CoverageDistUpper - CoverageDistCenter)*0.5; // NDC -> texcoords radius compression
  return vec3(center, radius);
}

float BlockerDistance(sampler2D depthTex, vec4 texCoord, vec3 coverageCircle)
{
  int numBlockerSearchSamples = 8;
  int numBlockers = 0;
  float avgBlockerDepth = 0.0;
  vec2 center = coverageCircle.xy; // Extract shadow coverage circle center
  float radius = coverageCircle.z; // Extract shadow coverage circle radius
  float phi = random(texCoord.xy)*TWOPI;
  vec3 sampleTexCoord = texCoord.xyz;
  
  for (int i = 0; i < numBlockerSearchSamples; ++i)
  {
    vec2 offset = VogelDiskSample(i, numBlockerSearchSamples, phi) * radius;
    sampleTexCoord.xy = center + offset;
    
    float depth = texture2D(depthTex, sampleTexCoord.xy).r;
    if ((depth != 0.0) && (depth > sampleTexCoord.z))
    {
      ++numBlockers;
      avgBlockerDepth += depth;
    }
  }
  
  if(numBlockers == 0) return -1.0;
  else return Depth2Distance(avgBlockerDepth / float(numBlockers));

}

// Compute visibility of given texture coordinate subject to given shadow coverage circle
// - depthTex: depth texture
// - texCoord: texture coordinates [0, 1]
float Visibility_ModifiedPCSS(sampler2D depthTex, vec4 texCoord, vec3 coverageCircle)
{
  vec2 center = coverageCircle.xy; // Extract shadow coverage circle center
  float radius = coverageCircle.z; // Extract shadow coverage circle radius
  int numPoints = 32;
  
  float visibility = 0.0;
  
  vec3 sampleTexCoord = texCoord.xyz;
  float phi = random(texCoord.xy)*TWOPI;
  
  // Iterate over all sample points
  for(int i = 0; i < numPoints; ++i)
  {
    vec2 offset = VogelDiskSample(i, numPoints, phi) * radius;
    sampleTexCoord.xy = center + offset;
    
#if 0
    //visibility += shadow2D(depthTex, sampleTexCoord).r;
    float depth = texture2D(depthTex, sampleTexCoord.xy).r;
    visibility += ((depth == 0.0) || (depth <= sampleTexCoord.z)) ? 1.0 : 0.0;
    
#else
    // TODO: Use approximation for this, maybe paraboloid or right cone centered at fragment
    //       and with radius to lower coverage point

    vec2 dF = center - texCoord.xy; // Vec from frag to circle center
    vec2 dSample = dF + offset;     // Vec from frag to current sample point
    float dFLen = length(dF);
    float dSampleLen = length(dSample);
    vec2 dFDir = (dFLen == 0.0) ? vec2(1.0, 0.0) : normalize(dF);
    vec2 dSampleDir = (dSampleLen == 0.0) ? vec2(1.0, 0.0) : normalize(dSample);
    
    // Compute ratio of frag->sample and frag->circle_edge distances
    // This is used to adjust the depth value used during testing
    float ca = dot(dFDir, dSampleDir);
    float dist = dFLen*ca + sqrt(dFLen*dFLen*(ca*ca - 1.0) + radius*radius);
    dist = clamp(dist, 0.00001, 2.0*radius);
    float ratio = clamp(dSampleLen/dist, 0.0, 1.0);
    
    // Interpolate minimum depth between fragment and coverage circle (at far plane = 1.0)
    dist = mix(texCoord.w, 1.0/osgShadow_umbraZNearFarInv.y, ratio);
    sampleTexCoord.z = Distance2Depth(dist);
    
    //visibility += shadow2D(depthTex, sampleTexCoord).r;
    float depth = texture2D(depthTex, sampleTexCoord.xy).r;
    visibility += ((depth == 0.0) || (depth <= sampleTexCoord.z)) ? 1.0 : 0.0;
#endif
  }
  
  //visibility += shadow2D(depthTex, texCoord.xyz).r;
  //float maxDepth = texture2D(depthTex, texCoord.xy).r;
  //visibility += ((maxDepth == 0.0) || (maxDepth <= texCoord.z)) ? 1.0 : 0.0;
  //++numPoints;
  
  return visibility / float(numPoints);
}

float Visibility_PCF(sampler2DShadow depthTex, vec4 texCoord)
{
  const int maxPointID = 2;
  const float numPoints = 2.0*float(maxPointID) + 1.0;
  float visibility = 0.0;
  for(int x = -maxPointID; x <= maxPointID; ++x){
    for(int y = -maxPointID; y <= maxPointID; ++y){
      vec2 off = 1.0*vec2(x,y) * osgShadow_texelSize;
      visibility += shadow2D(depthTex, texCoord.xyz + vec3(off, 0.0)).r;
    }
  }
  return visibility / (numPoints * numPoints);
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

#if 0
  float uDistance = max(-uTexCoord.w, 0.0);    // z-distance from umbra focal point to fragment
  float pDistance = max(-pTexCoord.w, 0.0);    // z-distance from penumbra focal point to fragment
  float pRatio = length(pTexCoordNDC);  // Assumes circular light due to implicit divide by 1
  float uRatio = length(uTexCoordNDC);  // TODO: For square light divide by distance to square edge
  
  // Compute amount of guaranteed visible light in umbra/antumbra
  // This is always zero in umbra, and asymptotically increases with distance in antumbra
  float uBlockedLight = osgShadow_sizeRatio * (uDistance + osgShadow_umbraDistance + osgShadow_lightDistance) / (uDistance + osgShadow_umbraDistance);
  float uMinVisibility = 1.0 - uBlockedLight*uBlockedLight;
  
  // Determine the minimum amount of light this fragment is guaranteed to see based on whether
  // it is in penumbra, umbra, or antumbra. Visibility is in range [uMinVisibility, 1.0]
  float lightMinVisibility = 1.0;  // Assume full light visibility unless computed otherwise
  uRatio = max(uRatio, 1.0);  // Anything less than 1 will use uMinVisibility anyway
  if(pTexCoord.w > 0.0) lightMinVisibility = pRatio * (uRatio - 1.0) / (uRatio - pRatio);
  lightMinVisibility = clamp(lightMinVisibility, 0.0, 1.0);
  lightMinVisibility = uMinVisibility + lightMinVisibility*(1.0 - uMinVisibility);
  
  // In antumbra the umbra texture lookup is reversed, so reverse texcoords preemptively
  if(uTexCoord.w < 0.0) uTexCoord.xyz = -uTexCoord.xyz + vec3(1.0, 1.0, 0.0);
  
  // Lookup umbral and penumbral visibility from their texture maps
  float uVisibility = 0.0, pVisibility = 0.0;
  //pVisibility = shadow2D(osgShadow_penumbraDepthTexture, pTexCoord.xyz).r;
  //uVisibility = shadow2D(osgShadow_umbraDepthTexture, uTexCoord.xyz).r;
  uVisibility = uVisibility_PCF();
  pVisibility = pVisibility_PCF();
  
  // Compute fragment color
  float minVisibility = osgShadow_ambientBias.x + lightMinVisibility * osgShadow_ambientBias.y;
  float shadowedVisibility = (1.0 - lightMinVisibility) * 0.5 * (uVisibility + pVisibility) * osgShadow_ambientBias.y;
  gl_FragColor = color * (minVisibility + shadowedVisibility);

#else
  // In antumbra the umbra texture lookup is reversed, so reverse texcoords preemptively
  if(uTexCoord.w < 0.0) uTexCoord.xyz = -uTexCoord.xyz + vec3(1.0, 1.0, 0.0);
  
  //float uVisibility = Visibility_PCF(osgShadow_umbraDepthTexture, uTexCoord);
  vec3 uCoverageCircle = ShadowCoverageCircle(uTexCoordNDC, uTexCoord.w, 1.0/osgShadow_umbraZNearFarInv.y, osgShadow_umbraDistance + osgShadow_lightDistance);
  float blockerDistance = BlockerDistance(osgShadow_umbraDepthTexture, uTexCoord, uCoverageCircle);
  vec4 uVisibility = vec4(1.0);
  if(blockerDistance != -1.0)
  {
    uCoverageCircle = ShadowCoverageCircle(uTexCoordNDC, uTexCoord.w, blockerDistance, osgShadow_umbraDistance + osgShadow_lightDistance);
    uVisibility = vec4(Visibility_ModifiedPCSS(osgShadow_umbraDepthTexture, uTexCoord, uCoverageCircle));
  }
  
  //vec4 pVisibility = vec4(Visibility_PCF(osgShadow_penumbraDepthTexture, pTexCoord));
  vec4 pVisibility = uVisibility;
  
  float minVisibility = osgShadow_ambientBias.x;
  vec4 shadowedVisibility = 0.5 * (uVisibility + pVisibility) * osgShadow_ambientBias.y;
  gl_FragColor = color * (minVisibility + shadowedVisibility);
  
#endif
}
