// Fragment shader when there is a base texture
//
uniform sampler2D osgShadow_baseTexture;
uniform sampler2DShadow osgShadow_penumbraDepthTexture;
uniform sampler2DShadow osgShadow_umbraDepthTexture;
uniform vec2  osgShadow_ambientBias;
uniform float osgShadow_umbraDistance;       // Distance from umbral focal point to center of shadowing body
uniform vec2  osgShadow_umbraZNearFarInv;    // Inverse of umbra near and far planes
uniform float osgShadow_umbraZFarLightRatio; // Ratio of umbra far plane to light distance
uniform float osgShadow_lightDistance;       // Distance from shadowing body center to light center
uniform float osgShadow_sizeRatio;           // Ratio of shadowing body radius to light radius
uniform vec2  osgShadow_texelSize;           // Size of each texel in normalized device coordinates (NDC)

const float TWOPI = 2.0*3.14159265359;

// Compute center and radius of shadow coverage circle for given texture coordinate and depth
// - texture coordinate (x,y) in NDC coordinates [-1,+1]
// - depth is actual depth distance from umbra focal point, assumed <= far plane distance
// + Returns vec3 = [vec2(center point), float(radius)] in texture coordinates [0, 1]
vec3 ShadowCoverageCircle_Umbra(vec2 uTexCoordNDC, float depth)
{
  // Ratio of fragment distance to far plane
  float zF_zFar = depth*osgShadow_umbraZNearFarInv.y;
  
  // Ratio of fragment distance to light distance
  float zF_zL = zF_zFar*osgShadow_umbraZFarLightRatio;
  
  // Distance to positive and negatuve shadow coverage texture coordinates
  float dist = length(uTexCoordNDC.xy);
  float coeff = (1.0 - zF_zFar)/(1.0 - zF_zL);
  float CoverageDistPositive = dist*zF_zFar + coeff*(1.0 - dist*zF_zL);
  float CoverageDistNegative = -(-dist*zF_zFar + coeff*(1.0 + dist*zF_zL));
  
  // Distance to center of shadow coverage circle
  float CoverageDistCenter = 0.5 * (CoverageDistPositive + CoverageDistNegative);
  
  // Shadow coverage center and radius
  vec2 dir = (dist == 0.0) ? vec2(1.0, 0.0) : normalize(uTexCoordNDC.xy);
  vec2 center = CoverageDistCenter * dir * 0.5 + 1.0; // [-1,1] NDC -> [0,1] texcoords
  float radius = (CoverageDistPositive - CoverageDistCenter)*0.5; // Compress radius accordingly
  return vec3(center, radius);
}

// Compute visibility of given texture coordinate subject to given shadow coverage circle
// - depthTex: depth texture
// - texCoord: texture coordinates [0, 1]
float Visibility_ModifiedPCSS(sampler2DShadow depthTex, vec4 texCoord, vec3 coverageCircle)
{
  vec2 center = coverageCircle.xy;
  float radius = coverageCircle.z;
  const int numRadialPoints = 4;
  const int numAngles = 8;
  const float numPoints = 1.0 + float(numRadialPoints*numAngles); // Extra 1 for center point itself
  
  float visibility = 0.0;
  visibility += shadow2D(depthTex, texCoord.xyz).r;
  
  for(int x = 1; x <= numRadialPoints; ++x)
  {
    float r = radius * float(x) / float(numRadialPoints);
    
    for(int y = 0; y < numAngles; ++y)
    {
      float theta = TWOPI * float(y) / float(numAngles);
      vec2 offset = r * vec2(cos(theta), sin(theta));
      visibility += shadow2D(depthTex, texCoord.xyz + vec3(offset, 0.0)).r;
    }
  }
  
  return visibility / numPoints;
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
  vec2 pNDCCoord = pTexCoord.xy * 2.0 - 1.0;   // [0, 1] -> [-1, 1]
  vec2 uNDCCoord = uTexCoord.xy * 2.0 - 1.0;   // [0, 1] -> [-1, 1]

#if 0
  float uDistance = max(-uTexCoord.w, 0.0);    // z-distance from umbra focal point to fragment
  float pDistance = max(-pTexCoord.w, 0.0);    // z-distance from penumbra focal point to fragment
  float pRatio = length(pNDCCoord);  // Assumes circular light due to implicit divide by 1
  float uRatio = length(uNDCCoord);  // TODO: For square light divide by distance to square edge
  
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
  vec3 uCoverageCircle = ShadowCoverageCircle_Umbra(uNDCCoord, uTexCoord.w);
  float uVisibility = Visibility_ModifiedPCSS(osgShadow_umbraDepthTexture, uTexCoord, uCoverageCircle);
  float pVisibility = Visibility_PCF(osgShadow_penumbraDepthTexture, pTexCoord);
  
  float minVisibility = osgShadow_ambientBias.x;
  float shadowedVisibility = 0.5 * (uVisibility + pVisibility) * osgShadow_ambientBias.y;
  gl_FragColor = color * (minVisibility + shadowedVisibility);
  
#endif
}
