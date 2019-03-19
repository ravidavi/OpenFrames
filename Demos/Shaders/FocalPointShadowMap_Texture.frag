// Fragment shader when there is a base texture
//
uniform sampler2D osgShadow_baseTexture;
uniform sampler2DShadow osgShadow_penumbraDepthTexture;
uniform sampler2DShadow osgShadow_umbraDepthTexture;
uniform vec2 osgShadow_ambientBias;
uniform float osgShadow_umbraDistance;  // Distance from umbral focal point to center of shadowing body
uniform float osgShadow_lightDistance;  // Distance from shadowing body center to light center
uniform float osgShadow_sizeRatio;      // Ratio of shadowing body radius to light radius
uniform vec2 osgShadow_texelSize;       // Size of each texel in normalized device coordinates (NDC)

vec4 pTexCoord = gl_TexCoord[1];
vec4 uTexCoord = gl_TexCoord[2];

float uVisibility_PCF()
{
  const int maxPointID = 2;
  const float numPoints = 2.0*float(maxPointID) + 1.0;
  float uVisibility = 0.0;
  for(int x = -maxPointID; x <= maxPointID; ++x){
    for(int y = -maxPointID; y <= maxPointID; ++y){
      vec2 off = 2.0*vec2(x,y) * osgShadow_texelSize;
      uVisibility += shadow2D(osgShadow_umbraDepthTexture, uTexCoord.xyz + vec3(off, 0.0)).r;
    }
  }
  return uVisibility / (numPoints * numPoints);
}

float pVisibility_PCF()
{
  const int maxPointID = 2;
  const float numPoints = 2.0*float(maxPointID) + 1.0;
  float pVisibility = 0.0;
  for(int x = -maxPointID; x <= maxPointID; ++x){
    for(int y = -maxPointID; y <= maxPointID; ++y){
      vec2 off = 2.0*vec2(x,y) * osgShadow_texelSize;
      pVisibility += shadow2D(osgShadow_penumbraDepthTexture, pTexCoord.xyz + vec3(off, 0.0)).r;
    }
  }
  return pVisibility / (numPoints * numPoints);
}

void main(void)
{
  // Get base fragment color, which will be attenuated based on visible light
  vec4 color = gl_Color * texture2D(osgShadow_baseTexture, gl_TexCoord[0].xy);
  
  // TexCoords are in range [0, w], so recover clip-space coordinates in range [-1, 1]
  pTexCoord.xyz = pTexCoord.xyz / pTexCoord.w;
  uTexCoord.xyz = uTexCoord.xyz / uTexCoord.w;
  vec2 pClipCoord = pTexCoord.xy * 2.0 - 1.0;
  vec2 uClipCoord = uTexCoord.xy * 2.0 - 1.0;
  float pRatio = length(pClipCoord);  // Assumes circular light due to implicit divide by 1
  float uRatio = length(uClipCoord);  // TODO: For square light divide by distance to square edge
  
  // Compute amount of guaranteed visible light in umbra/antumbra
  // This is always zero in umbra, and asymptotically increases with distance in antumbra
  float uDistance = max(-uTexCoord.w, 0.0);  // z-distance from fragment to umbra focal point
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
  float shadowedVisibility = (1.0 - lightMinVisibility) * 0.5 * (pVisibility + uVisibility) * osgShadow_ambientBias.y;
  gl_FragColor = color * (minVisibility + shadowedVisibility);
}
