// Fragment shader when there is a base texture
//
uniform sampler2D osgShadow_baseTexture;
uniform sampler2DShadow osgShadow_penumbraDepthTexture;
uniform sampler2D osgShadow_penumbraColorTexture;
uniform sampler2DShadow osgShadow_umbraDepthTexture;
uniform vec2 osgShadow_ambientBias;
uniform float osgShadow_umbraDistance;  // Distance from umbral focal point to center of shadowing body
uniform float osgShadow_lightDistance;  // Distance from shadowing body center to light center
uniform float osgShadow_sizeRatio;      // Ratio of shadowing body radius to light radius

vec4 pTexCoord = gl_TexCoord[1];
vec4 uTexCoord = gl_TexCoord[2];

float chebyshevUpperBound()
{
  vec2 momentsPenumbra = texture2D(osgShadow_penumbraColorTexture, pTexCoord.xy).rg;
  
  // Surface is fully lit since the current fragment is before the light occluder
  if ((momentsPenumbra.r == 1.0) || (pTexCoord.z <= momentsPenumbra.r))
    return 1.0;
  
  // The fragment is either in shadow or penumbra. We now use chebyshev's upperBound to check
  // How likely this pixel is to be lit (p_max)
  float variance = momentsPenumbra.g - (momentsPenumbra.r*momentsPenumbra.r);
  variance = max(variance,0.00002);
  
  float d = pTexCoord.z - momentsPenumbra.r;
  float p_max = variance / (variance + d*d);
  
  return p_max;
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
  float pVisibility = shadow2D(osgShadow_penumbraDepthTexture, pTexCoord.xyz).r;
  float uVisibility = shadow2D(osgShadow_umbraDepthTexture, uTexCoord.xyz).r;
  //float pTextureDepth = texture2D(osgShadow_penumbraColorTexture, pTexCoord.xy).r;
  //float pVisibilityFromColor = ((pTextureDepth < 1.0) && (pTextureDepth < pTexCoord.z)) ? 0.0 : 1.0;
  float pVisibilityFromColor = chebyshevUpperBound();
  
  // Compute fragment color
  float minVisibility = osgShadow_ambientBias.x + lightMinVisibility * osgShadow_ambientBias.y;
  float shadowedVisibility = (1.0 - lightMinVisibility) * 0.5 * (pVisibilityFromColor + uVisibility) * osgShadow_ambientBias.y;
  gl_FragColor = color * (minVisibility + shadowedVisibility);
  //gl_FragColor = color * (1.0 - pVisibilityFromColor);
}
