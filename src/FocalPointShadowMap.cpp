/***********************************
 Copyright 2019 Ravishankar Mathur
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 ***********************************/

/** \file FocalPointShadowMap.hpp
 * Definitions of the FocalPointShadowMap class.
 */

#include <OpenFrames/FocalPointShadowMap.hpp>
#include <osgShadow/ShadowedScene>
#include <osg/Notify>
#include <osg/ComputeBoundsVisitor>
#include <osg/PolygonOffset>
#include <osg/CullFace>
#include <osg/io_utils>

#include <iostream>
#include <osg/LightSource>
#include <osg/PolygonMode>
#include <osg/Geometry>
#include <osg/Depth>
#include <osgDB/ReadFile>
#include <osgText/Text>

//////////////////////////////////////////////////////////////////
// fragment shader when there is no base texture
//
static const char fragmentShaderSource_noBaseTexture[] =
    "uniform sampler2DShadow osgShadow_penumbraTexture; \n"
    "uniform sampler2DShadow osgShadow_umbraTexture; \n"
    "uniform vec2 osgShadow_ambientBias; \n"
    "\n"
    "void main(void) \n"
    "{ \n"
    "    gl_FragColor = gl_Color * (osgShadow_ambientBias.x + shadow2DProj( osgShadow_penumbraTexture, gl_TexCoord[0] ) * osgShadow_ambientBias.y); \n"
    "}\n";

//////////////////////////////////////////////////////////////////
// fragment shader when there is a base texture
//
static const char fragmentShaderSource_withBaseTexture[] =
"uniform sampler2D osgShadow_baseTexture; \n"
"uniform sampler2DShadow osgShadow_penumbraTexture; \n"
"uniform sampler2DShadow osgShadow_umbraTexture; \n"
"uniform vec2 osgShadow_ambientBias; \n"
"uniform float osgShadow_umbraDistance; \n"
"uniform float osgShadow_lightDistance; \n"
"uniform float osgShadow_sizeRatio; \n"
"\n"
"void main(void) \n"
"{ \n"
"   vec4 color = gl_Color * texture2D( osgShadow_baseTexture, gl_TexCoord[0].xy ); \n"

//  Step 1: Determine the minimum amount of light this fragment is guaranteed to see
"   float minVisibility = 1.0; \n"
"   vec4 pTexCoord = gl_TexCoord[1]; \n"
"   vec4 uTexCoord = gl_TexCoord[2]; \n"

//  -- If fragment is outside penumbra then it sees all of the light
"   if((pTexCoord.w <= 0.0) || \n" // Behind penumbral focal point
"      any(lessThanEqual(pTexCoord.xy, vec2(0.0))) || \n" // Outside penumbral left/bottom bounds
"      any(greaterThanEqual(pTexCoord.xy, vec2(pTexCoord.w)))) \n" // Outside penumbra; right/top bounds
"   { \n"
"     minVisibility = 1.0; \n"
"   } \n"

//  -- If fragment is inside penumbra then determine how much light it is guaranteed to see
//  -- based on whether it is also in umbra or antumbra
"   else \n"
"   { \n"
//    TexCoords are in range [0, w], so recover clip-space coordinates in range [-1, 1]
"     vec2 pClipCoord = (pTexCoord.xy / pTexCoord.w) * 2.0 - 1.0; \n"
"     vec2 uClipCoord = (uTexCoord.xy / uTexCoord.w) * 2.0 - 1.0; \n"
"     float pRatio = length(pClipCoord); \n" // Assumes circular light (implicit divide by 1)
"     float uRatio = length(uClipCoord); \n"

//    Compute amount of guaranteed visible light in umbra/antumbra
"     float uBlockedLight = 1.0; \n" // No guaranteed visible light in umbra
"     if(uTexCoord.w < 0.0) uBlockedLight = osgShadow_sizeRatio * (-uTexCoord.w + osgShadow_umbraDistance + osgShadow_lightDistance) / (-uTexCoord.w + osgShadow_umbraDistance); \n"
"     float uMinVisibility = 1.0 - uBlockedLight*uBlockedLight; \n"

"     if((uRatio < 1.0) || (pRatio >= uRatio)) minVisibility = uMinVisibility; \n"
"     else \n"
"     { \n"
"       minVisibility = pRatio * (uRatio - 1.0) / (uRatio - pRatio); \n"
"       minVisibility = uMinVisibility + minVisibility*(1.0 - uMinVisibility); \n"
"     } \n"

//    In antumbra the texture lookup is reversed, so reverse texcoords preemptively
"     if(uTexCoord.w < 0.0) uTexCoord.xyz = -uTexCoord.xyz + vec3(uTexCoord.w, uTexCoord.w, 0.0); \n"
"   } \n"

//  Step 2: Compute umbral and penumbral visibility
"   vec4 pVisibility = shadow2DProj(osgShadow_penumbraTexture, pTexCoord); \n"
"   vec4 uVisibility = shadow2DProj(osgShadow_umbraTexture, uTexCoord); \n"

//"   pVisibility.gb = vec2(0.0); \n"
//"   uVisibility.rg = vec2(0.0); \n"

//  Step 3: Compute fragment color
"   gl_FragColor = color * (osgShadow_ambientBias.x + minVisibility * osgShadow_ambientBias.y); \n"
"   vec4 dynamicVisibility = 0.5 * (pVisibility + uVisibility); \n"
"   gl_FragColor += color * (1.0 - minVisibility) * dynamicVisibility * osgShadow_ambientBias.y; \n"
"}\n";

//////////////////////////////////////////////////////////////////
// fragment shader
//
static const char fragmentShaderSource_debugHUD[] =
    "uniform sampler2D osgShadow_shadowTexture; \n"
    " \n"
    "void main(void) \n"
    "{ \n"
    "   vec4 texResult = texture2D(osgShadow_shadowTexture, gl_TexCoord[0].st ); \n"
    "   float value = texResult.r; \n"
    "   gl_FragColor = vec4( value, value, value, 0.8 ); \n"
    "} \n";

namespace OpenFrames
{
  
  FocalPointShadowMap::FocalPointShadowMap():
  _polyOffset(1.0,1.0),
  _ambientBias(0.5f,0.5f),
  _baseTextureUnit(0),
  _lightSize(0.0)
  {
  }
  
  FocalPointShadowMap::FocalPointShadowMap(const FocalPointShadowMap& copy, const osg::CopyOp& copyop):
  ShadowTechnique(copy,copyop),
  _polyOffset(copy._polyOffset),
  _ambientBias(copy._ambientBias),
  _baseTextureUnit(copy._baseTextureUnit),
  _lightSize(copy._lightSize)
  {
  }
  
  void FocalPointShadowMap::resizeGLObjectBuffers(unsigned int maxSize)
  {
    osg::resizeGLObjectBuffers(_cameraPenumbra, maxSize);
    osg::resizeGLObjectBuffers(_texgenPenumbra, maxSize);
    osg::resizeGLObjectBuffers(_texturePenumbra, maxSize);
    osg::resizeGLObjectBuffers(_cameraUmbra, maxSize);
    osg::resizeGLObjectBuffers(_texgenUmbra, maxSize);
    osg::resizeGLObjectBuffers(_textureUmbra, maxSize);
    osg::resizeGLObjectBuffers(_stateset, maxSize);
    osg::resizeGLObjectBuffers(_program, maxSize);
    
    for(ShaderList::iterator itr = _shaderList.begin();
        itr != _shaderList.end();
        ++itr)
    {
      osg::resizeGLObjectBuffers(*itr, maxSize);
    }
  }
  
  void FocalPointShadowMap::releaseGLObjects(osg::State* state) const
  {
    osg::releaseGLObjects(_cameraPenumbra, state);
    osg::releaseGLObjects(_texgenPenumbra, state);
    osg::releaseGLObjects(_texturePenumbra, state);
    osg::releaseGLObjects(_cameraUmbra, state);
    osg::releaseGLObjects(_texgenUmbra, state);
    osg::releaseGLObjects(_textureUmbra, state);
    osg::releaseGLObjects(_stateset, state);
    osg::releaseGLObjects(_program, state);
    
    for(ShaderList::const_iterator itr = _shaderList.begin();
        itr != _shaderList.end();
        ++itr)
    {
      osg::releaseGLObjects(*itr, state);
    }
  }
  
  void FocalPointShadowMap::setAmbientBias(const osg::Vec2& ambientBias)
  {
    _ambientBias = ambientBias;
    if (_ambientBiasUniform.valid()) _ambientBiasUniform->set(_ambientBias);
  }
  
  void FocalPointShadowMap::createUniforms()
  {
    _uniformList.clear();
    
    osg::Uniform* baseTextureSampler = new osg::Uniform("osgShadow_baseTexture",(int)_baseTextureUnit);
    _uniformList.push_back(baseTextureSampler);
    
    unsigned int baseShadowTextureUnit = _shadowedScene->getShadowSettings()->getBaseShadowTextureUnit();
    osg::Uniform* penumbraTextureSampler = new osg::Uniform("osgShadow_penumbraTexture", (int)baseShadowTextureUnit);
    osg::Uniform* umbraTextureSampler = new osg::Uniform("osgShadow_umbraTexture", (int)(baseShadowTextureUnit+1));
    _uniformList.push_back(penumbraTextureSampler);
    _uniformList.push_back(umbraTextureSampler);
    
    _ambientBiasUniform = new osg::Uniform("osgShadow_ambientBias",_ambientBias);
    _uniformList.push_back(_ambientBiasUniform.get());
    
    _umbraDistanceUniform = new osg::Uniform(osg::Uniform::FLOAT, "osgShadow_umbraDistance");
    _uniformList.push_back(_umbraDistanceUniform.get());

    _lightDistanceUniform = new osg::Uniform(osg::Uniform::FLOAT, "osgShadow_lightDistance");
    _uniformList.push_back(_lightDistanceUniform.get());

    _sizeRatioUniform = new osg::Uniform(osg::Uniform::FLOAT, "osgShadow_sizeRatio");
    _uniformList.push_back(_sizeRatioUniform.get());
  }
  
  void FocalPointShadowMap::createShaders()
  {
    // if we are not given shaders, use the default
    if( _shaderList.empty() )
    {
      if (_shadowedScene->getShadowSettings()->getBaseShadowTextureUnit()==0)
      {
        std::cout<< "Using fragmentShaderSource_noBaseTexture" << std::endl;
        osg::Shader* fragment_shader = new osg::Shader(osg::Shader::FRAGMENT, fragmentShaderSource_noBaseTexture);
        _shaderList.push_back(fragment_shader);
      }
      else
      {
        std::cout<< "Using fragmentShaderSource_withBaseTexture" << std::endl;
        osg::Shader* fragment_shader = new osg::Shader(osg::Shader::FRAGMENT, fragmentShaderSource_withBaseTexture);
        _shaderList.push_back(fragment_shader);
      }
    }
  }
  
  void FocalPointShadowMap::init()
  {
    if (!_shadowedScene) return;
    
    const osg::Vec2s& textureSize = _shadowedScene->getShadowSettings()->getTextureSize();
    unsigned int baseShadowTextureUnit = _shadowedScene->getShadowSettings()->getBaseShadowTextureUnit();
    
    // Set up shadow map textures
    {
      _texturePenumbra = new osg::Texture2D;
      _texturePenumbra->setTextureSize(textureSize.x(), textureSize.y());
      _texturePenumbra->setInternalFormat(GL_DEPTH_COMPONENT);
      _texturePenumbra->setShadowComparison(true);
      _texturePenumbra->setShadowCompareFunc(osg::Texture::LEQUAL); // Penumbra wants to know closest depth
      _texturePenumbra->setShadowTextureMode(osg::Texture2D::LUMINANCE);
      _texturePenumbra->setFilter(osg::Texture2D::MIN_FILTER,osg::Texture2D::LINEAR);
      _texturePenumbra->setFilter(osg::Texture2D::MAG_FILTER,osg::Texture2D::LINEAR);
      
      // the shadow comparison should fail if object is outside the texture
      _texturePenumbra->setWrap(osg::Texture2D::WRAP_S,osg::Texture2D::CLAMP_TO_BORDER);
      _texturePenumbra->setWrap(osg::Texture2D::WRAP_T,osg::Texture2D::CLAMP_TO_BORDER);
      _texturePenumbra->setBorderColor(osg::Vec4(1.0f,1.0f,1.0f,1.0f));
      
      _textureUmbra = new osg::Texture2D;
      _textureUmbra->setTextureSize(textureSize.x(), textureSize.y());
      _textureUmbra->setInternalFormat(GL_DEPTH_COMPONENT);
      _textureUmbra->setShadowComparison(true);
      _textureUmbra->setShadowCompareFunc(osg::Texture::GEQUAL); // Umbra wants to know farthest depth
      _textureUmbra->setShadowTextureMode(osg::Texture2D::LUMINANCE);
      _textureUmbra->setFilter(osg::Texture2D::MIN_FILTER,osg::Texture2D::LINEAR);
      _textureUmbra->setFilter(osg::Texture2D::MAG_FILTER,osg::Texture2D::LINEAR);
      
      // the shadow comparison should fail if object is outside the texture
      _textureUmbra->setWrap(osg::Texture2D::WRAP_S,osg::Texture2D::CLAMP_TO_BORDER);
      _textureUmbra->setWrap(osg::Texture2D::WRAP_T,osg::Texture2D::CLAMP_TO_BORDER);
      _textureUmbra->setBorderColor(osg::Vec4(0.0f,0.0f,0.0f,0.0f));
    }
    
    // set up the penumbra render to texture camera
    {
      // create the penumbra camera
      _cameraPenumbra = new osg::Camera;
      _cameraPenumbra->setReferenceFrame(osg::Camera::ABSOLUTE_RF_INHERIT_VIEWPOINT);
      _cameraPenumbra->setCullCallback(new CameraCullCallback(this));
      _cameraPenumbra->setClearMask(GL_DEPTH_BUFFER_BIT);
      _cameraPenumbra->setClearColor(osg::Vec4(1.0f,1.0f,1.0f,1.0f));
      _cameraPenumbra->setClearDepth(1.0); // Penumbra wants to know closest depth, so initialize to farthest depth
      _cameraPenumbra->setComputeNearFarMode(osg::Camera::DO_NOT_COMPUTE_NEAR_FAR);
      _cameraPenumbra->setViewport(0, 0, textureSize.x(), textureSize.y());
      _cameraPenumbra->setRenderOrder(osg::Camera::PRE_RENDER); // Render before main camera
      _cameraPenumbra->setRenderTargetImplementation(osg::Camera::FRAME_BUFFER_OBJECT); // Use OpenGL FBO
      _cameraPenumbra->attach(osg::Camera::DEPTH_BUFFER, _texturePenumbra.get()); // Use texture as depth buffer
      
      osg::StateSet* stateset = _cameraPenumbra->getOrCreateStateSet();
      
      // cull front faces so that only backfaces contribute to depth map
      osg::ref_ptr<osg::CullFace> cull_face = new osg::CullFace;
      cull_face->setMode(osg::CullFace::FRONT);
      stateset->setAttribute(cull_face.get(), osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
      stateset->setMode(GL_CULL_FACE, osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
      
      // negative polygonoffset - move the backface nearer to the eye point so that backfaces
      // shadow themselves
      float factor = -_polyOffset[0];
      float units =  -_polyOffset[1];
      osg::ref_ptr<osg::PolygonOffset> polygon_offset = new osg::PolygonOffset;
      polygon_offset->setFactor(factor);
      polygon_offset->setUnits(units);
      stateset->setAttribute(polygon_offset.get(), osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
      stateset->setMode(GL_POLYGON_OFFSET_FILL, osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
    }
    
    // set up the umbra render to texture camera
    {
      // create the umbra camera
      _cameraUmbra = new osg::Camera;
      _cameraUmbra->setReferenceFrame(osg::Camera::ABSOLUTE_RF_INHERIT_VIEWPOINT);
      _cameraUmbra->setCullCallback(new CameraCullCallback(this));
      _cameraUmbra->setClearMask(GL_DEPTH_BUFFER_BIT);
      _cameraUmbra->setClearColor(osg::Vec4(1.0f,1.0f,1.0f,1.0f));
      _cameraUmbra->setClearDepth(0.0); // Umbra wants to know farthest depth, so initialize to closest depth
      _cameraUmbra->setComputeNearFarMode(osg::Camera::DO_NOT_COMPUTE_NEAR_FAR);
      _cameraUmbra->setViewport(0, 0, textureSize.x(), textureSize.y());
      _cameraUmbra->setRenderOrder(osg::Camera::PRE_RENDER); // Render before main camera
      _cameraUmbra->setRenderTargetImplementation(osg::Camera::FRAME_BUFFER_OBJECT); // Use OpenGL FBO
      _cameraUmbra->attach(osg::Camera::DEPTH_BUFFER, _textureUmbra.get()); // Use texture as depth buffer
      
      osg::StateSet* stateset = _cameraUmbra->getOrCreateStateSet();
      
      // Reverse the depth test since umbra camera faces towards light source
      osg::ref_ptr<osg::Depth> depth = new osg::Depth;
      depth->setFunction(osg::Depth::GREATER);
      stateset->setAttribute(depth.get(), osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
      
      // cull back faces so that only front faces contribute to depth map
      osg::ref_ptr<osg::CullFace> cull_face = new osg::CullFace;
      cull_face->setMode(osg::CullFace::BACK);
      stateset->setAttribute(cull_face.get(), osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
      stateset->setMode(GL_CULL_FACE, osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
      
      // positive polygonoffset - move the front face farther from the eye point so that they
      // shadow themselves
      float factor = _polyOffset[0];
      float units =  _polyOffset[1];
      osg::ref_ptr<osg::PolygonOffset> polygon_offset = new osg::PolygonOffset;
      polygon_offset->setFactor(factor);
      polygon_offset->setUnits(units);
      stateset->setAttribute(polygon_offset.get(), osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
      stateset->setMode(GL_POLYGON_OFFSET_FILL, osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
    }
    
    {
      _stateset = new osg::StateSet;
      
      _stateset->setTextureAttributeAndModes(baseShadowTextureUnit, _texturePenumbra.get(),osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
      _stateset->setTextureMode(baseShadowTextureUnit, GL_TEXTURE_GEN_S, osg::StateAttribute::ON);
      _stateset->setTextureMode(baseShadowTextureUnit, GL_TEXTURE_GEN_T, osg::StateAttribute::ON);
      _stateset->setTextureMode(baseShadowTextureUnit, GL_TEXTURE_GEN_R, osg::StateAttribute::ON);
      _stateset->setTextureMode(baseShadowTextureUnit, GL_TEXTURE_GEN_Q, osg::StateAttribute::ON);
      
      _stateset->setTextureAttributeAndModes(baseShadowTextureUnit+1, _textureUmbra.get(),osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
      _stateset->setTextureMode(baseShadowTextureUnit+1, GL_TEXTURE_GEN_S, osg::StateAttribute::ON);
      _stateset->setTextureMode(baseShadowTextureUnit+1, GL_TEXTURE_GEN_T, osg::StateAttribute::ON);
      _stateset->setTextureMode(baseShadowTextureUnit+1, GL_TEXTURE_GEN_R, osg::StateAttribute::ON);
      _stateset->setTextureMode(baseShadowTextureUnit+1, GL_TEXTURE_GEN_Q, osg::StateAttribute::ON);
      
      // Let the fixed-function OpenGL generate texture coordinates
      _texgenPenumbra = new osg::TexGen;
      _texgenPenumbra->setMode(osg::TexGen::EYE_LINEAR);
      
      _texgenUmbra = new osg::TexGen;
      _texgenUmbra->setMode(osg::TexGen::EYE_LINEAR);
      
      // Add Program, when empty of Shaders then we are using fixed functionality
      _program = new osg::Program;
      _stateset->setAttribute(_program.get());
      
      // create default shaders if needed
      createShaders();
      
      // add the shader list to the program
      for(ShaderList::const_iterator itr=_shaderList.begin();
          itr!=_shaderList.end();
          ++itr)
      {
        _program->addShader(itr->get());
      }
      
      // create own uniforms
      createUniforms();
      
      // add the uniform list to the stateset
      for(UniformList::const_iterator itr=_uniformList.begin();
          itr!=_uniformList.end();
          ++itr)
      {
        _stateset->addUniform(itr->get());
      }
      
      {
        // Add fake texture for base texture layer, so that we can support
        // both textured and untextured scenes
        // TODO: at the moment the FPSM supports just one texture layer in the GLSL shader
        //       multitexture is not yet supported!
        
        osg::Image* image = new osg::Image;
        
        // allocate the image data, numPixels x 1 x 1 with 4 rgba floats - equivalent to a Vec4!
        int numPixels = 1;
        image->allocateImage(numPixels, 1, 1, GL_RGBA, GL_FLOAT);
        image->setInternalTextureFormat(GL_RGBA);
        
        // fill in the image data.
        osg::Vec4* dataPtr = (osg::Vec4*)image->data();
        osg::Vec4 color(1,1,1,1);
        *dataPtr = color;
        
        // make fake texture
        osg::Texture2D* fakeTex = new osg::Texture2D;
        fakeTex->setWrap(osg::Texture2D::WRAP_S,osg::Texture2D::CLAMP_TO_EDGE);
        fakeTex->setWrap(osg::Texture2D::WRAP_T,osg::Texture2D::CLAMP_TO_EDGE);
        fakeTex->setFilter(osg::Texture2D::MIN_FILTER,osg::Texture2D::LINEAR);
        fakeTex->setFilter(osg::Texture2D::MAG_FILTER,osg::Texture2D::LINEAR);
        fakeTex->setImage(image);
        
        // add fake texture
        _stateset->setTextureAttribute(_baseTextureUnit, fakeTex, osg::StateAttribute::ON);
        _stateset->setTextureMode(_baseTextureUnit, GL_TEXTURE_2D, osg::StateAttribute::ON);
        _stateset->setTextureMode(_baseTextureUnit, GL_TEXTURE_3D, osg::StateAttribute::OFF);
        
#if !defined(OSG_GLES1_AVAILABLE) && !defined(OSG_GLES2_AVAILABLE) && !defined(OSG_GLES3_AVAILABLE)
        _stateset->setTextureMode(_baseTextureUnit, GL_TEXTURE_1D, osg::StateAttribute::OFF);
#endif
      }
    }
    
    _dirty = false;
  }
  
  void FocalPointShadowMap::update(osg::NodeVisitor& nv)
  {
    _shadowedScene->osg::Group::traverse(nv);
  }
  
  void FocalPointShadowMap::cull(osgUtil::CullVisitor& cv)
  {
    // Record the traversal mask on entry so we can reapply it later
    unsigned int traversalMask = cv.getTraversalMask();
    
    osgUtil::RenderStage* orig_rs = cv.getRenderStage();
    
    // Traverse shadow receiving scene, which needs to be decorated by the shadow map
    {
      cv.pushStateSet(_stateset.get());
      
      _shadowedScene->osg::Group::traverse(cv);
      
      cv.popStateSet();
    }
    
    // Compute view frustum for RTT camera
    // 1) get the light position
    // 2) get the center and extents of the view frustum
    
    const osg::Light* selectedLight = 0;
    osg::Vec4d lightPos;
    
    //MR testing giving a specific light
    int desiredLightNum = _shadowedScene->getShadowSettings()->getLightNum();
    osgUtil::PositionalStateContainer::AttrMatrixList& aml = orig_rs->getPositionalStateContainer()->getAttrMatrixList();
    for(osgUtil::PositionalStateContainer::AttrMatrixList::iterator itr = aml.begin();
        itr != aml.end();
        ++itr)
    {
      const osg::Light* light = dynamic_cast<const osg::Light*>(itr->first.get());
      if (light)
      {
        // Check if desired light has been specified
        if(desiredLightNum >= 0)
        {
          // Check if found light matches desired light
          if(desiredLightNum == light->getLightNum())
            selectedLight = light;
          else
            continue;
        }
        else // Otherwise choose first found light
          selectedLight = light;
        
        osg::RefMatrix* matrix = itr->second.get();
        if (matrix)
        {
          lightPos = light->getPosition() * (*matrix);
        }
        else
        {
          lightPos = light->getPosition();
        }
      }
    }
    
    osg::Matrix eyeToWorld;
    eyeToWorld.invert(*cv.getModelViewMatrix());
    lightPos = lightPos * eyeToWorld;
    
    if (selectedLight)
    {
      // get the bounds of the model.
      osg::ComputeBoundsVisitor cbbv(osg::NodeVisitor::TRAVERSE_ACTIVE_CHILDREN);
      cbbv.setTraversalMask(getShadowedScene()->getCastsShadowTraversalMask());
      
      _shadowedScene->osg::Group::traverse(cbbv);
      
      osg::BoundingBox bb = cbbv.getBoundingBox();
      
      if (lightPos[3]!=0.0)   // point light
      {
        osg::Vec3d lightPos3(lightPos.x(), lightPos.y(), lightPos.z());
        osg::Vec3d lightToCenter = bb.center() - lightPos3;
        double lightDistance = lightToCenter.normalize();
        
        double penumbraDistance = lightDistance / (_lightSize/bb.radius() + 1.0);
        double penumbraZNear = penumbraDistance - bb.radius();
        double penumbraZFar  = penumbraDistance + bb.radius();
        double penumbraFOV = 2.0*std::asin(bb.radius() / penumbraDistance);

        _cameraPenumbra->setViewMatrixAsLookAt(bb.center() - (lightToCenter * penumbraDistance), bb.center(), computeOrthogonalVector(lightToCenter));
        _cameraPenumbra->setProjectionMatrixAsPerspective(penumbraFOV * 180.0/osg::PI, 1.0, penumbraZNear, penumbraZFar);
        
        double umbraDistance = lightDistance / (_lightSize/bb.radius() - 1.0);
        double umbraZNear = umbraDistance - bb.radius();
        double umbraZFar  = umbraDistance + bb.radius();
        double umbraFOV = 2.0*std::asin(bb.radius() / umbraDistance);
        
        _lightDistanceUniform->set((float)lightDistance);
        _umbraDistanceUniform->set((float)umbraDistance);
        _sizeRatioUniform->set((float)(bb.radius()/_lightSize));

        _cameraUmbra->setViewMatrixAsLookAt(bb.center() + (lightToCenter * umbraDistance), bb.center(), computeOrthogonalVector(-lightToCenter));
        _cameraUmbra->setProjectionMatrixAsPerspective(umbraFOV * 180.0/osg::PI, 1.0, umbraZNear, umbraZFar);
        
        //std::cout<< "lightDistance = " << lightDistance << ", penumbraDistance = " << penumbraDistance << ", bb radius = " << bb.radius() << ", Penumbra fov (deg) = " << penumbraFOV * 180.0/osg::PI << std::endl;

        //std::cout<< "lightDistance = " << lightDistance << ", umbraDistance = " << umbraDistance << ", bb radius = " << bb.radius() << ", Umbra fov (deg) = " << umbraFOV * 180.0/osg::PI << std::endl;

      }
      else    // directional light
      {
        // make an orthographic projection
        osg::Vec3d ortho_lightDir(lightPos.x(), lightPos.y(), lightPos.z());
        ortho_lightDir.normalize();
        
        // set the position far away along the light direction
        double centerDistance = bb.radius();
        osg::Vec3d position = bb.center() + ortho_lightDir * centerDistance;
        
        double znear = centerDistance-bb.radius();
        double zfar  = centerDistance+bb.radius();
        //double zNearRatio = 0.001;
        //if (znear<zfar*zNearRatio) znear = zfar*zNearRatio;
        
        double top   = bb.radius();
        double right = top;
        
        //std::cout<< "centerDistance = " << centerDistance << ", bb radius = " << bb.radius() << std::endl;
        
        _cameraPenumbra->setProjectionMatrixAsOrtho(-right, right, -top, top, znear, zfar);
        _cameraPenumbra->setViewMatrixAsLookAt(position,bb.center(),computeOrthogonalVector(ortho_lightDir));
      }
      
      unsigned int baseShadowTextureUnit = _shadowedScene->getShadowSettings()->getBaseShadowTextureUnit();
      cv.setTraversalMask( traversalMask & _shadowedScene->getCastsShadowTraversalMask() );
      
      // RTT traversal for penumbra camera
      {
        _cameraPenumbra->accept(cv);
        
        // compute the matrix which takes a vertex from local coords into tex coords
        // We actually use two matrices one used to define texgen
        // and second that will be used as modelview when appling to OpenGL
        _texgenPenumbra->setPlanesFromMatrix(_cameraPenumbra->getProjectionMatrix() *
                                     osg::Matrix::translate(1.0, 1.0, 1.0) *
                                     osg::Matrix::scale(0.5, 0.5, 0.5) );
        
        // Place texgen with modelview which removes big offsets (making it float friendly)
        osg::RefMatrix * refMatrix = new osg::RefMatrix(_cameraPenumbra->getInverseViewMatrix() * *cv.getModelViewMatrix() );
        cv.getRenderStage()->getPositionalStateContainer()->addPositionedTextureAttribute(baseShadowTextureUnit, refMatrix, _texgenPenumbra.get() );
      }
      
      // RTT traversal for umbra camera
      {
        _cameraUmbra->accept(cv);
        
        // compute the matrix which takes a vertex from local coords into tex coords
        // We actually use two matrices one used to define texgen
        // and second that will be used as modelview when appling to OpenGL
        _texgenUmbra->setPlanesFromMatrix(_cameraUmbra->getProjectionMatrix() *
                                          osg::Matrix::translate(1.0, 1.0, 1.0) *
                                          osg::Matrix::scale(0.5, 0.5, 0.5) );
        
        // Place texgen with modelview which removes big offsets (making it float friendly)
        osg::RefMatrix * refMatrix = new osg::RefMatrix(_cameraUmbra->getInverseViewMatrix() * *cv.getModelViewMatrix() );
        cv.getRenderStage()->getPositionalStateContainer()->addPositionedTextureAttribute(baseShadowTextureUnit+1, refMatrix, _texgenUmbra.get() );
      }
    }
    
    // reapply the original traversal mask
    cv.setTraversalMask( traversalMask );
  }
  
  void FocalPointShadowMap::cleanSceneGraph()
  {
  }
  
  ///////////////////// Debug Methods
  
  ////////////////////////////////////////////////////////////////////////////////
  // Callback used by debugging hud to display Shadow Map in color buffer
  // OSG does not allow to use the same GL Texture Id with different glTexParams.
  // Callback simply turns shadow compare mode off via GL while rendering hud and
  // restores it afterwards.
  ////////////////////////////////////////////////////////////////////////////////
  class FocalPointShadowMap::DrawableDrawWithDepthShadowComparisonOffCallback:
  public osg::Drawable::DrawCallback
  {
  public:
    //
    DrawableDrawWithDepthShadowComparisonOffCallback
    ( osg::Texture2D * texture, unsigned stage = 0 )
    : _texture( texture ), _stage( stage )
    {
    }
    
    virtual void drawImplementation
    ( osg::RenderInfo & ri,const osg::Drawable* drawable ) const
    {
      if( _texture.valid() ) {
        // make sure proper texture is currently applied
        ri.getState()->applyTextureAttribute( _stage, _texture.get() );
        
        // Turn off depth comparison mode
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB,
                        GL_NONE );
      }
      
      drawable->drawImplementation(ri);
      
      if( _texture.valid() ) {
        // Turn it back on
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB,
                        GL_COMPARE_R_TO_TEXTURE_ARB );
      }
    }
    
    osg::ref_ptr< osg::Texture2D > _texture;
    unsigned                       _stage;
  };
  
  ////////////////////////////////////////////////////////////////////////////////
  osg::ref_ptr<osg::Camera> FocalPointShadowMap::makeDebugHUD()
  {
    osg::Texture2D *displayTexture = _texturePenumbra;
    
    // Make sure we attach initialized texture to HUD
    if( displayTexture == nullptr )
    {
      init();
      displayTexture = _texturePenumbra;
    }
    
    osg::ref_ptr<osg::Camera> camera = new osg::Camera;
    
    const osg::Vec2s& size = _shadowedScene->getShadowSettings()->getTextureSize();

    // set the projection matrix
    camera->setProjectionMatrix(osg::Matrix::ortho2D(0,size.x(),0,size.y()));
    
    // set the view matrix
    camera->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
    camera->setViewMatrix(osg::Matrix::identity());
    
    // only clear the depth buffer
    camera->setClearMask(GL_DEPTH_BUFFER_BIT);
    camera->setClearColor(osg::Vec4(0.2f, 0.3f, 0.5f, 0.2f));
    
    // draw subgraph after main camera view.
    camera->setRenderOrder(osg::Camera::POST_RENDER);
    
    // we don't want the camera to grab event focus from the viewers main camera(s).
    camera->setAllowEventFocus(false);
    
    osg::Geode* geode = new osg::Geode;
    
    osg::Vec3 position(10.0f,size.y()-100.0f,0.0f);
    osg::Vec3 delta(0.0f,-120.0f,0.0f);
    float length = size.x() / 4.0;
    
    osg::Vec3 widthVec(length, 0.0f, 0.0f);
    osg::Vec3 depthVec(0.0f,length, 0.0f);
    osg::Vec3 centerBase( 10.0f + length/2, size.y()-length/2, 0.0f);
    centerBase += delta;
    
    osg::Geometry *geometry = osg::createTexturedQuadGeometry
    ( centerBase-widthVec*0.5f-depthVec*0.5f, widthVec, depthVec );
    
    geode->addDrawable( geometry );
    
    geometry->setDrawCallback
    ( new DrawableDrawWithDepthShadowComparisonOffCallback(displayTexture) );
    
    osg::StateSet* stateset = geode->getOrCreateStateSet();
    
    stateset->setMode(GL_LIGHTING,osg::StateAttribute::OFF);
    stateset->setMode(GL_BLEND,osg::StateAttribute::ON);
    stateset->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
    stateset->setTextureAttributeAndModes(0, displayTexture, osg::StateAttribute::ON);
    
    //shader for correct display
    osg::ref_ptr<osg::Program> program = new osg::Program;
    stateset->setAttribute(program.get());
    osg::Shader* fragment_shader = new osg::Shader(osg::Shader::FRAGMENT, fragmentShaderSource_debugHUD);
    program->addShader(fragment_shader);
    
    camera->addChild(geode);
    
    return camera;
  }
}

//////////////////////// End Debug Section

