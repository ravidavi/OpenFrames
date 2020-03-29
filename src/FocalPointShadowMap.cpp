/***********************************
 Copyright 2020 Ravishankar Mathur
 
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
#include <osg/ComputeBoundsVisitor>
#include <osg/PolygonOffset>
#include <osg/CullFace>
#include <osg/LightSource>
#include <osg/PolygonMode>
#include <osg/Geometry>
#include <osg/Depth>
#include <osgDB/ReadFile>
#include <osgShadow/ShadowedScene>

//////////////////////////////////////////////////////////////////
// fragment shader when there is no base texture
//
static const char fragmentShaderSource_noBaseTexture[] =
    "uniform sampler2DShadow osgShadow_penumbraDepthTexture; \n"
    "uniform sampler2DShadow osgShadow_umbraDepthTexture; \n"
    "uniform vec2 osgShadow_ambientBias; \n"
    "\n"
    "void main(void) \n"
    "{ \n"
    "    gl_FragColor = gl_Color * (osgShadow_ambientBias.x + shadow2DProj( osgShadow_penumbraDepthTexture, gl_TexCoord[0] ) * osgShadow_ambientBias.y); \n"
    "}\n";

//////////////////////////////////////////////////////////////////
// fragment shader for debug hud
//
static const char fragmentShaderSource_debugHUD[] =
    "uniform sampler2D osgShadow_shadowTexture; \n"
    " \n"
    "void main(void) \n"
    "{ \n"
    "   vec4 texResult = texture2D(osgShadow_shadowTexture, gl_TexCoord[0].st ); \n"
    "   float value = texResult.r; \n"
    "   gl_FragColor = vec4( value, value, value, 1.0 ); \n"
    "} \n";

namespace OpenFrames
{
  
  FocalPointShadowMap::FocalPointShadowMap():
  _polyOffset(1.0,1.0),
  _ambientBias(0.5f,0.5f),
  _baseTextureUnit(0),
  _lightSize(0.0),
  _sceneScale(1.0)
  {
  }
  
  FocalPointShadowMap::FocalPointShadowMap(const FocalPointShadowMap& copy, const osg::CopyOp& copyop):
  ShadowTechnique(copy,copyop),
  _polyOffset(copy._polyOffset),
  _ambientBias(copy._ambientBias),
  _baseTextureUnit(copy._baseTextureUnit),
  _lightSize(copy._lightSize),
  _sceneScale(copy._sceneScale)
  {
  }
  
  void FocalPointShadowMap::resizeGLObjectBuffers(unsigned int maxSize)
  {
    osg::resizeGLObjectBuffers(_cameraPenumbra, maxSize);
    osg::resizeGLObjectBuffers(_texgenPenumbra, maxSize);
    osg::resizeGLObjectBuffers(_texturePenumbraDepth, maxSize);
    osg::resizeGLObjectBuffers(_cameraUmbra, maxSize);
    osg::resizeGLObjectBuffers(_texgenUmbra, maxSize);
    osg::resizeGLObjectBuffers(_textureUmbraDepth, maxSize);
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
    osg::releaseGLObjects(_texturePenumbraDepth, state);
    osg::releaseGLObjects(_cameraUmbra, state);
    osg::releaseGLObjects(_texgenUmbra, state);
    osg::releaseGLObjects(_textureUmbraDepth, state);
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
    osg::Uniform* penumbraDepthTextureSampler = new osg::Uniform("osgShadow_penumbraDepthTexture", (int)baseShadowTextureUnit);
    osg::Uniform* umbraTextureSampler = new osg::Uniform("osgShadow_umbraDepthTexture", (int)(baseShadowTextureUnit+1));
    _uniformList.push_back(penumbraDepthTextureSampler);
    _uniformList.push_back(umbraTextureSampler);
    
    _ambientBiasUniform = new osg::Uniform("osgShadow_ambientBias", _ambientBias);
    _uniformList.push_back(_ambientBiasUniform.get());
    
    _umbraDistanceUniform = new osg::Uniform(osg::Uniform::FLOAT, "osgShadow_umbraDistance");
    _uniformList.push_back(_umbraDistanceUniform.get());
    
    _umbraZNearFarInvUniform = new osg::Uniform(osg::Uniform::FLOAT_VEC2, "osgShadow_umbraZNearFarInv");
    _uniformList.push_back(_umbraZNearFarInvUniform.get());
    
    _penumbraDistanceUniform = new osg::Uniform(osg::Uniform::FLOAT, "osgShadow_penumbraDistance");
    _uniformList.push_back(_penumbraDistanceUniform.get());
    
    _penumbraZNearFarInvUniform = new osg::Uniform(osg::Uniform::FLOAT_VEC2, "osgShadow_penumbraZNearFarInv");
    _uniformList.push_back(_penumbraZNearFarInvUniform.get());

    _lightDistanceUniform = new osg::Uniform(osg::Uniform::FLOAT, "osgShadow_lightDistance");
    _uniformList.push_back(_lightDistanceUniform.get());
    
    const osg::Vec2s& textureSize = _shadowedScene->getShadowSettings()->getTextureSize();
    _texelSizeUniform = new osg::Uniform("osgShadow_texelSize", osg::Vec2(1.0/textureSize.x(), 1.0/textureSize.y()));
    _uniformList.push_back(_texelSizeUniform.get());
  }
  
  void FocalPointShadowMap::createShaders()
  {
    // Shaders for shadow pass
    if(_shaderListShadowPass.empty())
    {
      osg::ref_ptr<osg::Shader> vertex_shader = osgDB::readRefShaderFile(osg::Shader::VERTEX, "Shaders/FocalPointShadowMap_Shadow.vert");
      if(vertex_shader.valid())
      {
        vertex_shader->setName("shadowpassvert");
        //_shaderListShadowPass.push_back(vertex_shader);
      }
      
      osg::ref_ptr<osg::Shader> fragment_shader = osgDB::readRefShaderFile(osg::Shader::FRAGMENT, "Shaders/FocalPointShadowMap_Shadow.frag");
      if(fragment_shader.valid())
      {
        fragment_shader->setName("shadowpassfrag");
        //_shaderListShadowPass.push_back(fragment_shader);
      }
    }
    
    // Shaders for rendering pass
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
        //osg::Shader* fragment_shader = new osg::Shader(osg::Shader::FRAGMENT, fragmentShaderSource_withBaseTexture);
        osg::ref_ptr<osg::Shader> fragment_shader = osgDB::readRefShaderFile(osg::Shader::FRAGMENT, "Shaders/FocalPointShadowMap_Texture.frag");
        if(fragment_shader.valid())
        {
          fragment_shader->setName("texturepassfrag");
          _shaderList.push_back(fragment_shader);
        }
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
      // Create penumbra depth texture
      _texturePenumbraDepth = new osg::Texture2D;
      _texturePenumbraDepth->setTextureSize(textureSize.x(), textureSize.y());
      _texturePenumbraDepth->setInternalFormat(GL_DEPTH_COMPONENT);
      _texturePenumbraDepth->setShadowCompareFunc(osg::Texture::LEQUAL); // Penumbra wants to know closest depth
      _texturePenumbraDepth->setFilter(osg::Texture2D::MIN_FILTER,osg::Texture2D::LINEAR);
      _texturePenumbraDepth->setFilter(osg::Texture2D::MAG_FILTER,osg::Texture2D::LINEAR);
      
      // The shadow depth comparison should pass (i.e. not shadowed) if point is outside the penumbra texture
      _texturePenumbraDepth->setWrap(osg::Texture2D::WRAP_S,osg::Texture2D::CLAMP_TO_BORDER);
      _texturePenumbraDepth->setWrap(osg::Texture2D::WRAP_T,osg::Texture2D::CLAMP_TO_BORDER);
      _texturePenumbraDepth->setBorderColor(osg::Vec4(1.0f,1.0f,1.0f,1.0f));
      
      // Create umbra depth texture
      _textureUmbraDepth = new osg::Texture2D;
      _textureUmbraDepth->setTextureSize(textureSize.x(), textureSize.y());
      _textureUmbraDepth->setInternalFormat(GL_DEPTH_COMPONENT);
      _textureUmbraDepth->setShadowCompareFunc(osg::Texture::GEQUAL); // Umbra wants to know farthest depth
      _textureUmbraDepth->setFilter(osg::Texture2D::MIN_FILTER,osg::Texture2D::LINEAR);
      _textureUmbraDepth->setFilter(osg::Texture2D::MAG_FILTER,osg::Texture2D::LINEAR);
      
      // The shadow depth comparison should pass (i.e. not shadowed) if point is outside the umbra texture
      _textureUmbraDepth->setWrap(osg::Texture2D::WRAP_S,osg::Texture2D::CLAMP_TO_BORDER);
      _textureUmbraDepth->setWrap(osg::Texture2D::WRAP_T,osg::Texture2D::CLAMP_TO_BORDER);
      _textureUmbraDepth->setBorderColor(osg::Vec4(0.0f,0.0f,0.0f,0.0f));
    }
    
    // set up the penumbra render to texture camera
    {
      // create the penumbra camera
      _cameraPenumbra = new osg::Camera;
      _cameraPenumbra->setReferenceFrame(osg::Camera::ABSOLUTE_RF_INHERIT_VIEWPOINT);
      _cameraPenumbra->setCullCallback(new CameraCullCallback(this));
      _cameraPenumbra->setClearMask(GL_DEPTH_BUFFER_BIT);
      _cameraPenumbra->setClearDepth(1.0); // Penumbra wants to know closest depth, so initialize to farthest depth
      _cameraPenumbra->setComputeNearFarMode(osg::Camera::DO_NOT_COMPUTE_NEAR_FAR);
      _cameraPenumbra->setViewport(0, 0, textureSize.x(), textureSize.y());
      _cameraPenumbra->setRenderOrder(osg::Camera::PRE_RENDER); // Render before main camera
      _cameraPenumbra->setRenderTargetImplementation(osg::Camera::FRAME_BUFFER_OBJECT); // Use OpenGL FBO
      _cameraPenumbra->attach(osg::Camera::DEPTH_BUFFER, _texturePenumbraDepth.get()); // Use texture as depth buffer
      
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
      _cameraUmbra->setClearDepth(0.0); // Umbra wants to know farthest depth, so initialize to closest depth
      _cameraUmbra->setComputeNearFarMode(osg::Camera::DO_NOT_COMPUTE_NEAR_FAR);
      _cameraUmbra->setViewport(0, 0, textureSize.x(), textureSize.y());
      _cameraUmbra->setRenderOrder(osg::Camera::PRE_RENDER); // Render before main camera
      _cameraUmbra->setRenderTargetImplementation(osg::Camera::FRAME_BUFFER_OBJECT); // Use OpenGL FBO
      _cameraUmbra->attach(osg::Camera::DEPTH_BUFFER, _textureUmbraDepth.get()); // Use texture as depth buffer
      
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
      // create default shaders if needed
      createShaders();
      
      osg::Program* programShadowPass = new osg::Program;
      _cameraPenumbra->getOrCreateStateSet()->setAttribute(programShadowPass);
      _cameraUmbra->getOrCreateStateSet()->setAttribute(programShadowPass);
      for(auto itr : _shaderListShadowPass)
      {
        programShadowPass->addShader(itr);
      }
      
      _stateset = new osg::StateSet;
      
      _stateset->setTextureAttributeAndModes(baseShadowTextureUnit, _texturePenumbraDepth.get(),osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
      _stateset->setTextureMode(baseShadowTextureUnit, GL_TEXTURE_GEN_S, osg::StateAttribute::ON);
      _stateset->setTextureMode(baseShadowTextureUnit, GL_TEXTURE_GEN_T, osg::StateAttribute::ON);
      _stateset->setTextureMode(baseShadowTextureUnit, GL_TEXTURE_GEN_R, osg::StateAttribute::ON);
      _stateset->setTextureMode(baseShadowTextureUnit, GL_TEXTURE_GEN_Q, osg::StateAttribute::ON);
      
      _stateset->setTextureAttributeAndModes(baseShadowTextureUnit+1, _textureUmbraDepth.get(),osg::StateAttribute::ON | osg::StateAttribute::OVERRIDE);
      _stateset->setTextureMode(baseShadowTextureUnit+1, GL_TEXTURE_GEN_S, osg::StateAttribute::ON);
      _stateset->setTextureMode(baseShadowTextureUnit+1, GL_TEXTURE_GEN_T, osg::StateAttribute::ON);
      _stateset->setTextureMode(baseShadowTextureUnit+1, GL_TEXTURE_GEN_R, osg::StateAttribute::ON);
      _stateset->setTextureMode(baseShadowTextureUnit+1, GL_TEXTURE_GEN_Q, osg::StateAttribute::ON);
      
      // Let the fixed-function OpenGL generate texture coordinates
      _texgenPenumbra = new osg::TexGen;
      _texgenPenumbra->setMode(osg::TexGen::EYE_LINEAR);
      
      _texgenUmbra = new osg::TexGen;
      _texgenUmbra->setMode(osg::TexGen::EYE_LINEAR);
      
      // Add Program for rendering pass, don't add any shaders to use fixed functionality
      _program = new osg::Program;
      _stateset->setAttribute(_program.get());
      
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
      
      // Create shader composition defines for texture units
      {
        _stateset->setDefine("BASE_TEX_UNIT", std::to_string(_baseTextureUnit));
        _stateset->setDefine("PENUMBRA_TEX_UNIT", std::to_string(baseShadowTextureUnit));
        _stateset->setDefine("UMBRA_TEX_UNIT", std::to_string(baseShadowTextureUnit+1));
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
    
    // Find desired light in scene, which must exist either within or before the shadowed scene in the scenegraph
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
      cbbv.setTraversalMask(_shadowedScene->getCastsShadowTraversalMask());
      
      _shadowedScene->osg::Group::traverse(cbbv);
      
      osg::BoundingBox bb = cbbv.getBoundingBox();
      
      if (lightPos[3]!=0.0)   // point light
      {
        osg::Vec3d lightPos3(lightPos.x(), lightPos.y(), lightPos.z());
        osg::Vec3d lightToCenter = bb.center() - lightPos3;
        double lightDistance = lightToCenter.normalize();
        double bbRadius = bb.radius() * _sceneScale;
        
        _lightDistanceUniform->set((float)lightDistance);

        double penumbraDistance = lightDistance / (_lightSize/bbRadius + 1.0);
        double penumbraZNear = penumbraDistance - bbRadius;
        double penumbraZFar  = penumbraDistance + bbRadius;
        double penumbraFOV = 2.0*std::asin(bbRadius / penumbraDistance);

        _penumbraDistanceUniform->set((float)penumbraDistance);
        _penumbraZNearFarInvUniform->set(osg::Vec2(1.0/penumbraZNear, 1.0/penumbraZFar));
        
        _cameraPenumbra->setViewMatrixAsLookAt(bb.center() - (lightToCenter * penumbraDistance), bb.center(), computeOrthogonalVector(lightToCenter));
        _cameraPenumbra->setProjectionMatrixAsPerspective(penumbraFOV * 180.0/osg::PI, 1.0, penumbraZNear, penumbraZFar);
        
        double umbraDistance = lightDistance / (_lightSize/bbRadius - 1.0);
        double umbraZNear = umbraDistance - bbRadius;
        double umbraZFar  = umbraDistance + bbRadius;
        double umbraFOV = 2.0*std::asin(bbRadius / umbraDistance);
        
        _umbraDistanceUniform->set((float)umbraDistance);
        _umbraZNearFarInvUniform->set(osg::Vec2(1.0/umbraZNear, 1.0/umbraZFar));

        _cameraUmbra->setViewMatrixAsLookAt(bb.center() + (lightToCenter * umbraDistance), bb.center(), computeOrthogonalVector(-lightToCenter));
        _cameraUmbra->setProjectionMatrixAsPerspective(umbraFOV * 180.0/osg::PI, 1.0, umbraZNear, umbraZFar);
        
        //std::cout<< "lightDistance = " << lightDistance << ", penumbraDistance = " << penumbraDistance << ", bb radius = " << bbRadius << ", Penumbra fov (deg) = " << penumbraFOV * 180.0/osg::PI << std::endl;

        //std::cout<< "lightDistance = " << lightDistance << ", umbraDistance = " << umbraDistance << ", bb radius = " << bbRadius << ", Umbra fov (deg) = " << umbraFOV * 180.0/osg::PI << std::endl;

      }
      else    // directional light
      {
        // make an orthographic projection
        osg::Vec3d ortho_lightDir(lightPos.x(), lightPos.y(), lightPos.z());
        ortho_lightDir.normalize();
        double bbRadius = bb.radius();
        
        // set the position far away along the light direction
        double centerDistance = bbRadius;
        osg::Vec3d position = bb.center() + ortho_lightDir * centerDistance;
        
        double znear = centerDistance-bbRadius;
        double zfar  = centerDistance+bbRadius;
        //double zNearRatio = 0.001;
        //if (znear<zfar*zNearRatio) znear = zfar*zNearRatio;
        
        double top   = bbRadius;
        double right = top;
        
        //std::cout<< "centerDistance = " << centerDistance << ", bb radius = " << bbRadius << std::endl;
        
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
    osg::Texture2D *displayTexture = _texturePenumbraDepth;
    
    // Make sure we attach initialized texture to HUD
    if( displayTexture == nullptr )
    {
      init();
      displayTexture = _texturePenumbraDepth;
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
    
    // draw subgraph after main camera view.
    camera->setRenderOrder(osg::Camera::POST_RENDER);
    
    // we don't want the camera to grab event focus from the viewers main camera(s).
    camera->setAllowEventFocus(false);
    
    osg::Geode* geode = new osg::Geode;
    osg::Geometry *geometry = osg::createTexturedQuadGeometry(osg::Vec3(), osg::Vec3(size.x(), 0, 0), osg::Vec3(0, size.y(), 0));
    geode->addDrawable( geometry );
    
    geometry->setDrawCallback(new DrawableDrawWithDepthShadowComparisonOffCallback(displayTexture));
    
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

