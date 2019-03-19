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
 * Declaration of the FocalPointShadowMap class.
 */

#ifndef OF_FOCALPOINTSHADOWMAP
#define OF_FOCALPOINTSHADOWMAP

#include <OpenFrames/Export.h>

#include <osg/Camera>
#include <osg/Material>
#include <osg/MatrixTransform>
#include <osg/LightSource>
#include <osgShadow/ShadowTechnique>

namespace OpenFrames
{

  /**
   * \class RenderRectangle
   *
   * \brief Encapsulates a rectangle in which a scene can be rendered.
   *
   * This class encapsulates a rectangle in which a scene can be rendered.
   * It provides decorations for the scene, such as a border around the
   * rectangle. It also automatically analyzes a scene and (if needed)
   * renders it in multiple stages in case there are large z distances
   * involved.
   */
  class OF_EXPORT FocalPointShadowMap : public osgShadow::ShadowTechnique
  {
    public :
    FocalPointShadowMap();
    
    FocalPointShadowMap(const FocalPointShadowMap& es, const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY);
    
    META_Object(OpenFrames, FocalPointShadowMap);
    
    /** set the polygon offset used initially */
    void setPolygonOffset(const osg::Vec2& polyOffset) { _polyOffset = polyOffset; }
    
    /** get the used polygon offset */
    const osg::Vec2& getPolygonOffset() const { return _polyOffset; }
    
    /** Set the values for the ambient bias the shader will use.*/
    void setAmbientBias(const osg::Vec2& ambientBias );
    
    /** Get the values that are used for the ambient bias in the shader.*/
    const osg::Vec2& getAmbientBias() const { return _ambientBias; }
    
    /** Set the light size (assuming half-length of square area light) */
    void setLightSize(const double& lightSize) { _lightSize = lightSize; }
    
    /** Get the light size (assuming half-length of square area light) */
    const double& getLightSize() const { return _lightSize; }
    
    /** Add a shader to internal list, will be used instead of the default ones */
    inline void addShader(osg::Shader* shader) { _shaderList.push_back(shader); }
    
    template<class T> void addShader( const osg::ref_ptr<T>& shader ) { addShader(shader.get()); }
    
    /** Reset internal shader list */
    inline void clearShaderList() { _shaderList.clear(); }
    
    /** initialize the ShadowedScene and local cached data structures.*/
    virtual void init();
    
    /** run the update traversal of the ShadowedScene and update any loca chached data structures.*/
    virtual void update(osg::NodeVisitor& nv);
    
    /** run the cull traversal of the ShadowedScene and set up the rendering for this ShadowTechnique.*/
    virtual void cull(osgUtil::CullVisitor& cv);
    
    /** Clean scene graph from any shadow technique specific nodes, state and drawables.*/
    virtual void cleanSceneGraph();
    
    /** Resize any per context GLObject buffers to specified size. */
    virtual void resizeGLObjectBuffers(unsigned int maxSize);
    
    /** If State is non-zero, this function releases any associated OpenGL objects for
     * the specified graphics context. Otherwise, releases OpenGL objects
     * for all graphics contexts. */
    virtual void releaseGLObjects(osg::State* = 0) const;
    
    // debug methods
    osg::ref_ptr<osg::Camera> makeDebugHUD();
    
  protected:
    virtual ~FocalPointShadowMap(void) {};
    
    /** Create the managed Uniforms */
    virtual void createUniforms();
    
    virtual void createShaders();
    
    // forward declare, interface and implementation provided in ShadowMap.cpp
    class DrawableDrawWithDepthShadowComparisonOffCallback;
    
    osg::ref_ptr<osg::Camera>       _cameraPenumbra;
    osg::ref_ptr<osg::Camera>       _cameraUmbra;
    osg::ref_ptr<osg::Texture2D>    _texturePenumbraDepth;
    osg::ref_ptr<osg::Texture2D>    _textureUmbraDepth;
    osg::ref_ptr<osg::TexGen>       _texgenPenumbra;
    osg::ref_ptr<osg::TexGen>       _texgenUmbra;
    osg::ref_ptr<osg::StateSet>     _stateset;
    osg::ref_ptr<osg::Program>      _program;
    
    typedef std::vector< osg::ref_ptr<osg::Uniform> > UniformList;
    typedef std::vector< osg::ref_ptr<osg::Shader> > ShaderList;
    
    osg::ref_ptr<osg::Uniform>      _ambientBiasUniform;
    osg::ref_ptr<osg::Uniform>      _umbraDistanceUniform;
    osg::ref_ptr<osg::Uniform>      _lightDistanceUniform;
    osg::ref_ptr<osg::Uniform>      _sizeRatioUniform;
    osg::ref_ptr<osg::Uniform>      _texelSizeUniform;
    UniformList                     _uniformList;
    ShaderList                      _shaderList;
    ShaderList                      _shaderListShadowPass;
    osg::Vec2                       _polyOffset;
    osg::Vec2                       _ambientBias;
    unsigned int                    _baseTextureUnit;
    double                          _lightSize;
  };
  
}

#endif
