/***********************************
 Copyright 2017 Ravishankar Mathur, Emergent Space Technologies Inc.
 
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

#ifndef _OF_VRUTILS_
#define _OF_VRUTILS_

#include <OpenFrames/Export.h>
#include <OpenFrames/DepthPartitioner.hpp>

#include <osg/ref_ptr>
#include <osg/Referenced>
#include <osg/Camera>
#include <osg/Texture2D>

namespace OpenFrames {
  
  /******************************************
   * Ravi Mathur
   * OpenFrames API, class VRTextureBuffer
   * Encapsulates textures used for VR offscreen rendering.
   ******************************************/
  struct OF_EXPORT VRTextureBuffer : public osg::Referenced
  {
  public:
    /** Create textures using specified size. Zero size means auto-compute */
    VRTextureBuffer(int width = 1080, int height = 1200);
    virtual ~VRTextureBuffer();
    
    // Textures for each eye's image
    osg::ref_ptr<osg::Texture2D> _rightColorTex, _rightDepthTex;
    osg::ref_ptr<osg::Texture2D> _leftColorTex, _leftDepthTex;
  };
  
  /******************************************
   * Ravi Mathur
   * OpenFrames API, class VRCamera
   * Encapsulates cameras used for VR stereo rendering.
   ******************************************/
  class OF_EXPORT VRCamera : public osg::Referenced
  {
  public:
    enum StereoMode
    {
      STEREO = 0, // Always stereo
      MONO = 1,   // Always mono
      AUTO = 2    // Auto-switch between Stereo/Mono based on distance
    };
    
    VRCamera(VRTextureBuffer *texBuffer, int camNum, StereoMode mode);
    
    // Set the projection matrix and enable cameras based on StereoMode
    void setProjectionMatrix(osg::Matrixd& projmat, const double &zNear);
    
    // Set view matrix
    void setViewMatrix(osg::Matrixd& viewmat);
    
    unsigned int getNumCameras();
    osg::Camera* getCamera(unsigned int camNum);
    void disableCameras();
    
  protected:
    virtual ~VRCamera();
    
    /** Cameras to represent right and left eye views */
    osg::ref_ptr<osg::Camera> _rightCamera, _leftCamera;
    
    /** Camera that draws one view to both left & right eye textures */
    osg::ref_ptr<osg::Camera> _monoCamera;
    
    /** The texture buffers drawn on by Cameras */
    osg::ref_ptr<VRTextureBuffer> _texBuffer;
    
    StereoMode _mode;
  };
  
  /** Creates and manages VR cameras for the depth partitioner */
  struct OF_EXPORT VRCameraManager : public DepthPartitionCallback::CameraManager
  {
    VRCameraManager(VRTextureBuffer *texBuffer);
    virtual ~VRCameraManager();
    
    virtual void enableCamera(unsigned int camNum,
                              osg::GraphicsContext* gc,
                              osg::Camera* masterCamera,
                              const double &zNear, const double &zFar);
    virtual void disableCameras(unsigned int start);
    virtual void reset();
    virtual void setClearColorBuffer(bool clear);
    
    typedef std::vector< osg::ref_ptr<VRCamera> > VRCameraList;
    VRCameraList _vrCameraList;
    
    osg::ref_ptr<VRTextureBuffer> _texBuffer;
  };
  
} // !namespace OpenFrames

#endif  // !define _OF_VRUTILS_
