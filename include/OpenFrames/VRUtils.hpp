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
#include <OpenFrames/OpenVRDevice.hpp>

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
    
    // Cameras that allow color textures to be chained between VRCameras
    // in case MSAA is being used
    osg::ref_ptr<osg::Camera> _rightTexCamera, _leftTexCamera;
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
    
    VRCamera(VRTextureBuffer *texBuffer, int camNum, StereoMode mode, bool useMSAA = false);
    
    // Get whether MSAA is being used
    bool getUseMSAA() { return _useMSAA; }
    
    /** Set the OpenGL clear mask for cameras */
    void setClearColorBuffer(bool clear);
    bool getClearColorBuffer() { return ((_rightCamera->getClearMask() & GL_COLOR_BUFFER_BIT) != 0x0); }
    
    /** Get number of cameras in use, depending on StereoMode */
    unsigned int getNumCameras();
    
    /** Get the internal osg::Camera at desired position */
    osg::Camera* getCamera(unsigned int pos);
    
    /** Disable all internal cameras */
    void disableCameras();
    
    // Update projection/modelview matrices, enable cameras, and handle other
    // camera-specific operations
    void updateCameras(osg::Matrixd& viewmat, osg::Matrixd& projmat, const double &zNear)
    {
      updateCameras(viewmat, viewmat, viewmat, projmat, projmat, projmat, zNear);
    }
    void updateCameras(osg::Matrixd& rightView, osg::Matrixd& leftView, 
      osg::Matrixd& centerView, osg::Matrixd& rightProj, 
      osg::Matrixd& leftProj, osg::Matrixd& centerProj, const double &zNear);

  protected:
    virtual ~VRCamera();
    
    /** Cameras to represent right and left eye views */
    osg::ref_ptr<osg::Camera> _rightCamera, _leftCamera;
    
    /** Camera that draws one view to both left & right eye textures */
    osg::ref_ptr<osg::Camera> _monoCamera;
    
    /** The texture buffers drawn on by Cameras */
    osg::observer_ptr<VRTextureBuffer> _texBuffer;
    
    StereoMode _mode; // Stereo rendering mode
    
    /** Whether to use multisample antialiasing */
    bool _useMSAA;
  };
  
  /** Creates and manages VR cameras for the depth partitioner */
  struct OF_EXPORT VRCameraManager : public DepthPartitionCallback::CameraManager
  {
    VRCameraManager(VRTextureBuffer *texBuffer, OpenVRDevice *ovrDevice);
    virtual ~VRCameraManager();
    
    virtual std::string getCameraName(unsigned int camNum);
    
    virtual void enableCamera(unsigned int camNum,
                              osg::GraphicsContext* gc,
                              osg::Camera* masterCamera,
                              const double &zNear, const double &zFar);
    virtual void disableCameras(unsigned int start);
    virtual void reset();
    virtual void setClearColorBuffer(bool clear);
    
    typedef std::vector< osg::ref_ptr<VRCamera> > VRCameraList;
    VRCameraList _vrCameraList;
    
    // The VR eye texture buffers and related utilities
    osg::observer_ptr<VRTextureBuffer> _texBuffer;
    
    osg::observer_ptr<OpenVRDevice> _ovrDevice; // OpenVR interface
  };
  
} // !namespace OpenFrames

#endif  // !define _OF_VRUTILS_
