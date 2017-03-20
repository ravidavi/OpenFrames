/***********************************
 Copyright 2017 Ravishankar Mathur
 
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

#ifndef _OF_OPENVRDEVICE_
#define _OF_OPENVRDEVICE_

#include <OpenFrames/Export.h>
#include <OpenFrames/View.hpp>
#include <osg/ref_ptr>
#include <osg/Referenced>
#include <osg/Quat>
#include <osg/Texture>
#include <osg/Vec3d>

/** Classes declared in OpenVR header */
namespace vr {
  class IVRSystem;
  class IVRRenderModels;
}

namespace OpenFrames {
  class VRTextureBuffer; // Used by OpenVRSwapBuffers below
  
  /******************************************
   * Ravi Mathur
   * OpenFrames API, class OpenVRDevice
   * Represents data needed to use an OpenVR-supported HMD.
   ******************************************/
  class OF_EXPORT OpenVRDevice : public osg::Referenced
  {
  public:
    OpenVRDevice(float worldUnitsPerMeter, float userHeightInMeters);

    /**
     Initialize OpenVR and connect to the HMD
     Return status: whether OpenVR was initialized
     */
    bool initVR();
    void shutdownVR();

    /** Get the per-eye texture size recommended by OpenVR */
    void getRecommendedTextureSize(int& w, int& h)
    {
      w = _width; h = _height;
    }

    /** Get whether OpenVR has been initialized */
    inline bool isInitialized() { return _isInitialized; }

    /** Update and get the per-eye projection matrix */
    void updateProjectionMatrices();
    osg::Matrixf& getRightEyeProjectionMatrix() { return _rightProj; }
    osg::Matrixf& getLeftEyeProjectionMatrix() { return _leftProj; }
    osg::Matrixf& getCenterProjectionMatrix() { return _centerProj; }

    /** Update and get the per-eye view matrix */
    void updateViewOffsets();
    osg::Matrixf& getRightEyeViewOffsetMatrix() { return _rightViewOffset; }
    osg::Matrixf& getLeftEyeViewOffsetMatrix() { return _leftViewOffset; }
    osg::Matrixf& getCenterViewOffsetMatrix() { return _centerViewOffset; }

    /** Update poses (positions/orientations) of all VR devices, and wait
     for the signal to start rendering. Note that this should be called
     just before the start of the rendering pass. */
    void waitGetPoses();
    osg::Matrixf& getHMDPoseMatrix() { return _hmdPose; }
    
    /** Get/set the world units in meters */
    void setWorldUnitsPerMeter(float worldUnitsPerMeter)
    {
      _worldUnitsPerMeter = worldUnitsPerMeter;
      _ipd = -1.0f; // Indicate that eye offsets should be recomputed
    }
    float getWorldUnitsPerMeter() { return _worldUnitsPerMeter; }

    /** Submits the latest rendered eye textures to OpenVR */
    void submitFrame(GLuint rightEyeTexName, GLuint leftEyeTexName);
    
  protected:
    virtual ~OpenVRDevice();
    
    float _worldUnitsPerMeter; // Distance units per real-world meter
    float _userHeightInMeters; // Height of user's HMD origin (approx forehead)
    int _width, _height; // Per-eye texture dimensions
    
    bool _isInitialized; // Whether OpenVR is initialized
    
    vr::IVRSystem* _vrSystem; // OpenVR interface
    vr::IVRRenderModels* _vrRenderModels; // Controller models
    
    // Per-eye asymmetric projection matrices
    osg::Matrixf _rightProj, _leftProj, _centerProj;
    
    // Per-eye view matrices, transform Head to Eye space
    osg::Matrixf _rightViewOffset, _leftViewOffset, _centerViewOffset;
    float _ipd; // Interpupillary distance

    // World to Head view transformation
    osg::Matrixf _hmdPose;
  };
  
  /******************************************
   * OpenFrames API, class OpenVRPoseCallback
   * Updates HMD and pose data from OpenVR. This should be attached as an
   * update callback to the view's master camera
   ******************************************/
  class OpenVRPoseCallback : public osg::Callback
  {
  public:
    OpenVRPoseCallback(OpenVRDevice *ovrDevice)
    : _ovrDevice(ovrDevice)
    { }
    
    virtual bool run(osg::Object* object, osg::Object* data);
    
  private:
    osg::observer_ptr<OpenVRDevice> _ovrDevice;
  };
  
  /******************************************
   * OpenFrames API, class OpenVRSlaveCallback
   * Inherit master camera view matrix but not its projection matrix
   ******************************************/
  struct OpenVRSlaveCallback : public osg::View::Slave::UpdateSlaveCallback
  {
    enum CameraType
    {
      RIGHT_CAMERA,
      LEFT_CAMERA,
      MONO_CAMERA
    };
    
    OpenVRSlaveCallback(CameraType cameraType, OpenVRDevice *ovrDevice)
    : _cameraType(cameraType), _ovrDevice(ovrDevice)
    { }
    
    virtual void updateSlave(osg::View& view, osg::View::Slave& slave);
    
    CameraType _cameraType;
    osg::observer_ptr<OpenVRDevice> _ovrDevice;
  };
  
  /******************************************
   * OpenFrames API, class OpenVRTrackball
   * Extends FollowingTrackball to include the OpenVR HMD transform.
   * This results in the World->Head transform, which can be combined
   * with the Head->Eye transform to create the per-eye view matrix.
   ******************************************/
  class OpenVRTrackball : public FollowingTrackball
  {
  public:
    OpenVRTrackball(OpenVRDevice *ovrDevice);
    
    virtual const char* className() const { return "OpenVRManipulator"; }

    // Get World to Head matrix
    virtual osg::Matrixd getInverseMatrix() const;

  private:
    osg::observer_ptr<OpenVRDevice> _ovrDevice;
  };
  
  /******************************************
   * OpenFrames API, class OpenVRSwapBuffers
   * Submit eye textures to OpenVR.
   ******************************************/
  struct OpenVRSwapBuffers : public osg::GraphicsContext::SwapCallback
  {
  public:
    OpenVRSwapBuffers(OpenVRDevice *ovrDevice, VRTextureBuffer *texBuffer);
    
    // Submit eye textures to OpenVR and do standard swap buffers
    virtual void swapBuffersImplementation(osg::GraphicsContext *gc);
    
    osg::observer_ptr<OpenVRDevice> _ovrDevice;
    osg::observer_ptr<VRTextureBuffer> _texBuffer;
  };
  
} // !namespace OpenFrames

#endif  // !define _OF_OPENVRDEVICE_
