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

#include <osg/ref_ptr>
#include <osg/Referenced>
#include <osg/Quat>
#include <osg/Texture>
#include <osg/Vec3d>

namespace vr {
  class IVRSystem;
  class IVRRenderModels;
}

namespace OpenFrames {
  
  /******************************************
   * Ravi Mathur
   * OpenFrames API, class OpenVRDevice
   * Represents data needed to use an OpenVR-supported HMD.
   ******************************************/
  class OF_EXPORT OpenVRDevice : public osg::Referenced
  {
  public:
    OpenVRDevice(double worldUnitsPerMeter);
    
    /**
     Initialize OpenVR and connect to the HMD
     Return status: whether OpenVR was initialized
     */
    bool initVR();
    void shutdownVR();
    
    /** Get the per-eye texture size recommended by OpenVR */
    void getRecommendedTextureSize(int& w, int& h)
    { w = _width; h = _height; }
    
    /** Get whether OpenVR has been initialized */
    bool isInitialized() { return _isInitialized; }
    
    /** Update the per-eye projection matrix bounds, and get the
     matrices using the given depth clip bounds. */
    void updateProjectionMatrices();
    osg::Matrixf& getRightEyeProjectionMatrix() { return _rightProj; }
    osg::Matrixf& getLeftEyeProjectionMatrix() { return _leftProj; }
    osg::Matrixf& getCenterProjectionMatrix() { return _centerProj; }

    /** Update poses (positions/orientations) of all VR devices, and wait
     for the signal to start rendering. Note that this should be called
     at (or near) the end of the OGS update traversal, most likely during
     a slave camera update callback. */
    void waitGetPoses();
    
    /** Submits the latest rendered eye textures to OpenVR */
    void submitFrame(GLuint rightEyeTexName, GLuint leftEyeTexName);
    
  protected:
    virtual ~OpenVRDevice();
    
    double _worldUnitsPerMeter; // Distance units per real-world meter
    int _width, _height; // Per-eye texture dimensions
    
    bool _isInitialized; // Whether OpenVR is initialized
    
    vr::IVRSystem* _vrSystem; // OpenVR interface
    vr::IVRRenderModels* _vrRenderModels; // Controller models
    
    osg::Matrixf _rightProj, _leftProj, _centerProj; // Per-eye projection matrices
    
    // Head to world view transformation
    osg::Vec3d _position;
    osg::Quat _orientation;
  };
  
} // !namespace OpenFrames

#endif  // !define _OF_OPENVRDEVICE_
