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
    
    /**
     Get the per-eye texture size suggested by OpenVR
     */
    void getSuggestedTextureSize(unsigned int& w, unsigned int& h)
    { w = _width; h = _height; }
    
    /** Get whether OpenVR has been initialized */
    bool getIsInitialized() { return _isInitialized; }
    
  protected:
    virtual ~OpenVRDevice();
    
    double _worldUnitsPerMeter; // Distance units per real-world meter
    unsigned int _width, _height; // Per-eye texture dimensions
    
    bool _isInitialized; // Whether OpenVR is initialized
    
    vr::IVRSystem* _vrSystem; // OpenVR interface
    vr::IVRRenderModels* _vrRenderModels; // Controller models
  };
  
} // !namespace OpenFrames

#endif  // !define _OF_OPENVRDEVICE_
