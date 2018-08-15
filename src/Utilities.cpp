/***********************************
 Copyright 2018 Ravishankar Mathur
 
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

/** \file Utilities.cpp
 * Several utility function definitions.
 */

#include <OpenFrames/Utilities.hpp>

namespace OpenFrames
{
  /**********************************************/
  void updateProjectionMatrix(osg::Matrix& proj, const double &zNear, const double &zFar)
  {
    double left, right, bottom, top, oldNear, oldFar;
    
    // Clamp the projection matrix z values to the range (near, far)
    double epsilon = 1.0e-6;
    if (fabs(proj(0, 3)) < epsilon &&
        fabs(proj(1, 3)) < epsilon &&
        fabs(proj(2, 3)) < epsilon) // Projection is Orthographic
    {
      // Get the current orthographic projection parameters
      proj.getOrtho(left, right, bottom, top, oldNear, oldFar);
      
      // Use the custom computed near/far values
      proj.makeOrtho(left, right, bottom, top, zNear, zFar);
    }
    else // Projection is Perspective
    {
      // Get the current perspective projection parameters
      proj.getFrustum(left, right, bottom, top, oldNear, oldFar);
      
      // Use the custom computed near/far values
      const double nz = zNear / oldNear;
      proj.makeFrustum(left*nz, right*nz, bottom*nz, top*nz, zNear, zFar);
    }
  }

  /*******************************************************/
  osg::Viewport* getMainViewport(osg::View *view)
  {
    // The master camera's viewport should be returned if available
    osg::Camera *cam = view->getCamera();
    osg::Viewport *vp = cam->getViewport();
    
    // Otherwise search through slaves for a valid viewport
    for(unsigned int i = 0; (i < view->getNumSlaves()) && (vp == NULL); ++i)
    {
      cam = view->getSlave(i)._camera; // Get slave camera
      vp = cam->getViewport(); // Get slave's viewport
      
      // Reject slave that renders to FBO
      if(cam->getRenderTargetImplementation() == osg::Camera::FRAME_BUFFER_OBJECT)
      {
        vp = NULL;
      }
    }
    return vp; // Return viewport
  }
  
  /*******************************************************/
  osg::GraphicsContext* getMainGraphicsContext(osg::View *view)
  {
    if(view == NULL) return NULL;
    
    // The master camera's graphics context should be returned if available
    osg::Camera *cam = view->getCamera();
    osg::GraphicsContext *gc = cam->getGraphicsContext();
    
    // Otherwise search through slaves for a valid graphics context
    for(unsigned int i = 0; (i < view->getNumSlaves()) && (gc == NULL); ++i)
    {
      cam = view->getSlave(i)._camera; // Get slave camera
      gc = cam->getGraphicsContext(); // Get slave's graphics context
    }
    
    return gc;
  }
} // !namespace OpenFrames
