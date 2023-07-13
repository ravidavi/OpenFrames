/***********************************
 Copyright 2023 Ravishankar Mathur
 
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

/** \file View.cpp
 * FollowingTrackball- and View-class function definitions.
 */

#include <OpenFrames/ReferenceFrame.hpp>
#include <OpenFrames/Utilities.hpp>
#include <OpenFrames/View.hpp>
#include <osgViewer/View>
#include <iostream>

namespace OpenFrames
{
  
  /*******************************************************/
  FollowingTrackball::FollowingTrackball()
  : _frameType(View::ABSOLUTE_FRAME), _rotationType(View::DIRECT)
  {
    // We will compute the home position manually
    setAutoComputeHomePosition(false);
    
    // We don't want the view to ever go through the center of
    // the viewed frame, so we set the minimum view distance to 0.
    setMinimumDistance(0.0, false);
  }
  
  /*******************************************************/
  FollowingTrackball::~FollowingTrackball() {}
  
  /*******************************************************/
  void FollowingTrackball::setTransformSources(TransformAccumulator *xform, TransformAccumulator *xform_lookat, View::ViewFrameType frameType, View::ViewRotationType rotationType)
  {
    _xform = xform;
    _xform_lookat = xform_lookat;
    _frameType = frameType;
    _rotationType = rotationType;
  }

  /*******************************************************/
  void FollowingTrackball::saveState()
  {
    // Save data for parent trackball (from osgGA)
    osg::Vec3d eye, center, up;

    // Get the look vectors for the current view
    getTransformation(eye, center, up);

    // Save the current view as the home position
    setHomePosition(eye, center, up);
  }
  
  /*******************************************************/
  void FollowingTrackball::restoreState()
  {
    home(0.0);
  }
  
  /*******************************************************/
  void FollowingTrackball::resetState()
  {
    osg::Vec3d eye(0.0, -1.0, 0.0);
    osg::Vec3d center(0.0, 0.0, 0.0);
    osg::Vec3d up(0.0, 0.0, 1.0);
    setHomePosition(eye, center, up);
    home(0.0);
  }

  /*******************************************************/
  // Get the Viewpoint to World transformation matrix
  osg::Matrixd FollowingTrackball::getMatrix() const
  {
    return osg::Matrix::inverse(FollowingTrackball::getInverseMatrix());    
  }
  
  /*******************************************************/
  // Get the World to Viewpoint transformation matrix
  osg::Matrixd FollowingTrackball::getInverseMatrix() const
  {
    osg::Matrixd matWorldToLocal;
    computeWorldToViewMatrix(matWorldToLocal);
    matWorldToLocal.postMult(TrackballManipulator::getInverseMatrix());
    return matWorldToLocal;
  }
  
  /*******************************************************/
  // Compute the World to Viewpoint transformation matrix that
  // incorporates custom view types
  void FollowingTrackball::computeWorldToViewMatrix(osg::Matrixd& matrix) const
  {
    // First compute World to Local transform
    if(_frameType == View::ABSOLUTE_FRAME)
    {
      // Translate origin to the viewed frame's origin.
      // Since there is no rotation, just negate the translation
      // component of the frame's Local->World transform.
      matrix.setTrans(-_xform->getLocalToWorld().getTrans());
    }
    else
    {
      // Get the body-fixed transform for the viewed frame
      matrix = _xform->getWorldToLocal();
    }
    
    // If using From->To view, add a transformation that rotates
    // towards the desired LookAt frame
    if(_xform_lookat->isValid())
    {
      // Compute From->To vector in From frame's coordinates
      osg::Vec3d relpos = (_xform_lookat->getLocalToWorld()*matrix).getTrans();
      double len = relpos.length();
      const double eps = 1.0e-6;
      
      // Compute From->To rotation if origins are not coincident
      // Rotation should map Y-axis to From->To vector, since the
      // Y-axis is the Trackball's initial view direction
      if(len > eps)
      {
        // Direct rotation using shortest angle
        if(_rotationType == View::DIRECT)
        {
          // Direct rotation from Y-axis to final lookat vector
          osg::Quat q;
          q.makeRotate(relpos, osg::Vec3d(0.0, 1.0, 0.0));
          
          // Append the rotation to World->Local matrix
          matrix.postMultRotate(q);
        }
        
        // Azimuth-Elevation rotation
        // More "natural" rotation, first in X-Y plane then up Z-axis
        else
        {
          // Compute projection of From-To vector onto XY plane
          osg::Vec3d relpos_tmp(relpos[0], relpos[1], 0.0);
          len = relpos_tmp.length();
          
          // Rotate XY projection vector in the World frame (matrix)
          // to the Y-axis of an intermediate frame
          // Only rotate if XY projection vector is nonsingular
          osg::Quat q;
          if(len > eps)
          {
            q.makeRotate(relpos_tmp, osg::Vec3d(0.0, 1.0, 0.0));
            matrix.postMultRotate(q);
          }
          
          // Rotate From-To vector in the intermediate frame (matrix)
          // to the Y-axis of the final Local frame
          // Note that |relpos_tmp| > epsilon is guaranteed
          relpos_tmp.set(0, len, relpos[2]);
          q.makeRotate(relpos_tmp, osg::Vec3d(0.0, 1.0, 0.0));
          matrix.postMultRotate(q);
        }
      }
    }
  }
  
  /*******************************************************/
  bool FollowingTrackball::handle(const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& us)
  {
    osgViewer::View *view = dynamic_cast<osgViewer::View*>(&us);
    osg::Viewport *vp = OpenFrames::getMainViewport(view);
    
    // For the trackballs to work correctly, we need to specify that each
    // osgViewer::View has its own range of (x,y) coordinates
    if(vp)
    {
      osg::ref_ptr<osgGA::GUIEventAdapter> event = new osgGA::GUIEventAdapter(ea);
      event->setInputRange(vp->x(), vp->y(), vp->x() + vp->width(), vp->y() + vp->height());
      return TrackballManipulator::handle(*(event.get()), us);
    }
    else
    {
      std::cerr<< "OpenFrames::FollowingTrackball ERROR: No suitable viewport." << std::endl;
      return TrackballManipulator::handle(ea, us);
    }
  }
  
  /*******************************************************/
  View::View()
  {
    _init();
    resetView();
  }
  
  /*******************************************************/
  View::View(ReferenceFrame *root, ReferenceFrame *viewFrame, ViewFrameType frameType)
  {
    _init();
    setViewFrame(root, viewFrame, frameType);
    resetView();
  }
  
  /*******************************************************/
  View::View(ReferenceFrame *root, ReferenceFrame *viewFrame, ReferenceFrame *lookatFrame, ViewFrameType frameType, ViewRotationType rotationType)
  {
    _init();
    setViewBetweenFrames(root, viewFrame, lookatFrame, frameType, rotationType);
    resetView();
  }
  
  /*******************************************************/
  View::~View() {}
  
  /*******************************************************/
  void View::_init()
  {
    // Set up the ReferenceFrame transform accumulator
    _xform = new TransformAccumulator;
    _xform_lookat = new TransformAccumulator;
    _frameType = RELATIVE_FRAME;
    _rotationType = AZEL;
    
    // Setup the trackball view manipulator
    setTrackball(NULL);
    
    // Set the default view distance to be auto computed
    _defaultViewDistance = 0.0;
    
    // Set the default view parameters to be unused
    clearDefaultViewParameters();
    
    // Set up the projection matrix
    _projType = PERSPECTIVE;
    setPerspective(45.0, 640.0/480.0);
  }
  
  /*******************************************************/
  osg::Matrixd View::getViewMatrix()
  {
    return _trackball->getInverseMatrix();
  }
  
  /*******************************************************/
  /** Use a new trackball manipulator, and set its data sources */
  void View::setTrackball(FollowingTrackball *trackball)
  {
    if(trackball == NULL) trackball = new FollowingTrackball;
    
    _trackball = trackball;
    _trackball->setTransformSources(_xform.get(), _xform_lookat.get(), _frameType, _rotationType);
  }
  
  /*******************************************************/
  /** Reset the trackball to look at the origin frame */
  void View::resetView()
  {
    // First let trackball reset its internal state
    _trackball->resetState();
    
    if(_defaultUp.length() != 0.0)
    {
      // Set the trackball's home position
      _trackball->setHomePosition(_defaultEye, _defaultCenter, _defaultUp);
    }
    else
    {
      // Get the bounding sphere of the frame we're looking at.
      osg::BoundingSphere bs;
      if(_xform->isValid())
        bs = _xform->getOrigin()->getBound();
      
      // Set default distance if needed
      if(_defaultViewDistance > 0.0) bs._radius = _defaultViewDistance;
      else if(bs._radius <= 0.0) bs._radius = 1;
      
      // Compute view distance based on bounding sphere size and field of view
      double dist = 2.0*bs._radius; // Default
      if (_projType == PERSPECTIVE)
      {
        double fovy, ratio;
        getPerspective(fovy, ratio); // Get projection vertical fov and aspect ratio (width/height)
        fovy = (fovy/2.0) * (osg::PI/180.0); // Trig calculations require radians and half-angle
        
        // Trig time! Compute camera distance such that fov is tangent to bounding sphere
        double fovx = std::atan(ratio*std::tan(fovy)); // Compute horizontal fov
        dist = bs._radius / std::sin(std::min(fovy, fovx));
      }
      
      // Set the trackball's home position
      _trackball->setHomePosition(bs._center+osg::Vec3(0.0, -dist, 0.0),
                                  bs._center, osg::Vec3(0.0, 0.0, 1.0));
    }
    
    _trackball->home(0.0); // Tell trackball to reset to its home position
  }
  
  /*******************************************************/
  void View::saveView()
  {
    _trackball->saveState();
  }

  /*******************************************************/
  void View::restoreView()
  {
    _trackball->restoreState();
  }
  
  /*******************************************************/
  void View::setViewFrame( ReferenceFrame* root,
                          ReferenceFrame* viewFrame,
                          ViewFrameType frameType )
  {
    _xform->setRoot(root);
    _xform->setOrigin(viewFrame);
    _frameType = frameType;
    _xform_lookat->setRoot(NULL);
    _trackball->setTransformSources(_xform.get(), _xform_lookat.get(), _frameType, _rotationType);
  }
  
  /*******************************************************/
  void View::setViewBetweenFrames( ReferenceFrame* root,
                                  ReferenceFrame* viewFrame,
                                  ReferenceFrame* lookatFrame,
                                  ViewFrameType frameType,
                                  ViewRotationType rotationType )
  {
    _xform->setRoot(root);
    _xform->setOrigin(viewFrame);
    _frameType = frameType;
    _xform_lookat->setRoot(root);
    _xform_lookat->setOrigin(lookatFrame);
    _rotationType = rotationType;
    _trackball->setTransformSources(_xform.get(), _xform_lookat.get(), _frameType, _rotationType);
  }
  
}
