/***********************************
   Copyright 2013 Ravishankar Mathur

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

#include <OpenFrames/View.hpp>
#include <OpenFrames/ReferenceFrame.hpp>
#include <osgViewer/View>

namespace OpenFrames
{

void LookAtTransform_AbsoluteDirect(osg::Matrixd &matrix,
                                    TransformAccumulator *from,
                                    TransformAccumulator *to)
{
  // Compute From origin in world coordinates
  osg::Vec3d view_origin = from->getLocalToWorld().getTrans();

  // Compute From->To vector in world coordinates
  osg::Vec3d relpos = to->getLocalToWorld().getTrans() - view_origin;
  double len = relpos.length();

  const double eps = 1.0e-6;
  if(len <= eps)
  {
    // If From and To origins coincide, then just set view to
    // From origin
    matrix.setTrans(from->getWorldToLocal().getTrans());
  }
  else
  {
    osg::Matrixd lookat_matrix;

    // Perform direct rotation from Y-axis to final lookat vector
    lookat_matrix.makeRotate(osg::Vec3d(0, 1, 0), relpos);

    // Translate to Local origin
    lookat_matrix.setTrans(view_origin);

    // Make the transformation World to Local
    matrix.invert(lookat_matrix); 
  }
}

void LookAtTransform_RelativeDirect(osg::Matrixd &matrix,
                                    TransformAccumulator *from,
                                    TransformAccumulator *to)
{
  // Get the From frame's World to Local transformation
  matrix = from->getWorldToLocal();

  // Compute From->To vector in From coordinates
  osg::Vec3d relpos = (to->getLocalToWorld()*matrix).getTrans();
  double len = relpos.length();

  const double eps = 1.0e-6;
  if(len > eps)
  {
    osg::Matrixd lookat_matrix;

    // Perform direct rotation from Y-axis to final lookat vector
    lookat_matrix.makeRotate(osg::Vec3d(0, 1, 0), relpos);

    // Make the transformation World to Local
    matrix.postMult(osg::Matrixd::inverse(lookat_matrix));
  }
}

void LookAtTransform_AbsoluteAzEl(osg::Matrixd &matrix,
                                  TransformAccumulator *from,
                                  TransformAccumulator *to)
{
  // Compute From origin in world coordinates
  osg::Vec3d view_origin = from->getLocalToWorld().getTrans();

  // Compute From->To vector in world coordinates
  osg::Vec3d relpos = to->getLocalToWorld().getTrans() - view_origin;
  double len = relpos.length();

  const double eps = 1.0e-6;
  if(len <= eps)
  {
    // If From and To origins coincide, then just set view to
    // From origin
    matrix.setTrans(from->getWorldToLocal().getTrans());
  }
  else
  {
    // If From and To origins are not coincident, compute the
    // rotation that maps Y-axis to From->To vector. We choose
    // Y axis because that is the Trackball's "into-screen" axis.
    // Note that instead of just picking the shortest rotation, we
    // first rotate in X-Y plane then up Z-axis to give a more
    // "natural" rotation.

    // First rotate Y-axis to projection of final vector on XY plane
    osg::Vec3d relpos_tmp(relpos[0], relpos[1], 0.0);
    len = relpos_tmp.length();

    // Only do in-plane rotation if nonsingular
    osg::Matrixd lookat_matrix;
    if(len > eps)
    {
      lookat_matrix.makeRotate(osg::Vec3d(0, 1, 0), relpos_tmp);
    }

    // Rotate new Y-vector upwards to final vector
    // Note that the length here is guaranteed > epsilon
    relpos_tmp.set(0, len, relpos[2]);
    osg::Quat q; 
    q.makeRotate(osg::Vec3d(0, 1, 0), relpos_tmp);
    lookat_matrix.preMultRotate(q);

    // Translate to Local origin
    lookat_matrix.setTrans(view_origin);

    // Make the transformation World to Local
    matrix.invert(lookat_matrix); 
  }
}

void LookAtTransform_RelativeAzEl(osg::Matrixd &matrix,
                                  TransformAccumulator *from,
                                  TransformAccumulator *to)
{
  // Get the From frame's World to Local transformation
  matrix = from->getWorldToLocal();

  // Compute From->To vector in From coordinates
  osg::Vec3d relpos = (to->getLocalToWorld()*matrix).getTrans();
  double len = relpos.length();

  const double eps = 1.0e-6;
  if(len > eps)
  {
    // If From and To origins are not coincident, compute the
    // rotation that maps Y-axis to From->To vector. We choose
    // Y axis because that is the Trackball's "into-screen" axis.
    // Note that instead of just picking the shortest rotation, we
    // first rotate in X-Y plane then up Z-axis to give a more
    // "natural" rotation.

    // First rotate Y-axis to projection of final vector on XY plane
    osg::Vec3d relpos_tmp(relpos[0], relpos[1], 0.0);
    len = relpos_tmp.length();

    // Only do in-plane rotation if nonsingular
    osg::Matrixd lookat_matrix;
    if(len > eps)
    {
      lookat_matrix.makeRotate(osg::Vec3d(0, 1, 0), relpos_tmp);
    }

    // Rotate new Y-vector upwards to final vector
    // Note that the length here is guaranteed > epsilon
    relpos_tmp.set(0, len, relpos[2]);
    osg::Quat q; 
    q.makeRotate(osg::Vec3d(0, 1, 0), relpos_tmp);
    lookat_matrix.preMultRotate(q);

    // Make the transformation World to Local
    matrix.postMult(osg::Matrixd::inverse(lookat_matrix));
  }
}

class FollowingTrackball : public osgGA::TrackballManipulator
{
  public:
	FollowingTrackball(TransformAccumulator *xform, TransformAccumulator *xform_lookat, View::LookAtType &lookatType)
	: _xform(xform), _xform_lookat(xform_lookat), _lookatType(lookatType)
	{
	  // We will compute the home position manually
	  setAutoComputeHomePosition(false);

	  // We don't want the view to ever go through the center of
	  // the viewed frame, so we set the minimum view distance to 0.
	  setMinimumDistance(0.0, false);
	}

	virtual const char* className() const { return "FollowingTrackball"; }

        // Get the Viewpoint to World transformation matrix
	virtual osg::Matrixd getMatrix() const
	{
          return osg::Matrix::inverse(getInverseMatrix());
	}

        // Get the World to Viewpoint transformation matrix
	virtual osg::Matrixd getInverseMatrix() const
	{
	  osg::Matrixd matrix;

          if(!_xform_lookat->isValid())
          {
            // Not using LookAt frame, so just get World to Local
            // coordinate transformation
            matrix = _xform->getWorldToLocal();
          }
          else
          {
            // If a LookAt frame is defined, create a transform that 
            // looks from the View frame to the LookAt frame using the
            // specified transformation algorithm
            switch(_lookatType)
            {
              case View::ABSOLUTE_AZEL :
                LookAtTransform_AbsoluteAzEl(matrix, _xform.get(), _xform_lookat.get());
                break;
              case View::RELATIVE_AZEL :
                LookAtTransform_RelativeAzEl(matrix, _xform.get(), _xform_lookat.get());
                break;
              case View::ABSOLUTE_DIRECT :
                LookAtTransform_AbsoluteDirect(matrix, _xform.get(), _xform_lookat.get());
                break;
              case View::RELATIVE_DIRECT :
                LookAtTransform_RelativeDirect(matrix, _xform.get(), _xform_lookat.get());
                break;
            }
          }

          // Add trackball to get World to Viewpoint frame
          matrix.postMult(TrackballManipulator::getInverseMatrix());
	  return matrix;
	}

	virtual bool handle(const osgGA::GUIEventAdapter& ea, osgGA::GUIActionAdapter& us)
	{
	  osgViewer::View *view = dynamic_cast<osgViewer::View*>(&us);
	  osg::Viewport *vp = view->getCamera()->getViewport();

	  // For the trackballs to work correctly, we want to specify that each
	  // osgViewer::View has its own range of (x,y) coordinates
	  osg::ref_ptr<osgGA::GUIEventAdapter> event = new osgGA::GUIEventAdapter(ea);
	  event->setInputRange(vp->x(), vp->y(), vp->x() + vp->width(), vp->y() + vp->height());

	  // Continue handling the modified event
	 return TrackballManipulator::handle(*(event.get()), us);
	}

  protected:
	virtual ~FollowingTrackball() {}

	// The ReferenceFrame being followed.
	osg::ref_ptr<TransformAccumulator> _xform, _xform_lookat;
        View::LookAtType &_lookatType;
};

View::View()
{
	_init();
	resetTrackball();
}

View::View(ReferenceFrame *root, ReferenceFrame *frame, ReferenceFrame *lookat, LookAtType lookatType)
{
	_init();
	setViewFrame(root, frame, lookat, lookatType);
	resetTrackball();
}

View::~View() {}

void View::_init()
{
	  // Set up the ReferenceFrame transform accumulator
	_xform = new TransformAccumulator;
        _xform_lookat = new TransformAccumulator;
        _lookatType = RELATIVE_AZEL;

	  // Setup the trackball view manipulator
	_trackball = new FollowingTrackball(_xform.get(), _xform_lookat.get(), _lookatType);

	  // Set the default view distance to be auto computed
	_defaultViewDistance = 0.0;

	  // Set up the projection matrix
	_projType = PERSPECTIVE;
	_aspectMultiplier = 1.0;
	setPerspective(50.0, 640.0/480.0);
}

/** Reset the trackball to look at the origin frame */
void View::resetTrackball()
{
	// Get the bounding sphere of the frame we're looking at.
	osg::BoundingSphere bs;
	if(_xform->isValid())
	  bs = _xform->getOrigin()->getBound();

	// Set default distance if needed
	if(_defaultViewDistance > 0.0) bs._radius = _defaultViewDistance;
	else if(bs._radius <= 0.0) bs._radius = 1.0;

	// Set the trackball's home position based on this bounding sphere
	_trackball->setHomePosition(bs._center+osg::Vec3(0.0, -2.0*bs._radius, 0.0),
	                            bs._center, osg::Vec3(0.0, 0.0, 1.0));
	_trackball->home(0.0); // Tell trackball to reset
}

/** Save the trackball's current view */
void View::saveTrackball()
{
	osg::Vec3d eye, center, up;

	// Get the look vectors for the current view
	_trackball->TrackballManipulator::getInverseMatrix().getLookAt(eye, center, up, _trackball->getDistance());

	// Save the current view as the home position
	_trackball->setHomePosition(eye, center, up);
}

void View::setViewFrame( ReferenceFrame* root, 
                         ReferenceFrame* frame,
                         ReferenceFrame* lookat,
                         LookAtType lookatType)
{
	_xform->setRoot(root);
	_xform->setOrigin(frame);

        if(lookat)
        {
          _xform_lookat->setRoot(root);
          _xform_lookat->setOrigin(lookat);
          _lookatType = lookatType;
        }
}

}
