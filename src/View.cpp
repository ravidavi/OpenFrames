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

#include <OpenFrames/View>
#include <OpenFrames/ReferenceFrame>
#include <osgViewer/View>

namespace OpenFrames
{

class FollowingTrackball : public osgGA::TrackballManipulator
{
  public:
	FollowingTrackball(TransformAccumulator *xform)
	: _xform(xform) 
	{
	  // We will compute the home position manually
	  setAutoComputeHomePosition(false);

	  // We don't want the view to ever go through the center of
	  // the viewed frame, so we set the minimum view distance to 0.
	  setMinimumDistance(0.0, false);
	}

	virtual const char* className() const { return "FollowingTrackball"; }

	virtual osg::Matrixd getMatrix() const
	{
	  osg::Matrixd matrix = TrackballManipulator::getMatrix();
	  matrix.postMult(_xform->getLocalToWorld());
	  return matrix;
	}

	virtual osg::Matrixd getInverseMatrix() const
	{
	  osg::Matrixd& view = _xform->getWorldToLocal();
	  view.postMult(TrackballManipulator::getInverseMatrix());
	  return view;
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
	osg::ref_ptr<TransformAccumulator> _xform;
};

View::View()
{
	_init();
	setViewFrame(NULL, NULL);
	resetTrackball();
}

View::View(ReferenceFrame *root, ReferenceFrame *frame)
{
	_init();
	setViewFrame(root, frame);
	resetTrackball();
}

View::~View() {}

void View::_init()
{
	  // Set up the ReferenceFrame transform accumulator
	_xform = new TransformAccumulator;

	  // Setup the trackball view manipulator
	_trackball = new FollowingTrackball(_xform.get());

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

void View::setViewFrame( ReferenceFrame* root, ReferenceFrame* frame )
{
	_xform->setRoot(root);
	_xform->setOrigin(frame);
}

}
