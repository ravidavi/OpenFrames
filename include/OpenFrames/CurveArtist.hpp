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

#ifndef _OF_CURVEARTIST_
#define _OF_CURVEARTIST_

#include <OpenFrames/Export.h>
#include <OpenFrames/TrajectoryArtist.hpp>
#include <osg/LineStipple>
#include <osg/LineWidth>

namespace OpenFrames
{

/**********************************************************
 * Ravi Mathur
 * OpenFrames API, class CurveArtist
 * Draws a series of Trajectory points connected by lines.  The x,y,z
 * components of the points can be independently specified to be any
 * elements of the Trajectory.
**********************************************************/
class OF_EXPORT CurveArtist : public TrajectoryArtist
{
  public:

	CurveArtist( const Trajectory *traj = NULL );

	// Copy constructor
	CurveArtist( const CurveArtist &ca,
	               const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

	/** Standard OSG node methods. */
	virtual Object* cloneType() const { return new CurveArtist(); }
	virtual Object* clone(const osg::CopyOp& copyop) const { return new CurveArtist(*this,copyop); }
	virtual bool isSameKindAs(const osg::Object* obj) const { return dynamic_cast<const CurveArtist*>(obj)!=NULL; }
	virtual const char* libraryName() const { return "OpenFrames"; }
	virtual const char* className() const { return "CurveArtist"; }

	/** Set the trajectory to be drawn. */
	virtual void setTrajectory(const Trajectory *traj);

	/** Set the data to be used for plotting x/y/z components */
	bool setXData(const Trajectory::DataSource &src);
	bool setYData(const Trajectory::DataSource &src);
	bool setZData(const Trajectory::DataSource &src);

		/** Specify line attributes that should be used. */
	void setColor(float r, float g, float b);
	void setWidth( float width );
	void setPattern( GLint factor, GLushort pattern );

	/** Do the actual drawing */
	virtual void drawImplementation(osg::RenderInfo& renderInfo) const;

	/** Data was cleared from or added to the trajectory. Inherited
	    from TrajectoryArtist */
	virtual void dataCleared(Trajectory* traj);
	virtual void dataAdded(Trajectory* traj);

  protected:
	virtual ~CurveArtist();

	/** Inhereted from TrajectoryArtist */
	virtual osg::BoundingBox computeBoundingBox() const;

	void verifyData() const;

        // Data sources for x, y, and z coordinates
	Trajectory::DataSource _dataSource[3];

	/** Line width, stipple pattern, and color. */
	osg::ref_ptr<osg::LineWidth> _lineWidth; 
	osg::ref_ptr<osg::LineStipple>  _linePattern;
	float _lineColor[3];

	mutable bool _dataValid; // If trajectory supports required data
	mutable bool _dataZero; // If we are just drawing at the origin
};

}

#endif
