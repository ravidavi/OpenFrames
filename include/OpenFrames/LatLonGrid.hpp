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

#ifndef _OF_LATLONGRID_
#define _OF_LATLONGRID_

#include <OpenFrames/Export.h>
#include <OpenFrames/ReferenceFrame.hpp>
#include <osg/Geometry>
#include <osg/ref_ptr>

namespace OpenFrames
{

/**********************************************************
 * Ravi Mathur
 * OpenFrames API, class LatLonGrid
 * A type of ReferenceFrame that draws a spherical latitude/longitude grid
 * with adjustable spacings.
**********************************************************/
class OF_EXPORT LatLonGrid : public ReferenceFrame
{
  public:
	LatLonGrid( const std::string &name );
	LatLonGrid( const std::string &name, float r, float g, float b, float a = 1.0 );

	/** Set/get latitude and longitude spacing, in radians.
	    Enter zero to disable latitude or longitude lines, or any value above 2*PI to
	    draw only the equator or prime meridian. */
	void setParameters(const double &radius, const double &latSpace, const double &lonSpace);
	void getParameters(double &radius, double &latSpace, double &lonSpace) const;

        /** Inherited from ReferenceFrame. */
	virtual const osg::BoundingSphere& getBound() const;

	/** Set the color of all associated ReferenceFrame objects */
	virtual void setColor( const osg::Vec4 &color );
	using ReferenceFrame::setColor; // Unhide inherited setColor

  protected:
	virtual ~LatLonGrid();

	void _init();
	void _createGrid();

	double _latSpacing, _lonSpacing; // Grid line spacing, in radians
	double _radius; // Radius of sphere

	osg::ref_ptr<osg::Geode> _geode;  // Node to hold grid geometry
	osg::ref_ptr<osg::Geometry> _gridGeom; // Geometry that draws everything
	osg::ref_ptr<osg::Geometry> _mainGeom; // For equator and prime meridian
	osg::ref_ptr<osg::Vec3Array> _vertices; // Vertices that define the grid
	osg::ref_ptr<osg::Vec4Array> _colors; // Colors
};

} // !namespace OpenFrames

#endif
