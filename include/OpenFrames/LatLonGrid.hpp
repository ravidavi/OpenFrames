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

/** \file LatLonGrid.hpp
 * Declaration of LatLonGrid class.
 */

#ifndef _OF_LATLONGRID_
#define _OF_LATLONGRID_

#include <OpenFrames/Export.h>
#include <OpenFrames/ReferenceFrame.hpp>
#include <osg/Geometry>
#include <osg/ref_ptr>

namespace OpenFrames
{
  /**
   * \class LatLonGrid
   *
   * \brief This class draws an ellipsoidal lat-lon grid.
   *
   * This class implements a type of ReferenceFrame that draws an ellipsoidal
   * latitude/longitude grid with adjustable radii and spacings.
   */
  class OF_EXPORT LatLonGrid : public ReferenceFrame
  {
  public:
	LatLonGrid( const std::string &name );
	LatLonGrid( const std::string &name, float r, float g, float b, float a = 1.0 );

	// Show/hide this frame's contents, e.g. everything a frame shows (excluding axes, labels, and children)
	// Inherited from ReferenceFrame
	virtual void showContents(bool showContents);
	virtual bool getContentsShown() const;

	/** Set/get latitude and longitude spacing, in radians.
	    Enter zero to disable latitude or longitude lines, or any value above 2*PI to
	    draw only the equator or prime meridian. */
	void setParameters(const double &radiusX, const double &radiusY, const double &radiusZ, const double &latSpace, const double &lonSpace);
	void getParameters(double &radiusX, double &radiusY, double &radiusZ, double &latSpace, double &lonSpace) const;

        /** Inherited from ReferenceFrame. */
	virtual const osg::BoundingSphere& getBound() const;

	/** Set the color of all associated ReferenceFrame objects */
	virtual void setColor( const osg::Vec4 &color );
	using ReferenceFrame::setColor; // Unhide inherited setColor

  /// Inherited
  virtual std::string frameInfo() const { return "LatLonGrid"; }

  protected:
	virtual ~LatLonGrid();

	void _init();
	void _createGrid();

	double _latSpacing, _lonSpacing; // Grid line spacing, in radians
	double _radiusX, _radiusY, _radiusZ; // Radii in x, y, z directions

	osg::ref_ptr<osg::Geode> _geode;  // Node to hold grid geometry
	osg::ref_ptr<osg::Geometry> _gridGeom; // Geometry that draws everything
	osg::ref_ptr<osg::Geometry> _mainGeom; // For equator and prime meridian
	osg::ref_ptr<osg::Vec3Array> _vertices; // Vertices that define the grid
	osg::ref_ptr<osg::Vec4Array> _colors; // Colors
  };

} // !namespace OpenFrames

#endif
