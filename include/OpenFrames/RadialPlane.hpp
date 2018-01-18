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

#ifndef _OF_RADIALPLANE_
#define _OF_RADIALPLANE_

#include <OpenFrames/Export.h>
#include <OpenFrames/ReferenceFrame.hpp>

namespace OpenFrames
{

/**********************************************************
 * Ravi Mathur
 * OpenFrames API, class RadialPlane
 * A type of ReferenceFrame that draws a radial x-y plane
 * with adjustable radial circles and longitude lines.
**********************************************************/
class OF_EXPORT RadialPlane : public ReferenceFrame
{
  public:
	RadialPlane( const std::string &name );
	RadialPlane( const std::string &name, float r, float g, float b, float a = 1.0 );

	/** Set/get the spacing between radial circles. */
	void setParameters(const double &radius, const double &radSpace, const double &lonSpace);
	void getParameters(double &radius, double &radSpace, double &lonSpace) const;

	/** Set the color of the plane */
	void setPlaneColor(const osg::Vec4 &color);
	void setPlaneColor(float r, float g, float b, float a = 1.0);

	/** Set the color of the radial/longitudinal lines on the plane */
	void setLineColor(const osg::Vec4 &color);
	void setLineColor(float r, float g, float b, float a = 1.0);

        /** Inherited from ReferenceFrame. */
	virtual const osg::BoundingSphere& getBound() const;

  protected:
	virtual ~RadialPlane();

	void init();
	void createPlane();
	void addCell(const osg::Vec2d v[], unsigned int nV);

	double _radius;           // Radius of plane
	double _radialSpacing;    // Distance between radial circles
	double _longitudeSpacing; // Angular distance between longitude lines

	osg::ref_ptr<osg::Geode> _planeGeode;  // Node to hold plane geometry
	osg::ref_ptr<osg::Geode> _linesGeode;  // Node to hold lines geometry
	
	// Geometry objects that draw each of the overlaid lines
	osg::ref_ptr<osg::Geometry> _linesGeom;
	osg::ref_ptr<osg::Geometry> _lonGeom; // For 0 degree longitude line

	// Array of points that define the plane and the lines
	osg::ref_ptr<osg::Vec2dArray> _lineVertices;
	osg::ref_ptr<osg::Vec2dArray> _planeVertices;

	// Arrays that define plane and line colors
	osg::ref_ptr<osg::Vec4Array> _lineColor;
	osg::ref_ptr<osg::Vec4Array> _planeColor;

	// Array defining normals
	osg::ref_ptr<osg::Vec3Array> _normals;

	// Level of Detail for cells that make up the plane tesselation
	unsigned int _planeLOD;
};

} // !namespace OpenFrames

#endif
