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

/** \file CoordinateAxes.hpp
 * Declaration of CoordinateAxes class.
 */

#ifndef _OF_COORDINATEAXES_
#define _OF_COORDINATEAXES_

#include <OpenFrames/Export.h>
#include <OpenFrames/ReferenceFrame.hpp>
#include <osg/Geometry>
#include <osg/ref_ptr>
#include <osg/LineWidth>

namespace OpenFrames
{
  /**
   * \class CoordinateAxes
   *
   * \brief A type of ReferenceFrame that displays X, Y, and Z axes.
   *
   * This class is a type of ReferenceFrame that displays its X, Y and Z axes
   * with adjustable tick marks and labels.
   */
  class OF_EXPORT CoordinateAxes : public ReferenceFrame
  {
  public:
	CoordinateAxes( const std::string &name );
	CoordinateAxes( const std::string &name, float r, float g, float b, float a = 1.0 );

	// Show/hide this frame's contents, e.g. everything a frame shows (excluding axes, labels, and children)
	// Inherited from ReferenceFrame
	virtual void showContents(bool showContents);
	virtual bool getContentsShown() const;

	/** Set/get axis length */
	void setAxisLength(double len);
	inline double getAxisLength() { return _axisLength; }

	/** Set which axes to draw using ReferenceFrame::AxesType */
	void setDrawAxes(unsigned int axes);
	inline unsigned int getDrawAxes() const { return _drawAxes; }

	/** Set/get width of axes. */
	void setAxisWidth(float width);
	float getAxisWidth() const;
	
	/** Set/get tick spacing */
	void setTickSpacing(double major_sp, double minor_sp);
	inline double getMajorTickSpacing() const { return _majorTickSpacing; }
	inline double getMinorTickSpacing() const { return _minorTickSpacing; }

	/** Set/get tick size */
	void setTickSize(unsigned majorSize, unsigned int minorSize);
	
	/** Set tick image or shader */
	bool setTickImage(const std::string &fname);
        bool setTickShader(const std::string &fname );

	/** Inherited from ReferenceFrame. */
	virtual const osg::BoundingSphere& getBound() const;
	virtual void setColor( const osg::Vec4 &color );
	using ReferenceFrame::setColor;

  /// Inherited
  virtual std::string frameInfo() const { return "CoordinateAxes"; }

  protected:
	virtual ~CoordinateAxes();

	void _init();
	void _computeTickAttenuation();
	void _createAxes();

        /** Reset shader to default state */
        void resetTickShader();

	double _axisLength; // Length of each axis
	double _majorTickSpacing, _minorTickSpacing; // Tick mark spacing
	unsigned int _drawAxes; // Which axes to draw
	osg::ref_ptr<osg::LineWidth> _lineWidth;  // Width of axis lines.

	osg::ref_ptr<osg::Geode> _axesGeode; // Node to hold axes geometry
	osg::ref_ptr<osg::Geode> _tickGeode; // Node to hold tick geometry

	osg::ref_ptr<osg::Geometry> _axesGeom;  // Geometry that draws axes
	osg::ref_ptr<osg::Geometry> _majorTickGeom; // Major tick marks geom
	osg::ref_ptr<osg::Geometry> _minorTickGeom; // Minor tick marks geom

	osg::ref_ptr<osg::Vec3dArray> _vertices; // Vertices that define the lines
	osg::ref_ptr<osg::Vec4Array> _colors;   // Colors for each line

        osg::ref_ptr<osg::Shader> _fragShader; // Tick mark fragment shader
  };

} // !namespace OpenFrames

#endif
