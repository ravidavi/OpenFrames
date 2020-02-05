/***********************************
   Copyright 2020 Ravishankar Mathur and Emergent Space Technologies, Inc.

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

/** \file PolyhedralCone.hpp
 * Declaration of PolyhedralCone class
 */

#ifndef _OF_RADIALPLANE_
#define _OF_RADIALPLANE_

#include <OpenFrames/Export.h>
#include <OpenFrames/ReferenceFrame.hpp>

namespace OpenFrames
{
  /**
   * \class PolyhedralCone
   *
   * \brief ReferenceFrame for drawing a polyhedral cone.
   *
   * A type of ReferenceFrame that draws a polyhedral cone, which is a finitely-generated cone
   *  *with specified clock and cone angles. The cone
   */
  class OF_EXPORT PolyhedralCone : public ReferenceFrame
  {
  public:
    PolyhedralCone( const std::string &name );
    PolyhedralCone( const std::string &name, float r, float g, float b, float a = 1.0 );

    // Show/hide this frame's contents, e.g. everything a frame shows (excluding axes, labels, and children)
    // Inherited from ReferenceFrame
    virtual void showContents(bool showContents);
    virtual bool getContentsShown() const;

    /** Set/get the clock and cone angles. */
    void setAngles(const osg::Vec3d& clockAngles, const osg::Vec3d& coneAngles);
    void getAngles(osg::Vec3d& clockAngles, osg::Vec3d& coneAngles) const;

    /** Set the color of the plane */
    void setConeColor(const osg::Vec4 &color);
    void setConeColor(float r, float g, float b, float a = 1.0);
    const osg::Vec4& getConeColor() const { return (*_planeColor)[0]; }

    /** Inherited from ReferenceFrame. */
    virtual const osg::BoundingSphere& getBound() const;

    /// Inherited
    virtual std::string frameInfo() const { return "PolyhedralCone"; }

  protected:
    virtual ~PolyhedralCone();

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
