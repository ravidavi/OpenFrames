/***********************************
   Copyright 2023 Ravishankar Mathur and Emergent Space Technologies, Inc.

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

#ifndef _OF_POLYHEDRALCONE_
#define _OF_POLYHEDRALCONE_

#include <OpenFrames/Export.h>
#include <OpenFrames/ReferenceFrame.hpp>
#include <osg/PositionAttitudeTransform>
#include <limits>

namespace OpenFrames
{
  /**
   * \class PolyhedralCone
   *
   * \brief ReferenceFrame for drawing a polyhedral cone.
   *
   * A type of ReferenceFrame that draws a polyhedral cone, which is a finitely-generated cone
   *  *with specified clock and cone angles.
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
    typedef std::vector<double> AngleArray;
    void setVertexAngles(const AngleArray& clockAngles, const AngleArray& coneAngles);
    const AngleArray& getClockAngles() const { return _clockAngles; }
    const AngleArray& getConeAngles() const { return _coneAngles; }

    /** Set/get the cone principal axis length */
    void setConeLength(const double& length) { _coneTransform->setScale(osg::Vec3d(length,length,length)); }
    double getConeLength() const { return _coneTransform->getScale().x(); }

    /** Set the color of the cone faces */
    void setConeColor(const osg::Vec4 &color);
    void setConeColor(float r, float g, float b, float a = 1.0) { setConeColor(osg::Vec4(r, g, b, a)); }
    const osg::Vec4& getConeColor() const { return (*_coneColor)[0]; }

    /** Set the color of the cone lines */
    void setLineColor(const osg::Vec4 &color);
    void setLineColor(float r, float g, float b, float a = 1.0) { setLineColor(osg::Vec4(r, g, b, a));}
    const osg::Vec4& getLineColor() const { return (*_lineColor)[0]; }

    enum DrawMode
    {
      NONE = 0,
      SIDES = 1,
      EDGES = 2,
      BASE_OUTLINE = 4,
      DEFAULT = SIDES | EDGES | BASE_OUTLINE,
      ALL = SIDES | EDGES | BASE_OUTLINE,
    };

    /// Select components of cone that should be drawn
    void setDrawMode(unsigned int drawMode);
    unsigned int getDrawMode() const;

    /// Compute visibility of point from this cone
    virtual bool isVisible(osg::Vec3d point, const double& minDistance = 0.0, const double& maxDistance = std::numeric_limits<double>::max()) const { return false; }

    /** Inherited from ReferenceFrame. */
    virtual const osg::BoundingSphere& getBound() const;

    /// Inherited
    virtual std::string frameInfo() const { return "PolyhedralCone"; }
    
    /// Set up cone position/orientation to look in a specified direction
    void makeConeLookAt(const osg::Vec3d& apex, const osg::Vec3d& dir, const osg::Vec3d& up);

  protected:
    virtual ~PolyhedralCone();

    void init();
    void createCone();

    AngleArray _clockAngles; // List of mask clock angles
    AngleArray _coneAngles;  // List of mask cone angles

    // Transform that scales cone to its desired length
    // and points it in the desired direction
    osg::ref_ptr<osg::PositionAttitudeTransform> _coneTransform;
    
    // Node to hold cone geometry
    osg::ref_ptr<osg::Geode> _coneGeode;
    
    // Geometry objects that draw the cone and its outlines
    osg::ref_ptr<osg::Geometry> _sideGeom;
    osg::ref_ptr<osg::Geometry> _edgeGeom;
    osg::ref_ptr<osg::Geometry> _baseOutlineGeom;

    // Array of points that define the cone
    osg::ref_ptr<osg::Vec3dArray> _sideVertices;
    osg::ref_ptr<osg::Vec3dArray> _edgeVertices;
    osg::ref_ptr<osg::Vec3dArray> _baseVertices;

    // Cone colors
    osg::ref_ptr<osg::Vec4Array> _coneColor;
    osg::ref_ptr<osg::Vec4Array> _lineColor;
  };

} // !namespace OpenFrames

#endif
