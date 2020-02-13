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
    void setAngles(const AngleArray& clockAngles, const AngleArray& coneAngles);
    const AngleArray& getClockAngles() const { return _clockAngles; }
    const AngleArray& getConeAngles() const { return _coneAngles; }

    /** Set/get the cone principal axis length */
    void setLength(const double& length);
    double getLength() const;

    /** Set the color of the plane */
    void setConeColor(const osg::Vec4 &color);
    void setConeColor(float r, float g, float b, float a = 1.0);
    const osg::Vec4& getConeColor() const { return (*_coneColor)[0]; }
  
    /** Define cone axis in local coordinate system
     Cone axis points from apex towards base */
    void setConeAxis(const osg::Vec3d& axis);

    /** Inherited from ReferenceFrame. */
    virtual const osg::BoundingSphere& getBound() const;

    /// Inherited
    virtual std::string frameInfo() const { return "PolyhedralCone"; }

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
    
    // Geometry object that draw each triangle of the cone
    osg::ref_ptr<osg::Geometry> _coneGeom;

    // Array of points that define the cone
    osg::ref_ptr<osg::Vec3dArray> _coneVertices;

    // Cone colors
    osg::ref_ptr<osg::Vec4Array> _coneColor;
  };

} // !namespace OpenFrames

#endif
