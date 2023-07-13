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

/** \file EllipticCone.hpp
 * Declaration of EllipticCone class
 */

#ifndef _OF_ELLIPTICCONE_
#define _OF_ELLIPTICCONE_

#include <OpenFrames/Export.h>
#include <OpenFrames/PolyhedralCone.hpp>

namespace OpenFrames
{
  /**
   * \class EllipticCone
   *
   * \brief ReferenceFrame for drawing an elliptic cone
   *
   * A type of PolyhedralCone that draws an elliptic cone, with variable semimajor and semiminor angles.
   */
  class OF_EXPORT EllipticCone : public PolyhedralCone
  {
  public:
    EllipticCone( const std::string &name );
    EllipticCone( const std::string &name, float r, float g, float b, float a = 1.0 );

    /** Set/get semimajor/semiminor half-angles */
    void setPrimaryAngles(const double& a, const double& b);

    /// Compute visibility of point from this cone
    virtual bool isVisible(osg::Vec3d point, const double& minDistance = 0.0, const double& maxDistance = std::numeric_limits<double>::max()) const;

    /// Inherited
    virtual std::string frameInfo() const { return "EllipticCone"; }

  protected:
    virtual ~EllipticCone();

    void init();
    void createEllipticCone();

    double _a, _b; // Semimajor (a) and Semiminor (b) axes
  };

} // !namespace OpenFrames

#endif
