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

/** \file RectangularCone.hpp
 * Declaration of RectangularCone class
 */

#ifndef _OF_RECTANGULARCONE_
#define _OF_RECTANGULARCONE_

#include <OpenFrames/Export.h>
#include <OpenFrames/PolyhedralCone.hpp>

namespace OpenFrames
{
  /**
   * \class RectangularCone
   *
   * \brief ReferenceFrame for drawing an elliptic cone
   *
   * A type of PolyhedralCone that draws an elliptic cone, with variable x and y angles.
   */
  class OF_EXPORT RectangularCone : public PolyhedralCone
  {
  public:
    RectangularCone( const std::string &name );
    RectangularCone( const std::string &name, float r, float g, float b, float a = 1.0 );

    /** Set/get x and y half-angles */
    void setPrimaryAngles(const double& x, const double& y);

    /// Inherited
    virtual std::string frameInfo() const { return "RectangularCone"; }

  protected:
    virtual ~RectangularCone();

    void init();
    void createRectangularCone();

    double _x, _y; // Angles along x and y axes
  };

} // !namespace OpenFrames

#endif
