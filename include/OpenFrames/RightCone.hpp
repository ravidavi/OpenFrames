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

/** \file RightCone.hpp
 * Declaration of RightCone class
 */

#ifndef _OF_RIGHTCONE_
#define _OF_RIGHTCONE_

#include <OpenFrames/Export.h>
#include <OpenFrames/PolyhedralCone.hpp>

namespace OpenFrames
{
  /**
   * \class RightCone
   *
   * \brief ReferenceFrame for drawing a right cone
   *
   * A type of PolyhedralCone that draws a right cone, with variable semimajor and semiminor angles.
   */
  class OF_EXPORT RightCone : public PolyhedralCone
  {
  public:
    RightCone( const std::string &name );
    RightCone( const std::string &name, float r, float g, float b, float a = 1.0 );

    /** Set/get semimajor/semiminor angles */
    void setPrimaryAngles(const double& a, const double& b);

    /// Inherited
    virtual std::string frameInfo() const { return "RightCone"; }

  protected:
    virtual ~RightCone();

    void init();
    void createRightCone();

    double _a, _b; // Semimajor (a) and Semiminor (b) axes
  };

} // !namespace OpenFrames

#endif
