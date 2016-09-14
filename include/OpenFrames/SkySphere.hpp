/***********************************
   Copyright 2016 Ravishankar Mathur

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

#ifndef _OF_SKYSPHERE_
#define _OF_SKYSPHERE_

#include <OpenFrames/Export.h>
#include <OpenFrames/Sphere.hpp>

#include <array>

namespace OpenFrames {

/*******************************************
 * Ravi Mathur
 * OpenFrames API, class SkySphere
 * Extends OpenFrames::Sphere by adding ability to show a texture
 * and/or stars from a star catalog.
 * Textures are widely available online.
 * Star Catalogs must be text files with the following format: (TBD)
******************************************/
class OF_EXPORT SkySphere : public OpenFrames::Sphere {
  public:
    /** Indicates what elements to draw */
    enum DrawMode
    {
      NONE = 0,     // Don't draw anything
      TEXTURE = 1,  // Draw provided texture
      STARFIELD = 2 // Draw stars from provided star catalog
    };
    
    struct Star
    {
      float ra;   // Radians, 0 <= ra <= 2*PI
      float dec;  // Radians, -PI/2 <= dec <= PI/2
      float mag, colorindex;  // Nondim, unbounded
    };

    SkySphere(const std::string &name);

    ///
    /// Specify which elements to draw by combining elements of DrawMode
    void setDrawMode(unsigned int drawMode);
    unsigned int getDrawMode();

    ///
    /// Set the star catalog drawn as the starfield
    /// Note that magnitude and brightness are inverse, e.g. mag = -1 is
    /// brighter than mag = 1
    /// Note that stars are drawn according to their position in the
    /// catalog, so sorting the catalog allows drawing brightest/dimmest
    /// stars first.
    /// Limits: minMag < maxMag AND numStars >= 1
    bool setStarData(const std::string &catalogName, float minMag, float maxMag, unsigned int numStars, float starScale = 4.0);

    ///
    /// Convert a Star to a XYZ position, RGB color, and size (in color[3])
    static void StarToPoint(const Star &star, osg::Vec3 &pos, osg::Vec4 &color);

    // Get the Geometry bin to which a star should be assigned
    static unsigned int getStarBin(const osg::Vec3 &p);

  protected:
    virtual ~SkySphere(); // Must be allocated on heap using 'new'

    bool processStars(); // Load and set up all star data

    // Clear all stars
    void clearStars();

    std::string _starCatalogFile; // File containing star catalog
    float _minMag, _maxMag; // Range of drawn star magnitudes
    unsigned int _maxNumStars; // Maximum number of drawn stars
    float _starScale; // Pixel size scale

    static const unsigned int _starBinSpacing = 3;
    static const unsigned int _starBinCount = _starBinSpacing*_starBinSpacing*6;
    typedef std::array<osg::ref_ptr<osg::Geometry>, _starBinCount> StarBins;
    StarBins _starBinGeoms;

  private:
    void _init();
};

}  // !namespace OpenFrames

#endif  // !define _OF_SKYSPHERE_
