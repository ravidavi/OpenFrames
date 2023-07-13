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

/** \file SkySphere.hpp
 * Declaration of SkySphere class.
 */

#ifndef _OF_SKYSPHERE_
#define _OF_SKYSPHERE_

#include <OpenFrames/Export.h>
#include <OpenFrames/Sphere.hpp>

#include <array>

namespace OpenFrames
{
  /**
   * \class SkySphere
   *
   * \brief Extends OpenFrames::Sphere.
   *
   * Extends OpenFrames::Sphere by adding ability to show a texture
   * and/or stars from a star catalog. Textures are widely
   * available online. Star Catalogs must be text files with the
   * following format: (TBD).
   */
  class OF_EXPORT SkySphere : public OpenFrames::Sphere
  {
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
    
    /// Copy properties of another SkySphere into this one
    SkySphere& operator = (const SkySphere& rhs);

    ///
    /// Specify which elements to draw by combining elements of DrawMode
    void setDrawMode(unsigned int drawMode);
    unsigned int getDrawMode() const;

    ///
    /// Set the star catalog text file drawn as the starfield
    /// Note that the catalog header MUST be:
    ///  "ra[_ralimit] dec mag ci proper" , where the associated columns are:
    ///   ra[_ralimit] (hours) Right Ascension. If "_ralimit" is provided then it will
    ///                        be used as the maximum instead of hours.
    ///                        e.g. "ra_360.0" for degrees
    ///   dec          (degrees) Declination
    ///   mag          Apparent visual magnitude (G-band)
    ///   ci           (optional) B-V color index
    ///   proper       (optional) Name or other string identifier
    ///
    /// Note that magnitude and brightness are inverse, e.g. mag = -1 is
    /// brighter than mag = 1
    /// Note that stars are drawn according to their position in the
    /// catalog, so sorting the catalog allows drawing brightest/dimmest
    /// stars first.
    /// Limits: minMag < maxMag AND numStars >= 1
    bool setStarData(const std::string &catalogName,
                     float minMag, float maxMag, unsigned int maxNumStars,
                     float minPixSize, float maxPixSize, float minDimRatio);
    void getStarData(std::string &catalogName,
                     float &minMag, float &maxMag, unsigned int &maxNumStars,
                     float &minPixSize, float &maxPixSize, float &minDimRatio);

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
    float _minPixSize, _maxPixSize; // Limits on final star pixel size
    float _minDimRatio; // Minimum dimming ratio for any star

    // Stars are grouped by position into star bins. Bins are arranged
    // in a cube superscribed on the star unit sphere. Each cube face
    // contains bins arranged in a square grid.
    // Note that each bin does not have an equal number of stars, since
    // the sphere->cube mapping is not evenly spaced and stars are not
    // uniformly distributed in the sky

    // Number of bin rows (and columns) per cube face
    static const unsigned int _starBinSpacing = 2;

    // Total number of bins for a 6-sided cube
    static const unsigned int _starBinCount = _starBinSpacing*_starBinSpacing*6;

    // Star bins containing each set of stars
    typedef std::array<osg::ref_ptr<osg::Geometry>, _starBinCount> StarBins;
    StarBins _starBinGeoms;
    osg::ref_ptr<osg::Geode> _starGeode; // Contains all StarBins

  private:
    void _init();
  };

}  // !namespace OpenFrames

#endif  // !define _OF_SKYSPHERE_
