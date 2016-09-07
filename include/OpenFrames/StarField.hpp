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

#ifndef _OF_STARFIELD_
#define _OF_STARFIELD_

#include <OpenFrames/Export.h>
#include <OpenFrames/Sphere.hpp>

#include <osg/Referenced>
#include <osg/ref_ptr>

namespace OpenFrames {

/*******************************************
 * Ravi Mathur
 * OpenFrames API, class StarField
 * Encapsulate a star field by specifying a texture and/or star catalog.
 * Textures are widely available online.
 * Star Catalogs must be CSV files with the following format: (TBD)
******************************************/
class OF_EXPORT StarField : public osg::Referenced {
  public:
	StarField();

        bool setTextureMap(const std::string &fname);
        osg::Group* getStarField();

  protected:
	virtual ~StarField(); // Must be allocated on heap using 'new'

  private:
        osg::ref_ptr<Sphere> _starSphere; // Draws starmap texture
        osg::ref_ptr<osg::Geode> _starField; // Draws star catalog
};

}  // !namespace OpenFrames

#endif  // !define _OF_STARFIELD_
