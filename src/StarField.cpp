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

#include <OpenFrames/StarField.hpp>

namespace OpenFrames{

StarField::StarField()
{
  osg::Geode *geode;
  osg::StateSet *ss;

  // Create OpenFrames::Sphere that will show the starmap texture
  _starSphere = new Sphere("Star Sphere");

  // Hide its axes and labels
  _starSphere->showAxes(ReferenceFrame::NO_AXES);
  _starSphere->showAxesLabels(ReferenceFrame::NO_AXES);
  _starSphere->showNameLabel(false);

  // Make sure it follows the eye
  _starSphere->getTransform()->setFollowEye(true);

  // Disable lighting and depth testing
  geode = _starSphere->getSphere();
  ss = geode->getOrCreateStateSet();
  ss->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
  ss->setMode(GL_DEPTH_TEST, osg::StateAttribute::OFF);

  // Draw before other objects
  ss->setRenderBinDetails(-1, "RenderBin");

  // Make sure starmap sphere is never culled
  geode->setCullingActive(false);
}

bool StarField::setTextureMap(const std::string &fname)
{
  return _starSphere->setTextureMap(fname);
}

osg::Group* StarField::getStarField()
{
  return _starSphere->getGroup();
}

StarField::~StarField() {}

} // !namespace OpenFrames
