/***********************************
 Copyright 2018 Ravishankar Mathur
 
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

#include <OpenFrames/Sphere.hpp>
#include <OpenFrames/WindowProxy.hpp>

using namespace OpenFrames;

int main()
{
  const double r_earth = 6.371;
  const double r_moon = 1.737;
  const double r_sun = 695.7;
  
  // Create the interface that represents a window
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(30, 30, 640, 480, 1, 1, false, false);
  
  // Create a ReferenceFrame for the root
  ReferenceFrame* root = new ReferenceFrame("Root");
  root->showAxes(ReferenceFrame::NO_AXES);
  root->showAxesLabels(ReferenceFrame::NO_AXES);
  root->showNameLabel(false);
  
  // Create a Sphere for the Earth
  Sphere* earth = new Sphere("Earth");
  earth->setColor(0.0, 0.0, 1.0, 1.0);
  earth->showAxes(ReferenceFrame::NO_AXES);
  earth->showAxesLabels(ReferenceFrame::NO_AXES);
  earth->showNameLabel(false);
  earth->setTextureMap("Images/EarthTexture.bmp");
  earth->setRadius(r_earth);
  root->addChild(earth);
  
  // Set Earth material
  // Earth has ambient, diffuse, and specular reflections
  osg::Material* mat = new osg::Material;
  mat->setAmbient(osg::Material::FRONT_AND_BACK, osg::Vec4(0.75, 0.75, 0.75, 1.0));
  mat->setDiffuse(osg::Material::FRONT_AND_BACK, osg::Vec4(0.5, 0.5, 0.5, 1.0));
  mat->setSpecular(osg::Material::FRONT_AND_BACK, osg::Vec4(1.0, 1.0, 1.0, 1.0));
  mat->setShininess(osg::Material::FRONT_AND_BACK, 100.0);
  earth->setMaterial(mat);
  
  // Create a Sphere for the Moon
  Sphere* moon = new Sphere("Moon");
  moon->setColor(0.5, 0.5, 0.5, 1.0);
  moon->showAxes(ReferenceFrame::NO_AXES);
  moon->showAxesLabels(ReferenceFrame::NO_AXES);
  moon->showNameLabel(false);
  moon->setTextureMap("Images/MoonTexture.bmp");
  moon->setRadius(r_moon);
  moon->setPosition(10.0, -10.0, 0.0);
  root->addChild(moon);
  
  // Set Moon material
  // Moon has ambient & diffuse but no specular reflection
  mat = new osg::Material;
  mat->setAmbient(osg::Material::FRONT_AND_BACK, osg::Vec4(1.0, 1.0, 1.0, 1.0));
  mat->setDiffuse(osg::Material::FRONT_AND_BACK, osg::Vec4(1.0, 1.0, 1.0, 1.0));
  moon->setMaterial(mat);
  
  // Create a manager to handle access to the scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(root);
  
  // Add the scene to the window
  myWindow->setScene(fm, 0, 0);
  myWindow->getGridPosition(0, 0)->setBackgroundColor(0, 0, 0); // Black background
  
  // Create a view of the Earth
  View *viewEarth = new View(root, earth);
  View *viewMoon = new View(root, moon);
  myWindow->getGridPosition(0, 0)->addView(viewEarth);
  myWindow->getGridPosition(0, 0)->addView(viewMoon);
  
  // Change global lighting to a skylight (from +Z axis)
  osg::LightSource* globalLightSource = myWindow->getGridPosition(0, 0)->getGlobalLightSource();
  globalLightSource->setReferenceFrame(osg::LightSource::RELATIVE_RF);
  globalLightSource->getLight()->setPosition(osg::Vec4(0, 0, 1, 0));
  globalLightSource->getLight()->setAmbient(osg::Vec4(0.4, 0.4, 0.4, 1.0));
  globalLightSource->getLight()->setDiffuse(osg::Vec4(1.0, 1.0, 1.0, 1.0));
  globalLightSource->getLight()->setSpecular(osg::Vec4(0.8, 0.8, 0.8, 1.0));
  
  myWindow->startThread(); // Start window animation
  myWindow->join(); // Wait for window animation to finish
  
  return 0;
}
