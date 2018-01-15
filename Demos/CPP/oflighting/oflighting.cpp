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
  const double d_sun = 149600;
  
  // Create the interface that represents a window
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(30, 30, 640, 480, 1, 1, false, false);
  
  // Create a ReferenceFrame for the root
  ReferenceFrame* root = new ReferenceFrame("Root");
  root->showAxes(ReferenceFrame::NO_AXES);
  root->showAxesLabels(ReferenceFrame::NO_AXES);
  root->showNameLabel(false);
  
  // Create a Sphere for the Earth
  Sphere* earth = new Sphere("Earth");
  earth->showAxes(ReferenceFrame::NO_AXES);
  earth->showAxesLabels(ReferenceFrame::NO_AXES);
  earth->showNameLabel(false);
  earth->setTextureMap("Images/land_shallow_topo_2048.jpg");
  earth->setNightTextureMap("Images/land_ocean_ice_lights_2048.jpg");
  earth->setRadius(r_earth);
  earth->setAutoLOD(true);
  root->addChild(earth);
  
  // Set Earth material, which overrides any color set for the Sphere
  // Use diffuse but no ambient reflections (so dark side can show night texture)
  osg::Material* mat = new osg::Material;
  mat->setAmbient(osg::Material::FRONT_AND_BACK, osg::Vec4(0.0, 0.0, 0.0, 1.0));
  mat->setDiffuse(osg::Material::FRONT_AND_BACK, osg::Vec4(1.0, 1.0, 1.0, 1.0));
  mat->setSpecular(osg::Material::FRONT_AND_BACK, osg::Vec4(0.5, 0.5, 0.5, 1.0));
  mat->setShininess(osg::Material::FRONT_AND_BACK, 100);
  earth->setMaterial(mat);
  
  // Create a Sphere for the Moon
  Sphere* moon = new Sphere("Moon");
  moon->showAxes(ReferenceFrame::NO_AXES);
  moon->showAxesLabels(ReferenceFrame::NO_AXES);
  moon->showNameLabel(false);
  moon->setTextureMap("Images/MoonTexture.bmp");
  moon->setRadius(r_moon);
  moon->setPosition(10.0, -10.0, 0.0);
  earth->addChild(moon);
  
  // Set Moon material
  // Use diffuse but no ambient reflections (dark side of Moon will be black)
  mat = new osg::Material;
  mat->setAmbient(osg::Material::FRONT_AND_BACK, osg::Vec4(0.0, 0.0, 0.0, 1.0));
  mat->setDiffuse(osg::Material::FRONT_AND_BACK, osg::Vec4(1.0, 1.0, 1.0, 1.0));
  moon->setMaterial(mat);
  
  // Create a Sphere for the Sun
  Sphere* sun = new Sphere("Sun");
  sun->showAxes(ReferenceFrame::NO_AXES);
  sun->showAxesLabels(ReferenceFrame::NO_AXES);
  sun->showNameLabel(false);
  sun->setTextureMap("Images/SunTexture.jpg");
  sun->setRadius(r_sun);
  sun->setPosition(-d_sun, 0.0, 0.0);
  root->addChild(sun);
  
  // Set Sun material
  // Sun has emission but no reflection
  mat = new osg::Material;
  mat->setAmbient(osg::Material::FRONT_AND_BACK, osg::Vec4(0, 0, 0, 1));
  mat->setDiffuse(osg::Material::FRONT_AND_BACK, osg::Vec4(0, 0, 0, 1));
  mat->setSpecular(osg::Material::FRONT_AND_BACK, osg::Vec4(0, 0, 0, 1));
  mat->setEmission(osg::Material::FRONT_AND_BACK, osg::Vec4(1, 1, 1, 1));
  sun->setMaterial(mat);
  
  // Make the Sun a light source
  // By default this will use GL_LIGHT0 which overrides the global light
  // If using multiple lights, call light->setLightNum(num) with unique light numbers
  osg::Light* sunLight = sun->setLightSourceEnabled(true)->getLight();
  sunLight->setPosition(osg::Vec4(0.0, 0.0, 0.0, 1.0)); // At center of Sun
  sunLight->setAmbient(osg::Vec4(0.4, 0.4, 0.4, 1.0));
  sunLight->setDiffuse(osg::Vec4(2.0, 2.0, 2.0, 1.0)); // Bright sun!
  sunLight->setSpecular(osg::Vec4(0.8, 0.8, 0.8, 1.0));
  
  // Create a manager to handle access to the scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(root);
  
  // Add the scene to the window
  myWindow->setScene(fm, 0, 0);
  myWindow->getGridPosition(0, 0)->setBackgroundColor(0, 0, 0); // Black background
  
  // Create views of the Earth and Moon
  View *viewEarth = new View(root, earth);
  View *viewMoon = new View(root, moon);
  View *viewSun = new View(root, sun);
  myWindow->getGridPosition(0, 0)->addView(viewEarth);
  myWindow->getGridPosition(0, 0)->addView(viewMoon);
  myWindow->getGridPosition(0, 0)->addView(viewSun);
  
  myWindow->startThread(); // Start window animation
  myWindow->join(); // Wait for window animation to finish
  
  return 0;
}
