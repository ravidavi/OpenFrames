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

#include <OpenFrames/Model.hpp>
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
  sun->setLightSourceEnabled(true);
  osg::Light* sunLight = sun->getLightSource()->getLight();
  sunLight->setPosition(osg::Vec4(0.0, 0.0, 0.0, 1.0)); // At center of Sun
  sunLight->setAmbient(osg::Vec4(0.4, 0.4, 0.4, 1.0));
  sunLight->setDiffuse(osg::Vec4(2.0, 2.0, 2.0, 1.0)); // Bright sun!
  sunLight->setSpecular(osg::Vec4(0.8, 0.8, 0.8, 1.0));
  
  // Create a Model for the Comet
  Model* cg = new Model("67P_CG");
  cg->showAxes(ReferenceFrame::NO_AXES);
  cg->showAxesLabels(ReferenceFrame::NO_AXES);
  cg->showNameLabel(false);
  cg->setModel("Models/Comet67P_CG.3ds");
  cg->setPosition(100, 100, 100);
  mat = new osg::Material;
  mat->setAmbient(osg::Material::FRONT_AND_BACK, osg::Vec4(0.0, 0.0, 0.0, 1.0));
  mat->setDiffuse(osg::Material::FRONT_AND_BACK, osg::Vec4(0.25, 0.25, 0.25, 1.0));
  mat->setSpecular(osg::Material::FRONT_AND_BACK, osg::Vec4(0.1, 0.1, 0.2, 1.0));
  mat->setShininess(osg::Material::FRONT_AND_BACK, 100);
  cg->getModel()->getOrCreateStateSet()->setAttributeAndModes(mat);
  earth->addChild(cg);
  
  // Create a manager to handle access to the scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(root);
  
  // Add the scene to the window
  myWindow->setScene(fm, 0, 0);
  
  // Set up sky background using the Gaia DR2 galactic disk texture and HYGv3 star database
  myWindow->getGridPosition(0, 0)->setBackgroundColor(0, 0, 0); // Black background
  myWindow->getGridPosition(0, 0)->setSkySphereTexture("Images/ESA_Gaia_DR2_2048.jpg");
  myWindow->getGridPosition(0, 0)->setSkySphereStarData("Stars/Stars_HYGv3.txt", -2.0, 6.0, 40000); // At most 40000 stars of magnitude range [-2.0, 6.0] from the HYGv3 database
  
  // The Gaia image is in Galactic coordinates, so transform it to J2000 Equatorial coordinates
  // to match the HYGv3 coordinate system
  // Matrix that transforms ICRS (J2000 Equatorial) to Galactic coordinates
  // Source: https://gea.esac.esa.int/archive/documentation/GDR1/Data_processing/chap_cu3ast/sec_cu3ast_intro.html
  osg::Matrixd eq_to_gal_mat(-0.0548755604162154, +0.4941094278755837, -0.8676661490190047, 0.0,
                             -0.8734370902348850, -0.4448296299600112, -0.1980763734312015, 0.0,
                             -0.4838350155487132, +0.7469822444972189, +0.4559837761750669, 0.0,
                             0.0                , 0.0                , 0.0                , 1.0);
  osg::Quat eq_to_gal = eq_to_gal_mat.getRotate();
  
  // Quaternion that transforms Galactic to J2000 Equatorial coordinates
  osg::Quat gal_to_eq = eq_to_gal.inverse();

  // Gaia image is offset by 180 degrees as compared to the OpenFrames::Sphere texture wrapping
  // convention, so rotate it by 180 degrees before performing the Galatic->J2000 transformation
  myWindow->getGridPosition(0, 0)->getSkySphere()->setSphereAttitude(osg::Quat(osg::PI, osg::Vec3d(0, 0, 1))*gal_to_eq);
  
  // Create views of the Earth and Moon
  View *viewEarth = new View(root, earth);
  View *viewMoon = new View(root, moon);
  View *viewSun = new View(root, sun);
  View *viewCG = new View(root, cg);
  myWindow->getGridPosition(0, 0)->addView(viewEarth);
  myWindow->getGridPosition(0, 0)->addView(viewMoon);
  myWindow->getGridPosition(0, 0)->addView(viewSun);
  myWindow->getGridPosition(0, 0)->addView(viewCG);
  
  myWindow->startThread(); // Start window animation
  myWindow->join(); // Wait for window animation to finish
  
  return 0;
}
