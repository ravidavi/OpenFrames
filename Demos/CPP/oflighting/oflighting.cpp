/***********************************
 Copyright 2019 Ravishankar Mathur
 
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

#include <OpenFrames/FocalPointShadowMap.hpp>
#include <OpenFrames/Model.hpp>
#include <OpenFrames/Sphere.hpp>
#include <OpenFrames/WindowProxy.hpp>
#include <osgDB/ReadFile>
#include <osgGA/GUIEventHandler>

using namespace OpenFrames;

static int ReceivesShadowTraversalMask_earth = 0x1;
static int CastsShadowTraversalMask_earth = 0x2;
static int ReceivesShadowTraversalMask_comet = 0x1;
static int CastsShadowTraversalMask_comet = 0x2;
static unsigned int BaseShadowTexUnit_earth = 1;
static unsigned int BaseShadowTexUnit_comet = 1;

class MoveModelHandler : public osgGA::GUIEventHandler
{
public:
  MoveModelHandler(Model *model)
  : _model(model)
  {}
  
  virtual bool handle(osgGA::Event* event, osg::Object* object, osg::NodeVisitor* nv)
  {
    osgGA::GUIEventAdapter* ea = event->asGUIEventAdapter();
    if(ea)
    {
      if(ea->getEventType() == osgGA::GUIEventAdapter::KEYDOWN)
      {
        double delta = 10.0;
        if(ea->getModKeyMask() && osgGA::GUIEventAdapter::MODKEY_SHIFT) delta *= 0.1;
        
        if(ea->getKey() == '=' || ea->getKey() == '+')
        {
          osg::Vec3d pos;
          _model->getPosition(pos);
          _model->setPosition(pos + osg::Vec3d(delta, 0.0, 0.0));
          return true;
        }
        else if(ea->getKey() == '-' || ea->getKey() == '_')
        {
          osg::Vec3d pos;
          _model->getPosition(pos);
          _model->setPosition(pos - osg::Vec3d(delta, 0.0, 0.0));
          return true;
        }
        else if(ea->getKey() == osgGA::GUIEventAdapter::KEY_Up)
        {
          osg::Vec3d pos;
          _model->getPosition(pos);
          _model->setPosition(pos - osg::Vec3d(0.0, 0.0, delta));
          return true;
        }
        else if(ea->getKey() == osgGA::GUIEventAdapter::KEY_Down)
        {
          osg::Vec3d pos;
          _model->getPosition(pos);
          _model->setPosition(pos + osg::Vec3d(0.0, 0.0, delta));
          return true;
        }
        else if(ea->getKey() == osgGA::GUIEventAdapter::KEY_Left)
        {
          osg::Vec3d pos;
          _model->getPosition(pos);
          _model->setPosition(pos - osg::Vec3d(0.0, delta, 0.0));
          return true;
        }
        else if(ea->getKey() == osgGA::GUIEventAdapter::KEY_Right)
        {
          osg::Vec3d pos;
          _model->getPosition(pos);
          _model->setPosition(pos + osg::Vec3d(0.0, delta, 0.0));
          return true;
        }
      }
    }
    
    return false;
  }
  
protected:
  Model* _model;
};

int main()
{
  const double r_earth = 6371.0;
  const double r_moon = 1737.0;
  const double r_sun = 695000.7;
  const double d_sun = 149600000.0;
  const double d_moon = 384400.0;
  const double d_cg = 100000.0;
  
  // Create the interface that represents a window
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(30, 30, 1024, 768, 1, 1, false, false);
  
  // Create a ReferenceFrame for the root
  ReferenceFrame* root = new ReferenceFrame("Root");
  root->showAxes(ReferenceFrame::NO_AXES);
  root->showAxesLabels(ReferenceFrame::NO_AXES);
  root->showNameLabel(false);
  
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
  {
    osg::Material* mat = new osg::Material;
    mat->setAmbient(osg::Material::FRONT_AND_BACK, osg::Vec4(0, 0, 0, 1));
    mat->setDiffuse(osg::Material::FRONT_AND_BACK, osg::Vec4(0, 0, 0, 1));
    mat->setSpecular(osg::Material::FRONT_AND_BACK, osg::Vec4(0, 0, 0, 1));
    mat->setEmission(osg::Material::FRONT_AND_BACK, osg::Vec4(1, 1, 1, 1));
    sun->setMaterial(mat);
  }
  
  // Make the Sun a light source
  // By default this will use GL_LIGHT0 which overrides the global light
  // If using multiple lights, call light->setLightNum(num) with unique light numbers
  {
    sun->setLightSourceEnabled(true);
    osg::Light* sunLight = sun->getLightSource()->getLight();
    sunLight->setPosition(osg::Vec4(0.0, 0.0, 0.0, 1.0)); // At center of Sun
    sunLight->setAmbient(osg::Vec4(0.0, 0.0, 0.0, 1.0));
    sunLight->setDiffuse(osg::Vec4(2.0, 2.0, 2.0, 1.0)); // Bright sun!
    sunLight->setSpecular(osg::Vec4(0.8, 0.8, 0.8, 1.0));
  }
  
  // Create an empty ReferenceFrame to serve as the shadowed scene root for the comet
  ReferenceFrame* shadowedSceneRoot_earth = new ReferenceFrame("Shadow Scene Root - Earth");
  shadowedSceneRoot_earth->showAxes(ReferenceFrame::NO_AXES);
  shadowedSceneRoot_earth->showAxesLabels(ReferenceFrame::NO_AXES);
  shadowedSceneRoot_earth->showNameLabel(false);
  shadowedSceneRoot_earth->setShadowedSceneRoot(true);
  root->addChild(shadowedSceneRoot_earth);
  
  // Initialize shadowing info
  OpenFrames::FocalPointShadowMap *fpsm_earth;
  {
    osgShadow::ShadowedScene* shadowedScene = shadowedSceneRoot_earth->getShadowedSceneRoot();
    osgShadow::ShadowSettings *shadowSettings = shadowedScene->getShadowSettings();
    shadowSettings->setReceivesShadowTraversalMask(ReceivesShadowTraversalMask_earth);
    shadowSettings->setCastsShadowTraversalMask(CastsShadowTraversalMask_earth);
    shadowSettings->setBaseShadowTextureUnit(BaseShadowTexUnit_earth);
    int texSize = 1024;
    shadowSettings->setTextureSize(osg::Vec2s(texSize, texSize));
    
    fpsm_earth = new OpenFrames::FocalPointShadowMap;
    fpsm_earth->setLightSize(r_sun);
    fpsm_earth->setAmbientBias(osg::Vec2(0.0, 1.0));
    fpsm_earth->setPolygonOffset(osg::Vec2(-0.5, -0.5));
    fpsm_earth->setSceneScale(1.0/std::sqrt(3.0));
    shadowedScene->setShadowTechnique(fpsm_earth);
  }
  
  // Create a Sphere for the Earth
  Sphere* earth = new Sphere("Earth");
  earth->showAxes(ReferenceFrame::NO_AXES);
  earth->showAxesLabels(ReferenceFrame::NO_AXES);
  earth->showNameLabel(false);
  earth->setTextureMap("Images/land_shallow_topo_2048.jpg");
  //earth->setNightTextureMap("Images/land_ocean_ice_lights_2048.jpg");
  earth->setRadius(r_earth);
  earth->setAutoLOD(true);
  earth->getSphereTransform()->setNodeMask(ReceivesShadowTraversalMask_earth);
  shadowedSceneRoot_earth->addChild(earth);
  
  // Set Earth material, which overrides any color set for the Sphere
  // Use diffuse but no ambient reflections (so dark side can show night texture)
  {
    osg::Material* mat = new osg::Material;
    mat->setAmbient(osg::Material::FRONT_AND_BACK, osg::Vec4(0.0, 0.0, 0.0, 1.0));
    mat->setDiffuse(osg::Material::FRONT_AND_BACK, osg::Vec4(1.0, 1.0, 1.0, 1.0));
    mat->setSpecular(osg::Material::FRONT_AND_BACK, osg::Vec4(0.5, 0.5, 0.5, 1.0));
    mat->setShininess(osg::Material::FRONT_AND_BACK, 100);
    earth->setMaterial(mat);
  }
  
  // Create a Sphere for the Moon
  Sphere* moon = new Sphere("Moon");
  moon->showAxes(ReferenceFrame::NO_AXES);
  moon->showAxesLabels(ReferenceFrame::NO_AXES);
  moon->showNameLabel(false);
  moon->setTextureMap("Images/MoonTexture.bmp");
  moon->setRadius(r_moon);
  moon->setPosition(-d_moon, 0.0, 0.0);
  moon->getSphereTransform()->setNodeMask(CastsShadowTraversalMask_earth | ReceivesShadowTraversalMask_earth);
  earth->addChild(moon);
  
  // Set Moon material
  // Use diffuse but no ambient reflections (dark side of Moon will be black)
  {
    osg::Material* mat = new osg::Material;
    mat->setAmbient(osg::Material::FRONT_AND_BACK, osg::Vec4(0.0, 0.0, 0.0, 1.0));
    mat->setDiffuse(osg::Material::FRONT_AND_BACK, osg::Vec4(1.0, 1.0, 1.0, 1.0));
    moon->setMaterial(mat);
  }
  
  // Create an empty ReferenceFrame to serve as the shadowed scene root for the comet
  ReferenceFrame* shadowedSceneRoot_comet = new ReferenceFrame("Shadow Scene Root - Comet");
  shadowedSceneRoot_comet->showAxes(ReferenceFrame::NO_AXES);
  shadowedSceneRoot_comet->showAxesLabels(ReferenceFrame::NO_AXES);
  shadowedSceneRoot_comet->showNameLabel(false);
  shadowedSceneRoot_comet->setShadowedSceneRoot(true);
  shadowedSceneRoot_comet->setPosition(0.0, 0.0, d_cg);
  root->addChild(shadowedSceneRoot_comet);
  
  // Initialize shadowing info
  OpenFrames::FocalPointShadowMap *fpsm_comet;
  {
    osgShadow::ShadowedScene* shadowedScene = shadowedSceneRoot_comet->getShadowedSceneRoot();
    osgShadow::ShadowSettings *shadowSettings = shadowedScene->getShadowSettings();
    shadowSettings->setReceivesShadowTraversalMask(ReceivesShadowTraversalMask_comet);
    shadowSettings->setCastsShadowTraversalMask(CastsShadowTraversalMask_comet);
    shadowSettings->setBaseShadowTextureUnit(BaseShadowTexUnit_comet);
    int texSize = 1024;
    shadowSettings->setTextureSize(osg::Vec2s(texSize, texSize));
    
    fpsm_comet = new OpenFrames::FocalPointShadowMap;
    fpsm_comet->setLightSize(r_sun);
    fpsm_comet->setAmbientBias(osg::Vec2(0.0, 1.0));
    fpsm_comet->setPolygonOffset(osg::Vec2(-0.5, -0.5));
    //shadowedScene->setShadowTechnique(fpsm_comet);
  }
  
  // Create a Model for the Comet
  Model* cg = new Model("67P_CG");
  cg->showAxes(ReferenceFrame::NO_AXES);
  cg->showAxesLabels(ReferenceFrame::NO_AXES);
  cg->showNameLabel(false);
  //cg->setModel("Models/Comet67P_CG.3ds");
  //cg->setModel("Models/CSHP_DV_257_01_______00343.obj");
  //cg->setModel("Models/halley.3ds");
  //cg->setModel("Models/BennuRadarShape.obj");
  cg->setModel("Models/Bennu_v20_200k.obj");
  double cgScale = 1000.0;
  cg->setModelScale(cgScale, cgScale, cgScale);
  //cg->getModel()->setNodeMask(CastsShadowTraversalMask_comet | ReceivesShadowTraversalMask_comet);
  cg->getModel()->setNodeMask(ReceivesShadowTraversalMask_comet);
  shadowedSceneRoot_comet->addChild(cg);

  // Set comet material
  {
    osg::Material* mat = new osg::Material;
    mat->setAmbient(osg::Material::FRONT_AND_BACK, osg::Vec4(0.0, 0.0, 0.0, 1.0));
    mat->setDiffuse(osg::Material::FRONT_AND_BACK, osg::Vec4(0.25, 0.25, 0.25, 1.0));
    mat->setSpecular(osg::Material::FRONT_AND_BACK, osg::Vec4(0.1, 0.1, 0.2, 1.0));
    mat->setShininess(osg::Material::FRONT_AND_BACK, 100);
    cg->getModel()->getOrCreateStateSet()->setAttributeAndModes(mat, osg::StateAttribute::ON);
  }
  
  // Create a model for spacecraft near comet
  Model* sc = new Model("SC");
  sc->showAxes(ReferenceFrame::NO_AXES);
  sc->showAxesLabels(ReferenceFrame::NO_AXES);
  sc->showNameLabel(false);
  sc->setModel("Models/OsirisRex-2013-comp.lwo");
  sc->setPosition(-cg->getBound()._radius, 0.0, 0.0);
  sc->setAttitude(osg::Quat(osg::PI/2.0, osg::Vec3(0.0, 1.0, 0.0)));
  double scScale = 0.001;
  sc->setModelScale(scScale, scScale, scScale);
  sc->addDraggerCallback(nullptr);
  sc->getDragger()->setNodeMask(ReceivesShadowTraversalMask_comet);
  sc->getModel()->setNodeMask(CastsShadowTraversalMask_comet | ReceivesShadowTraversalMask_comet);
  shadowedSceneRoot_comet->addChild(sc);
  
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
  // Source: Gaia Data Release 1 (DR1), Documentation Release 1.2, Section 3.1.7 (Eqn 3.11).
  //         https://gea.esac.esa.int/archive/documentation/GDR1/Data_processing/chap_cu3ast/sec_cu3ast_intro.html
  // Source: Lui et al, "Reconsidering the Galactic Coordinate System", Astronomy & Astrophysics 526, A16, 2011.
  //         https://www.aanda.org/articles/aa/pdf/2011/02/aa14961-10.pdf (See equation for matrix N_Hip in Section 2.2)
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
  View *viewSC = new View(root, sc);
  myWindow->getGridPosition(0, 0)->addView(viewEarth);
  myWindow->getGridPosition(0, 0)->addView(viewMoon);
  myWindow->getGridPosition(0, 0)->addView(viewSun);
  myWindow->getGridPosition(0, 0)->addView(viewCG);
  myWindow->getGridPosition(0, 0)->addView(viewSC);
  
  // Install event handler to move model
  myWindow->getGridPosition(0, 0)->getSceneView()->addEventHandler(new MoveModelHandler(sc));
  
  myWindow->startThread(); // Start window animation
  
  // Add debug HUD for ShadowMap
#if 0
  while(!myWindow->isAnimating()) OpenThreads::Thread::YieldCurrentThread();
  
  {
    OpenThreads::Thread::microSleep(500000);
    osg::ref_ptr<osg::Camera> hudCam = myWindow->getGridPosition(0, 0)->getHUD();
    osg::GraphicsContext* gc = hudCam->getGraphicsContext();
    
    osg::ref_ptr<osg::Camera> smDebugHUD = fpsm_earth->makeDebugHUD();
    smDebugHUD->setGraphicsContext(gc);
    smDebugHUD->setViewport(new osg::Viewport(0, 0, 400, 400));
    hudCam->addChild(smDebugHUD);
  }
#endif
  
  myWindow->join(); // Wait for window animation to finish
  
  return 0;
}
