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
#include <OpenFrames/TrajectoryFollower.hpp>
#include <OpenFrames/FocalPointShadowMap.hpp>
#include <osg/PositionAttitudeTransform>
#include <osgDB/ReadFile>
#include <osgGA/TrackballManipulator>
#include <osgGA/GUIEventHandler>
#include <osgShadow/ShadowedScene>
#include <osgShadow/ShadowMap>
#include <osgShadow/MinimalShadowMap>
#include <osgShadow/ParallelSplitShadowMap>
#include <osgShadow/SoftShadowMap>
#include <osgShadow/StandardShadowMap>
#include <osgShadow/ViewDependentShadowMap>
#include <iostream>

using namespace OpenFrames;

class MoveModelHandler : public osgGA::GUIEventHandler
{
public:
  MoveModelHandler(osg::PositionAttitudeTransform *pat)
  : _pat(pat)
  {}
  
  virtual bool handle(osgGA::Event* event, osg::Object* object, osg::NodeVisitor* nv)
  {
    osgGA::GUIEventAdapter* ea = event->asGUIEventAdapter();
    if(ea)
    {
      if(ea->getEventType() == osgGA::GUIEventAdapter::KEYDOWN)
      {
        const double zoomRate = 0.001;
        if(ea->getKey() == osgGA::GUIEventAdapter::KEY_Up)
        {
          osg::Vec3d pos = _pat->getPosition();
          _pat->setPosition(pos + osg::Vec3d(-zoomRate, 0.0, 0.0));
          return true;
        }
        else if(ea->getKey() == osgGA::GUIEventAdapter::KEY_Down)
        {
          osg::Vec3d pos = _pat->getPosition();
          _pat->setPosition(pos + osg::Vec3d(zoomRate, 0.0, 0.0));
          return true;
        }
      }
    }
    
    return false;
  }
  
protected:
  osg::PositionAttitudeTransform* _pat;
};

static int ReceivesShadowTraversalMask = 0x1;
static int CastsShadowTraversalMask = 0x2;

int main()
{
  const double r_sun = 695508.0;
  const double AU = 1.496e8;
  
  // Shadowing properties
  osgShadow::ShadowedScene *shadowedScene = new osgShadow::ShadowedScene;
  osgShadow::ShadowSettings *shadowSettings = shadowedScene->getShadowSettings();
  shadowSettings->setReceivesShadowTraversalMask(ReceivesShadowTraversalMask);
  shadowSettings->setCastsShadowTraversalMask(CastsShadowTraversalMask);
  //shadowSettings->setDebugDraw(true);
  //shadowSettings->setShadowMapProjectionHint(osgShadow::ShadowSettings::ORTHOGRAPHIC_SHADOW_MAP);
  //shadowSettings->setMultipleShadowMapHint(osgShadow::ShadowSettings::CASCADED);
  int texSize = 1024;
  shadowSettings->setTextureSize(osg::Vec2s(texSize, texSize));
  //shadowSettings->setLightNum(0);
  //osgShadow::ShadowMap *sm = new osgShadow::ShadowMap;
  OpenFrames::FocalPointShadowMap *fpsm = new OpenFrames::FocalPointShadowMap;
  fpsm->setLightSize(r_sun);
  fpsm->setAmbientBias(osg::Vec2(0.0, 1.0));
  fpsm->setPolygonOffset(osg::Vec2(-1, -1));
  shadowedScene->setShadowTechnique(fpsm);
  
  //osgShadow::SoftShadowMap *ssm = new osgShadow::SoftShadowMap;
  //shadowedScene->setShadowTechnique(ssm);
  
  osg::LightSource* lightSource = new osg::LightSource;
  osg::Light* light = lightSource->getLight();
  light->setName("My Light");
  light->setPosition(osg::Vec4(0.1*AU, 0.0, 0.0, 1.0));
  light->setAmbient(osg::Vec4(0.0, 0.0, 0.0, 1.0));
  light->setDiffuse(osg::Vec4(4.0, 4.0, 4.0, 1.0));
  light->setSpecular(osg::Vec4(0.8, 0.8, 0.8, 1.0));
  
  osg::Node* model;
  model = osgDB::readNodeFile("Models/CSHP_DV_257_01_______00343.obj");
  //model = osgDB::readNodeFile("Models/ESA_Rosetta_OSIRIS_67P_SHAP2P.obj");
  {
    float length = 3.0f;
    
    osg::Vec3 widthVec(0.0, length, 0.0);
    osg::Vec3 heightVec(0.0, 0.0, length);
    osg::Vec3 centerBase(0.0, 0.0, 0.0f);
    
    osg::PositionAttitudeTransform* pat = new osg::PositionAttitudeTransform;
    //pat->setAttitude(osg::Quat(osg::PI/2.0 - 0.001, osg::Vec3d(0, 1, 0)));
    
    osg::Geometry *plane = osg::createTexturedQuadGeometry(centerBase-widthVec*0.5f-heightVec*0.5f, widthVec, heightVec );
    pat->addChild(plane);
    //model = pat;
  }
    
  model->setNodeMask(CastsShadowTraversalMask | ReceivesShadowTraversalMask);
  model->setNodeMask(ReceivesShadowTraversalMask);
  osg::Material* mat = new osg::Material;
  mat->setAmbient(osg::Material::FRONT_AND_BACK, osg::Vec4(0.0, 0.0, 0.0, 1.0));
  mat->setDiffuse(osg::Material::FRONT_AND_BACK, osg::Vec4(0.2, 0.2, 0.2, 1.0));
  mat->setSpecular(osg::Material::FRONT_AND_BACK, osg::Vec4(0.1, 0.1, 0.2, 1.0));
  mat->setShininess(osg::Material::FRONT_AND_BACK, 100);
  model->getOrCreateStateSet()->setAttributeAndModes(mat);
  
  double scale = 0.00001;
  double offset = 2.386;
  //offset = 0.01;
  osg::PositionAttitudeTransform* pat = new osg::PositionAttitudeTransform;
  pat->setScale(osg::Vec3d(scale, scale, scale));
  pat->setPosition(osg::Vec3d(offset, 0, 0));
  pat->setAttitude(osg::Quat(osg::PI/4.0, osg::Vec3d(0, 1, 0)));
  osg::Node* model2;
  if(true) model2 = osgDB::readNodeFile("Models/aura.3ds");
  else
  {
    osg::ShapeDrawable* sphereSD = new osg::ShapeDrawable;
    sphereSD->setName("SphereDrawable");
    sphereSD->setUseDisplayList(false);
    sphereSD->setUseVertexBufferObjects(true);
    sphereSD->getOrCreateStateSet(); // Will be used for textures and modes
    osg::Sphere* sphere = new osg::Sphere;
    sphere->setRadius(100.0);
    sphereSD->setShape(sphere);
    
    osg::Geode* geode = new osg::Geode;
    geode->addDrawable(sphereSD);
    model2 = geode;
  }
  model2->setNodeMask(CastsShadowTraversalMask | ReceivesShadowTraversalMask);
  pat->addChild(model2);
  
  shadowedScene->addChild(model);
  shadowedScene->addChild(pat);
  shadowedScene->addChild(lightSource);
  
  osgViewer::Viewer viewer;
  viewer.setSceneData(shadowedScene);
  
  osgGA::TrackballManipulator* tb = new osgGA::TrackballManipulator;
  tb->setHomePosition(osg::Vec3d(offset + 2.0*model2->getBound()._radius*scale, 0.0, 0.0), osg::Vec3d(offset, 0, 0), osg::Vec3d(0, 0, -1));
  //tb->setHomePosition(osg::Vec3d(0, -4.322, 0.75), osg::Vec3d(0, 0, 0.5), osg::Vec3d(0, 0, 1));
  //tb->setHomePosition(osg::Vec3d(0.05, 0.0, 0.0), osg::Vec3d(0, 0, 0), osg::Vec3d(0, 0, 1));
  tb->setMinimumDistance(0.0);

  viewer.setCameraManipulator(tb);
  viewer.addEventHandler(new MoveModelHandler(pat));
  viewer.realize();
  
  // Add debug HUD for ShadowMap
#if 1
  {
    OpenThreads::Thread::microSleep(500000);
    osg::GraphicsContext* gc = viewer.getCamera()->getGraphicsContext();
    osg::Viewport* vp = viewer.getCamera()->getViewport();
    
    osg::ref_ptr<osg::Camera> smDebugHUD = fpsm->makeDebugHUD();
    smDebugHUD->setGraphicsContext(gc);
    smDebugHUD->setViewport(new osg::Viewport(0, 0, 200, 200));
    //smDebugHUD->setRenderOrder(osg::Camera::POST_RENDER, hudCamera->getRenderOrderNum() + 1);
    //hudCamera->setNodeMask(0x0);
    //smDebugHUD->setClearColor(osg::Vec4(1, 0, 0, 1));
    viewer.addSlave(smDebugHUD.get(), false);
  }
#endif
  
  viewer.getCamera()->getGraphicsContext()->getState()->setUseModelViewAndProjectionUniforms(true);
  viewer.run();

  return 0;
}
