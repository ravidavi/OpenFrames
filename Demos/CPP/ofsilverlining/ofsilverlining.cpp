#include <OpenFrames/Model.hpp>
#include <OpenFrames/ReferenceFrame.hpp>
#include <OpenFrames/VRUtils.hpp>
#include <OpenFrames/WindowProxy.hpp>
#include <osg/io_utils>
#include <osg/Depth>
#include <osg/TexEnv>
#include <osg/TexGen>
#include <osg/LightModel>
#include <osg/LightSource>
#include <osg/MatrixTransform>
#include <osgDB/ReadFile>
#include <osgGA/StateSetManipulator>
#include <osgViewer/ViewerEventHandlers>
#include <osgViewer/Viewer>
#include <iostream>
#include "SilverLiningNode.hpp"

// Create your own SilverLining envrionment class
class MySilverLiningNode : public SilverLiningNode
{
public:
  MySilverLiningNode(const char* licenseUser, const char* licenseKey)
    : SilverLiningNode(licenseUser, licenseKey) {}

  virtual void createAtmosphereData(osg::RenderInfo& renderInfo)
  {
    // Set our location (will be changed dynamically with viewpoint)
    SilverLining::Location loc;
    loc.SetAltitude(0.0);
    loc.SetLatitude(39.0);
    loc.SetLongitude(-76.8);
    _atmosphere->GetConditions()->SetLocation(loc);

    // Set the time to local time
    SilverLining::LocalTime t;
    t.SetFromSystemTime();
    //t.SetHour(15.0);
    //t.SetTimeZone(EST);
    _atmosphere->GetConditions()->SetTime(t);

    // Create cloud layers
    SilverLining::CloudLayer* cumulusCongestusLayer = SilverLining::CloudLayerFactory::Create(CUMULUS_CONGESTUS);
    cumulusCongestusLayer->SetIsInfinite(true);
    cumulusCongestusLayer->SetBaseAltitude(1500);
    cumulusCongestusLayer->SetThickness(200);
    cumulusCongestusLayer->SetBaseLength(40000);
    cumulusCongestusLayer->SetBaseWidth(40000);
    cumulusCongestusLayer->SetDensity(0.3);

    // Note, we pass in X and -Y since this accepts "east" and "south" coordinates.
    cumulusCongestusLayer->SetLayerPosition(_cameraPos.x(), -_cameraPos.y());
    cumulusCongestusLayer->SeedClouds(*_atmosphere);
    cumulusCongestusLayer->GenerateShadowMaps(false);
    atmosphere()->GetConditions()->AddCloudLayer(cumulusCongestusLayer);
  }
};

int StartOSGViewer(int argc, char** argv)
{
  osg::ArgumentParser arguments(&argc, argv);

  osg::Node* model = osgDB::readNodeFiles(arguments);
  if (!model) model = osgDB::readNodeFile("lz.osg");

  osg::ref_ptr<osg::LightSource> ls = new osg::LightSource;
  ls->getLight()->setLightNum(0);
  ls->getLight()->setPosition(osg::Vec4(0.5f, 0.5f, 0.5f, 0.0f));
  ls->getLight()->setAmbient(osg::Vec4(0.2f, 0.2f, 0.2f, 1.0f));
  ls->getLight()->setDiffuse(osg::Vec4(0.49f, 0.465f, 0.494f, 1.0f));
  ls->getLight()->setSpecular(osg::Vec4(1.0f, 0.98f, 0.95f, 1.0f));

  osg::ref_ptr<SilverLiningNode> silverLining = new MySilverLiningNode("Your user name", "Your license code");
  silverLining->setGlobalLight(ls->getLight());  // The light will be changed on the fly by SilverLining

  osg::ref_ptr<osg::MatrixTransform> scene = new osg::MatrixTransform;
  scene->addChild(model);
  scene->addChild(silverLining.get());

  osgViewer::Viewer viewer;
  viewer.getCamera()->setClearMask(GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);  // No need for OSG to clear the color buffer now
  viewer.getCamera()->setNearFarRatio(0.00001);
  viewer.getCamera()->setCullingMode(osg::CullSettings::NO_CULLING);
  viewer.addEventHandler(new osgGA::StateSetManipulator(viewer.getCamera()->getOrCreateStateSet()));
  viewer.addEventHandler(new osgViewer::StatsHandler);
  viewer.addEventHandler(new osgViewer::WindowSizeHandler);
  viewer.setSceneData(scene.get());
  viewer.setUpViewOnSingleScreen(0);
  return viewer.run();
}

int StartOFViewer(int argc, char** argv)
{
  // Parse user inputs
  osg::ArgumentParser arguments(&argc, argv);

  // Use VR mode
  bool useVR = arguments.read("--vr");
  unsigned int width = 800, height = 600;
  if (useVR) // Switch to VR mirror window size
  {
    width = 1080 / 2;
    height = 1200 / 2;
  }

  // Set VR world units per meter scale
  double worldUnitsPerMeter = 1.0;
  arguments.read("--vrScale", worldUnitsPerMeter);
  if (worldUnitsPerMeter < 1.0)
  {
    OSG_NOTICE << "VR WorldUnits/Meter ratio must be >= 1.0. Setting to 1.0" << std::endl;
    worldUnitsPerMeter = 1.0;
  }

  // Remaining options are unrecognized
  arguments.reportRemainingOptionsAsUnrecognized();

  // Get model filename
  std::vector<std::string> files;
  for (int pos = 1; pos < arguments.argc(); ++pos)
  {
    if (arguments.isString(pos))
    {
      files.push_back(arguments[pos]);
    }
  }

  // Create the window interface
  unsigned int x = 30, y = 30;
  unsigned int nrows = 1, ncols = 1;
  bool isEmbedded = false;
  osg::ref_ptr<OpenFrames::WindowProxy> myWindow = new OpenFrames::WindowProxy(x, y, width, height, nrows, ncols, isEmbedded, useVR);
  useVR = myWindow->getUseVR(); // Check if VR was enabled
  myWindow->setWindowName("OpenFrames with SilverLining");
  myWindow->setWorldUnitsPerMeter(worldUnitsPerMeter);
  myWindow->setWorldUnitsPerMeterLimits(1.0, DBL_MAX);

  // Load the first available model from the command line
  OpenFrames::Model *theModel = new OpenFrames::Model("Model", 0.5, 0.5, 0.5, 0.9);
  for (int i = 0; i < files.size(); ++i)
  {
    if (theModel->setModel(files[i])) break;
  }

  // Create a frame manager to handle the scene
  osg::ref_ptr<OpenFrames::FrameManager> fm = new OpenFrames::FrameManager;
  fm->setFrame(theModel);

  // Set up the scene
  myWindow->setScene(fm, 0, 0);

  // Create SilverLining sky
  osg::ref_ptr<SilverLiningNode> slSkyNode = new MySilverLiningNode("Your user name", "Your license code");

  if (useVR)
  {
    OpenFrames::VRCamera* backCameraVR = myWindow->getGridPosition(0, 0)->getBackgroundVR();
    osg::Camera *cam;
    for (unsigned int i = 0; i < backCameraVR->getNumCameras(); ++i)
    {
      cam = backCameraVR->getCamera(i);
      cam->addChild(slSkyNode);
    }
  }
  else
  {
    osg::Camera* backCamera = myWindow->getGridPosition(0, 0)->getBackground();
    backCamera->addChild(slSkyNode);
  }

  myWindow->startThread(); // Start window animation
  myWindow->join(); // Wait for window animation to finish
  return 0;
}

int main(int argc, char** argv)
{
  //return StartOSGViewer(argc, argv);
  return StartOFViewer(argc, argv);
}