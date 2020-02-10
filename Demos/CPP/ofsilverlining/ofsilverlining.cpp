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
    t.SetFromSystemTime(); // Use this to set time based on actual current time
    //t.SetHour(15.0);     // Use this to set a specified time & time zone
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

/** IGNORE THIS FUNCTION, IT IS HERE FOR TESTING AND WILL BE REMOVED LATER
    USE THE StartOFViewer function below. */
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
  viewer.addEventHandler(new osgGA::StateSetManipulator(viewer.getCamera()->getOrCreateStateSet()));
  viewer.addEventHandler(new osgViewer::StatsHandler);
  viewer.addEventHandler(new osgViewer::WindowSizeHandler);
  viewer.setSceneData(scene.get());
  viewer.setUpViewOnSingleScreen(0);
  return viewer.run();
}

/* Create the OpenFrames viewer and scene, and animate*/
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
  theModel->showAxes(OpenFrames::ReferenceFrame::NO_AXES);
  theModel->showAxesLabels(OpenFrames::ReferenceFrame::NO_AXES);
  theModel->showNameLabel(false);
  for (int i = 0; i < files.size(); ++i)
  {
    if (theModel->setModel(files[i])) break;
  }

  // Create a ReferenceFrame that will be attached to the ground
  OpenFrames::ReferenceFrame *est = new OpenFrames::ReferenceFrame("Emergent", 1.0, 1.0, 1.0, 0.9);
  est->setXLabel("East");
  est->setYLabel("North");
  est->setZLabel("Up");
  theModel->addChild(est); // Assumes theModel is Earth in meters (e.g. osgEarth)

  // Compute location and topocentric orientation of Emergent HQ with osg's EllipsoidModel
  osg::EllipsoidModel earthEllipsoid;
  double lat_EST = osg::DegreesToRadians(39.103731);
  double lon_EST = osg::DegreesToRadians(-76.869185);
  double alt_EST = 70.0; // Meters above sea level
  osg::Matrixd transform_EST; // Position and orientation of Emergent HQ
  earthEllipsoid.computeLocalToWorldTransformFromLatLongHeight(lat_EST, lon_EST, alt_EST, transform_EST);
  est->setPosition(transform_EST.getTrans());
  est->setAttitude(transform_EST.getRotate());

  // Create a view for Emergent
  OpenFrames::View *estView = new OpenFrames::View(theModel, est);
  myWindow->getGridPosition(0, 0)->addView(estView);

  // Create a frame manager to handle the scene
  osg::ref_ptr<OpenFrames::FrameManager> fm = new OpenFrames::FrameManager;
  fm->setFrame(theModel);

  // Set up the scene
  myWindow->setScene(fm, 0, 0);

  // Create SilverLining sky
  osg::ref_ptr<SilverLiningNode> slSkyNode = new MySilverLiningNode("Your user name", "Your license code");
  slSkyNode->skyDrawable()->setDrawStars(false);

  // Add SilverLining sky to background
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

  // Add galactic background image and stars
  myWindow->getGridPosition(0, 0)->setSkySphereTexture("Images/ESA_Gaia_DR2_8192.jpg");
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
    0.0, 0.0, 0.0, 1.0);
  osg::Quat eq_to_gal = eq_to_gal_mat.getRotate();

  // Quaternion that transforms Galactic to J2000 Equatorial coordinates
  osg::Quat gal_to_eq = eq_to_gal.inverse();

  // Gaia image is offset by 180 degrees as compared to the OpenFrames::Sphere texture wrapping
  // convention, so rotate it by 180 degrees before performing the Galatic->J2000 transformation
  myWindow->getGridPosition(0, 0)->getSkySphere()->setSphereAttitude(osg::Quat(osg::PI, osg::Vec3d(0, 0, 1))*gal_to_eq);

  myWindow->startThread(); // Start window animation
  myWindow->join(); // Wait for window animation to finish
  return 0;
}

int main(int argc, char** argv)
{
  //return StartOSGViewer(argc, argv); // IGNORE THIS, IT IS HERE FOR TESTING AND WILL BE REMOVED LATER
  return StartOFViewer(argc, argv);
}