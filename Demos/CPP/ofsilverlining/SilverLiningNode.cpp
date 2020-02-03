#include <osg/CoordinateSystemNode>
#include <osg/Depth>
#include <osgDB/FileNameUtils>
#include <osgUtil/CullVisitor>
#include "SilverLiningNode.hpp"

/* SilverLiningNode::SkyDrawable */

SilverLiningNode::SkyDrawable::SkyDrawable( SilverLiningNode* s )
    :   _silverLining(s)
{}

SilverLiningNode::SkyDrawable::SkyDrawable( const SkyDrawable& copy, const osg::CopyOp& copyop )
    :   osg::Drawable(copy, copyop), _silverLining(copy._silverLining)
{}

void SilverLiningNode::SkyDrawable::drawImplementation( osg::RenderInfo& renderInfo ) const
{
  // Get the current camera's parameters
  osg::Camera* camera = renderInfo.getCurrentCamera();
  double fovy, ar, zNear, zFar;
  camera->getProjectionMatrixAsPerspective(fovy, ar, zNear, zFar);
  _silverLining->setSkyBoxSize(zFar < 100000.0 ? zFar : 100000.0);

  //OSG_NOTICE << "Camera " << renderInfo.getCurrentCamera()->getName() << ": zNear = " << zNear << ", zFar = " << zFar << std::endl;

  renderInfo.getState()->disableAllVertexArrays();

  // Initialize SilverLining if needed
  _silverLining->initializeSilverLining(renderInfo);

  osg::Matrix viewMat = renderInfo.getCurrentCamera()->getViewMatrix();
  _silverLining->atmosphere()->SetCameraMatrix(viewMat.ptr());
  osg::Matrix projMat = renderInfo.getState()->getProjectionMatrix();
  _silverLining->atmosphere()->SetProjectionMatrix(projMat.ptr());

  // Draw the SilverLining skybox
  const bool drawSky = true;
  const bool geocentricMode = true;
  double skyBoxSize = _silverLining->getSkyBoxSize();
  const bool drawStars = true;
  const bool clearDepth = false;
  const bool drawSunAndMoon = true;
  _silverLining->atmosphere()->DrawSky(drawSky, geocentricMode, skyBoxSize, drawStars, clearDepth, drawSunAndMoon, renderInfo.getCurrentCamera());

  renderInfo.getState()->dirtyAllVertexArrays();
  renderInfo.getState()->dirtyAllAttributes();
  renderInfo.getState()->setLastAppliedProgramObject(0L);
  renderInfo.getState()->apply();
}

osg::BoundingBox SilverLiningNode::SkyDrawable::computeBoundingBox() const
{
    osg::BoundingBox skyBoundBox;
    if ( !_silverLining->isAtmosphereValid() ) return skyBoundBox;

    SilverLining::Atmosphere* atmosphere = _silverLining->atmosphere();
    double skyboxSize = _silverLining->getSkyBoxSize();
    if (skyboxSize == 0.0) skyboxSize = 1000.0;

    osg::Vec3d radiusVec = osg::Vec3d(skyboxSize, skyboxSize, skyboxSize) * 0.5;
    const osg::Vec3d& camPos = _silverLining->getCameraPosition();
    skyBoundBox.set( camPos-radiusVec, camPos+radiusVec );

    bool hasLimb = atmosphere->GetConfigOptionBoolean("enable-atmosphere-from-space");
    if ( hasLimb ) {
      // Compute bounds of atmospheric limb centered at (0,0,0)
      double earthRadius = atmosphere->GetConfigOptionDouble("earth-radius-meters-equatorial");
      double atmosphereHeight = earthRadius + atmosphere->GetConfigOptionDouble("atmosphere-height");
      double atmosphereThickness = atmosphere->GetConfigOptionDouble("atmosphere-scale-height-meters") + earthRadius;

      osg::BoundingBox atmosphereBox;
      osg::Vec3d atmMin(-atmosphereThickness, -atmosphereThickness, -atmosphereThickness);
      osg::Vec3d atmMax(atmosphereThickness, atmosphereThickness, atmosphereThickness);
      atmosphereBox.set(atmMin, atmMax);
      skyBoundBox.expandBy(atmosphereBox);
    }
    return skyBoundBox;
}

/* SilverLiningNode::CloudDrawable */

SilverLiningNode::CloudDrawable::CloudDrawable( SilverLiningNode* s )
    :   _silverLining(s)
{}

SilverLiningNode::CloudDrawable::CloudDrawable( const CloudDrawable& copy, const osg::CopyOp& copyop )
    :   osg::Drawable(copy, copyop), _silverLining(copy._silverLining)
{}

SilverLiningNode::~SilverLiningNode()
{
    delete _atmosphere;
}

void SilverLiningNode::CloudDrawable::drawImplementation( osg::RenderInfo& renderInfo ) const
{
  renderInfo.getState()->disableAllVertexArrays();

  const bool drawClouds = true;
  const bool drawPrecipitation = true;
  const bool enableDepthTest = true;
  const bool geocentricMode = true;
  _silverLining->atmosphere()->DrawObjects(drawClouds, drawPrecipitation, enableDepthTest, 0.0f, false, 0, true, true, true, geocentricMode);

  renderInfo.getState()->dirtyAllVertexArrays();
  renderInfo.getState()->dirtyAllAttributes();
  renderInfo.getState()->apply();
}

osg::BoundingBox SilverLiningNode::CloudDrawable::computeBoundingBox() const
{
    osg::BoundingBox cloudBoundBox;
    if ( !_silverLining->isAtmosphereValid() ) return cloudBoundBox;

    double minX, minY, minZ, maxX, maxY, maxZ;
    _silverLining->atmosphere()->GetCloudBounds( minX, minY, minZ, maxX, maxY, maxZ );
    cloudBoundBox.set( osg::Vec3d(minX, minY, minZ), osg::Vec3d(maxX, maxY, maxZ) );
    return cloudBoundBox;
}

/* SilverLiningNode::AtmosphereUpdater */

bool SilverLiningNode::AtmosphereUpdater::run(osg::Object* object, osg::Object* data)
{
  SilverLiningNode* silverLining = static_cast<SilverLiningNode*>(object);
  osg::NodeVisitor* nv = data->asNodeVisitor();

  if (silverLining && nv) {
    if (nv->getVisitorType() == osg::NodeVisitor::UPDATE_VISITOR) {
      if (silverLining->isAtmosphereValid()) {
        silverLining->skyDrawable()->dirtyBound();
        silverLining->cloudDrawable()->dirtyBound();
      }
    }
    else if (nv->getVisitorType() == osg::NodeVisitor::CULL_VISITOR) {
      silverLining->setCameraPosition(nv->getEyePoint());
      if (silverLining->isAtmosphereValid()) {
        silverLining->updateLocation();
        silverLining->updateGlobalLight();
      }
    }
  }

  return traverse(object, data);
}

/* SilverLiningNode */

SilverLiningNode::SilverLiningNode( const char* licenseUser, const char* licenseKey )
    :   _initialized(false), _skyBoxSize(0.0)
{
  // Draw sky before everything else
  _sky = new SkyDrawable(this);
  _sky->setUseVertexBufferObjects(false);
  _sky->setUseDisplayList(false);
  _sky->getOrCreateStateSet()->setRenderBinDetails(-100, "RenderBin");
  addDrawable(_sky.get());

  // Draw clouds after everything else
  _cloud = new CloudDrawable(this);
  _cloud->setUseVertexBufferObjects(false);
  _cloud->setUseDisplayList(false);
  _cloud->getOrCreateStateSet()->setRenderBinDetails(99, "RenderBin");
  addDrawable(_cloud.get());
  _cloud->setNodeMask(0x0); // Disable clouds for now

  AtmosphereUpdater* updater = new AtmosphereUpdater;
  setUpdateCallback(updater);
  setCullCallback(updater);

  _atmosphere = new SilverLining::Atmosphere(licenseUser, licenseKey);
  _atmosphere->EnableLensFlare(true);

  const char* slPath = ::getenv("SILVERLINING_PATH");
  if (slPath)
    _resourcePath = osgDB::concatPaths(slPath, "Resources");
}

SilverLiningNode::SilverLiningNode(const SilverLiningNode& copy, const osg::CopyOp& copyop)
  : osg::Geode(copy, copyop), _sky(copy._sky), _cloud(copy._cloud), _light(copy._light),
  _atmosphere(copy._atmosphere), _resourcePath(copy._resourcePath),
  _cameraPos(copy._cameraPos), _initialized(copy._initialized)
{}

bool SilverLiningNode::initializeSilverLining( osg::RenderInfo& renderInfo )
{
  if (_initialized) return true;
  srand(1234); // constant random seed to ensure consistent clouds across windows

  OSG_NOTICE << "SilverLining resourcePath = " << _resourcePath << std::endl;
  int result = _atmosphere->Initialize(
    SilverLining::Atmosphere::OPENGL, _resourcePath.c_str(), true, 0);
  if (result != SilverLining::Atmosphere::E_NOERROR) {
    std::cout << "SilverLining failed to initialize: " << result << std::endl;
    return false;
  }

  _initialized = true;
  _atmosphere->SetUpVector(0.0, 0.0, 1.0);
  _atmosphere->SetRightVector(1.0, 0.0, 0.0);
  createAtmosphereData(renderInfo);

  return true;
}

void SilverLiningNode::updateLocation()
{
  if (!_initialized) return;
  
  // Get new local orientation
  osg::Vec3d up = _cameraPos;
  up.normalize();
  osg::Vec3d north = osg::Vec3d(0, 0, 1);
  osg::Vec3d east = north ^ up;
  if (east.length2() <= 1.0e-6) east = osg::Vec3d(1, 0, 0);
  else east.normalize();

  _atmosphere->SetUpVector(up.x(), up.y(), up.z());
  _atmosphere->SetRightVector(east.x(), east.y(), east.z());

  // Compute lat/lon/alt with osg's EllipsoidModel
  // Ideally this would come from osgEarth or whatever Earth model is being used
  osg::EllipsoidModel earthEllipsoid;
  double lat, lon, alt;
  earthEllipsoid.convertXYZToLatLongHeight(_cameraPos.x(), _cameraPos.y(), _cameraPos.z(), lat, lon, alt);
  lat = osg::RadiansToDegrees(lat);
  lon = osg::RadiansToDegrees(lon);

  SilverLining::Location loc;
  loc.SetAltitude(alt);
  loc.SetLatitude(lat);
  loc.SetLongitude(lon);
  _atmosphere->GetConditions()->SetLocation(loc);
}

void SilverLiningNode::updateGlobalLight()
{
  if (_initialized && _light.valid()) {
    float ra, ga, ba, rd, gd, bd, x, y, z;
    _atmosphere->GetAmbientColor(&ra, &ga, &ba);
    _atmosphere->GetSunOrMoonColor(&rd, &gd, &bd);
    _atmosphere->GetSunOrMoonPositionGeographic(&x, &y, &z);

    _light->setAmbient(osg::Vec4(ra, ga, ba, 1.0f));
    _light->setDiffuse(osg::Vec4(rd, gd, bd, 1.0f));
    _light->setPosition(osg::Vec4(x, y, z, 0.0f));
  }
}