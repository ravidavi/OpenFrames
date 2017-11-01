/***********************************
   Copyright 2017 Ravishankar Mathur

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

#include <OpenFrames/CoordinateAxes.hpp>
#include <OpenFrames/CurveArtist.hpp>
#include <OpenFrames/DrawableTrajectory.hpp>
#include <OpenFrames/FrameManager.hpp>
#include <OpenFrames/FrameTransform.hpp>
#include <OpenFrames/MarkerArtist.hpp>
#include <OpenFrames/Model.hpp>
#include <OpenFrames/RadialPlane.hpp>
#include <OpenFrames/SegmentArtist.hpp>
#include <OpenFrames/Sphere.hpp>
#include <OpenFrames/Trajectory.hpp>
#include <OpenFrames/WindowProxy.hpp>
#include <iostream>
#include <cmath>
#include <osg/Math>

using namespace OpenFrames;

double tscale = 1.0; // Animation speedup relative to real time
ReferenceFrame *earth;
TimeManagementVisitor *tmv;
WindowProxy *theWindow;

/** The function called when the user presses a key */
void KeyPressCallback(unsigned int *winID, unsigned int *row, unsigned int *col, int *key)
{
  static bool paused = false;
  static bool stereo = false;

  // Pause/unpause animation
  if (*key == 'p')
  {
    paused = !paused;
    tmv->setPauseState(true, paused);
    earth->getTransform()->accept(*tmv);
    tmv->setPauseState(false, paused);
  }

  // Reset time to epoch. All ReferenceFrames that are following
  // a Trajectory will return to their starting positions.
  else if (*key == 'r')
  {
    tmv->setReset(true);
    earth->getTransform()->accept(*tmv);
    tmv->setReset(false);
  }

  // Speed up time
  else if ((*key == '+') || (*key == '='))
  {
    tscale += 0.05;
    tmv->setTimeScale(true, tscale);
    earth->getTransform()->accept(*tmv);
    tmv->setTimeScale(false, tscale);
  }

  // Slow down time
  else if ((*key == '-') || (*key == '_'))
  {
    tscale -= 0.05;
    tmv->setTimeScale(true, tscale);
    earth->getTransform()->accept(*tmv);
    tmv->setTimeScale(false, tscale);
  }
}

/** This example shows how to create multiple subwindows, and have
  * a ReferenceFrame follow a path defined by Trajectory points. It also
  * shows how to use Artists to draw a single Trajectory in several
  * different ways.
  **/
int main()
{
  double km = 1.0; // km per graphics unit
  std::cout.precision(15);
  std::cout.setf(std::ios::scientific, std::ios::floatfield);

  // Create the interface that will draw a scene onto a window.
  unsigned int x = 30, y = 30, width = 1080 / 2, height = 1200 / 2;
  unsigned int nrows = 1, ncols = 1;
  bool isEmbedded = false, useVR = true;
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(x, y, width, height, nrows, ncols, isEmbedded, useVR);
  theWindow = myWindow.get();

  // Create the object that will handle keyboard input 
  // This includes pausing, resetting, modifying time, etc...
  osg::ref_ptr<TimeManagementVisitor> mytmv = new TimeManagementVisitor;
  tmv = mytmv.get();

  // Create the objects that will populate the scene using
  // Sphere(name, color(r,g,b,a))
  // Model(name, color(r,g,b,a))
  //earth = new Sphere("Earth", 0, 1, 0, 0.9);
  earth = new Model("Earth", 0, 1, 0, 0.9);
  earth->showAxes(ReferenceFrame::NO_AXES);
  earth->showAxesLabels(ReferenceFrame::NO_AXES);
  earth->showNameLabel(false);

  // Set Earth parameters
  //Sphere *earthSphere = dynamic_cast<Sphere*>(earth);
  //earthSphere->setRadius(6378137.0);
  //earthSphere->setTextureMap("../Images/EarthTexture.bmp");
  Model *earthModel = dynamic_cast<Model*>(earth);
  earthModel->setModel("osgearth/earthmap.earth");
  //earthModel->setModel("D:/tiled_pointsets/ZED_Data/tile_0_0_0_0.laz.lastile");

  // Create an artist to draw data center marker
  MarkerArtist *centermarker = new MarkerArtist;
  centermarker->setMarkerShader("Shaders/Marker_CirclePulse.frag");
  centermarker->setMarkerSize(15);

  // Create Models for each Indian Tunnel tileset
  osg::Vec3d center;
  double dist;
  const unsigned int numTunnelModels = 46;
  std::string tunnelID[numTunnelModels] =
  { "001", "002", "003", "004", "005",
  "006", "007", "008", "009", "010",
  "011", "012", "013", "014", "015",
  "016", "017", "018", "019", "020",
  "021", "022", "023", "024", "025",
  "026", "027", "028", "029", "030",
  "032", "033", "034", "035", // No "031" tileset
  "036", "037", "038", "039", "040",
  "041", "042", "043", "044", "045",
  "046", "full"
  };
  std::string tunnelBasePath = "D:/tiled_pointsets/IndianTunnel_TroyAmes/IndianTunnel_TroyAmes_";
  std::string tunnelTileName = "/tileset.lastile"; // lastile extension requires osgJuniper plugin
  Model* tunnelModel[numTunnelModels];
  DrawableTrajectory* drawcenter;
  osg::BoundingSphere indianTunnelBound; // Cumulative bounding sphere for all tunnel tilesets

  //for (unsigned int i = 0; i < numTunnelModels - 1; ++i)
  for (unsigned int i = numTunnelModels-1; i < numTunnelModels; ++i)
  {
    tunnelModel[i] = new Model("Indian Tunnel " + tunnelID[i], 1, 0, 0, 0.9);
    tunnelModel[i]->setModel(tunnelBasePath + tunnelID[i] + tunnelTileName);
    if (!tunnelModel[i]->getModel()) continue; // Model not loaded
    osg::BoundingSphere bound = tunnelModel[i]->getModel()->getBound();
    indianTunnelBound.expandBy(bound);
    center = bound._center;
    dist = center.normalize();
    center *= dist + 10.0; // To visually align with ground
    tunnelModel[i]->setPosition(center.x(), center.y(), center.z());
    tunnelModel[i]->showAxes(ReferenceFrame::NO_AXES);
    tunnelModel[i]->showAxesLabels(ReferenceFrame::NO_AXES);
    tunnelModel[i]->moveZAxis(osg::Vec3d(), 1.0);

    // Create a DrawableTrajectory to hold this tileset's center marker
    // Note that we can't share one DrawableTrajectory since it uses an
    // OpenGL uniform variable, which would be set to the last tileset's
    // position. So we must use one DrawableTrajectory per tileset.
    drawcenter = new DrawableTrajectory("center marker");
    drawcenter->showAxes(ReferenceFrame::NO_AXES);
    drawcenter->showAxesLabels(ReferenceFrame::NO_AXES);
    drawcenter->showNameLabel(false);
    drawcenter->addArtist(centermarker);
    tunnelModel[i]->addChild(drawcenter);
    earth->addChild(tunnelModel[i]);
  }

  // Create ReferenceFrame for IndianTunnel topographic coordinates
  osg::Vec3d up, north, east, earth_z(0, 0, 1);
  osg::Matrixd frameMat;
  osg::Quat frameQuat;
  up = indianTunnelBound._center; // Up vector is tunnel's center vector
  if (up.normalize() <= 1.0e-4) up.set(0, 0, 1);
  east = earth_z ^ up; // East is Earth-Z crossed with up vector
  if (east.length() <= 1.0e-4) east.set(0, 1, 0);
  else east.normalize();
  north = up ^ east; // North is up cross east
  north.normalize();
  frameMat.set( // Create rotation matrix
    east.x(), east.y(), east.z(), 0,    // X = east
    north.x(), north.y(), north.z(), 0, // Y = north
    up.x(), up.y(), up.z(), 0,          // Z = up
    0, 0, 0, 1);
  frameQuat = frameMat.getRotate();
  ReferenceFrame *indianTunnelFrame = new ReferenceFrame("Indian Tunnel Frame", 1, 0, 0, 0.9);
  indianTunnelFrame->showNameLabel(false);
  indianTunnelFrame->showAxes(ReferenceFrame::X_AXIS | ReferenceFrame::Y_AXIS); // Don't show Z (up) vector
  indianTunnelFrame->setXLabel("East");  // X points east
  indianTunnelFrame->setYLabel("North"); // Y points north
  double radius = indianTunnelBound._radius;
  indianTunnelFrame->moveXAxis(osg::Vec3d(radius / 2, 0, radius / 2), radius / 2.0);
  indianTunnelFrame->moveYAxis(osg::Vec3d(0, radius / 2, radius / 2), radius / 2.0);
  indianTunnelFrame->setAttitude(frameQuat._v[0], frameQuat._v[1], frameQuat._v[2], frameQuat._v[3]);
  indianTunnelFrame->setPosition(indianTunnelBound._center.x(), indianTunnelBound._center.y(), indianTunnelBound._center.z());
  earth->addChild(indianTunnelFrame);

  // Create Model for Barnegat tileset
  Model *barnegatModel = new Model("Barnegat NJ", 1, 0, 0, 0.9);
  //barnegatModel->setModel("D:/tiled_pointsets/barnegat/tile_0_0_0_0.laz.lastile");
  if (barnegatModel->getModel())
  {
    center = barnegatModel->getModel()->getBound()._center;
    dist = center.normalize();
    center *= dist - 32.0; // To visually align with ground
    barnegatModel->setPosition(center.x(), center.y(), center.z());
    barnegatModel->showAxes(ReferenceFrame::NO_AXES);
    barnegatModel->showAxesLabels(ReferenceFrame::NO_AXES);
    barnegatModel->moveZAxis(osg::Vec3d(), 1.0);
    drawcenter = new DrawableTrajectory("center marker");
    drawcenter->showAxes(ReferenceFrame::NO_AXES);
    drawcenter->showAxesLabels(ReferenceFrame::NO_AXES);
    drawcenter->showNameLabel(false);
    drawcenter->addArtist(centermarker);
    barnegatModel->addChild(drawcenter);
    //earth->addChild(barnegatModel);

    // Create ReferenceFrame for Barnegat topographic coordinates
    osg::BoundingSphere barnegatBound = barnegatModel->getModel()->getBound();
    up = barnegatBound._center;
    if (up.normalize() <= 1.0e-4) up.set(0, 0, 1);
    east = earth_z ^ up;
    if (east.length() <= 1.0e-4) east.set(0, 1, 0);
    else east.normalize();
    north = up ^ east;
    north.normalize();
    frameMat.set(
      east.x(), east.y(), east.z(), 0,
      north.x(), north.y(), north.z(), 0,
      up.x(), up.y(), up.z(), 0,
      0, 0, 0, 1);
    frameQuat = frameMat.getRotate();
    ReferenceFrame *barnegatFrame = new ReferenceFrame("Barnegat Frame", 1, 0, 0, 0.9);
    barnegatFrame->showNameLabel(false);
    barnegatFrame->showAxes(ReferenceFrame::X_AXIS | ReferenceFrame::Y_AXIS);
    barnegatFrame->setXLabel("East");
    barnegatFrame->setYLabel("North");
    radius = barnegatBound._radius;
    barnegatFrame->moveXAxis(osg::Vec3d(radius / 2, 0, radius / 2), radius / 2.0);
    barnegatFrame->moveYAxis(osg::Vec3d(0, radius / 2, radius / 2), radius / 2.0);
    barnegatFrame->setAttitude(frameQuat._v[0], frameQuat._v[1], frameQuat._v[2], frameQuat._v[3]);
    barnegatFrame->setPosition(barnegatBound._center.x(), barnegatBound._center.y(), barnegatBound._center.z());
    //earth->addChild(barnegatFrame);
  }

  // Create Model for King's Bowl tileset
  Model *kingsBowlModel = new Model("King's Bowl", 1, 0, 0, 0.9);
  //kingsBowlModel->setModel("D:/tiled_pointsets/KingsBowl_BrentGarry/tile_0_0_0_0.laz.lastile");
  if (kingsBowlModel->getModel())
  {
    center = kingsBowlModel->getModel()->getBound()._center;
    //dist = center.normalize();
    //center *= dist - 32.0; // To visually align with ground
    kingsBowlModel->setPosition(center.x(), center.y(), center.z());
    kingsBowlModel->showAxes(ReferenceFrame::NO_AXES);
    kingsBowlModel->showAxesLabels(ReferenceFrame::NO_AXES);
    kingsBowlModel->moveZAxis(osg::Vec3d(), 1.0);
    drawcenter = new DrawableTrajectory("center marker");
    drawcenter->showAxes(ReferenceFrame::NO_AXES);
    drawcenter->showAxesLabels(ReferenceFrame::NO_AXES);
    drawcenter->showNameLabel(false);
    drawcenter->addArtist(centermarker);
    kingsBowlModel->addChild(drawcenter);
    //earth->addChild(kingsBowlModel);

    // Create ReferenceFrame for King's Bowl topographic coordinates
    osg::BoundingSphere kingsBowlBound = kingsBowlModel->getModel()->getBound();
    up = kingsBowlBound._center;
    if (up.normalize() <= 1.0e-4) up.set(0, 0, 1);
    east = earth_z ^ up;
    if (east.length() <= 1.0e-4) east.set(0, 1, 0);
    else east.normalize();
    north = up ^ east;
    north.normalize();
    frameMat.set(
      east.x(), east.y(), east.z(), 0,
      north.x(), north.y(), north.z(), 0,
      up.x(), up.y(), up.z(), 0,
      0, 0, 0, 1);
    frameQuat = frameMat.getRotate();
    ReferenceFrame *kingsBowlFrame = new ReferenceFrame("King's Bowl Frame", 1, 0, 0, 0.9);
    kingsBowlFrame->showNameLabel(false);
    kingsBowlFrame->showAxes(ReferenceFrame::X_AXIS | ReferenceFrame::Y_AXIS);
    kingsBowlFrame->setXLabel("East");
    kingsBowlFrame->setYLabel("North");
    radius = kingsBowlBound._radius;
    kingsBowlFrame->moveXAxis(osg::Vec3d(radius / 2, 0, radius / 2), radius / 2.0);
    kingsBowlFrame->moveYAxis(osg::Vec3d(0, radius / 2, radius / 2), radius / 2.0);
    kingsBowlFrame->setAttitude(frameQuat._v[0], frameQuat._v[1], frameQuat._v[2], frameQuat._v[3]);
    kingsBowlFrame->setPosition(kingsBowlBound._center.x(), kingsBowlBound._center.y(), kingsBowlBound._center.z());
    //earth->addChild(kingsBowlFrame);
  }

  // Set starting VR scale
  //myWindow->setWorldUnitsPerMeter(12000000.0);
  myWindow->setWorldUnitsPerMeter(100);
  myWindow->setWorldUnitsPerMeterLimits(1.0, DBL_MAX);

  // Create views
  View *view = new View(earth, indianTunnelFrame);
  //View *view2 = new View(earth, barnegatFrame);
  //View *view3 = new View(earth, kingsBowlFrame);
  View *view4 = new View(earth, earth);

  // Create a manager to handle the spatial scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(earth);

  // Set up the scene
  theWindow->setScene(fm, 0, 0);
  theWindow->getGridPosition(0, 0)->setBackgroundColor(0, 0, 0);
  theWindow->getGridPosition(0, 0)->setSkySphereStarData("Stars/Stars_HYGv3.txt", -2.0, 8.0, 40000, 1.0, 4.0, 0.1);
  theWindow->getGridPosition(0, 0)->addView(view);
  //theWindow->getGridPosition(0, 0)->addView(view2);
  //theWindow->getGridPosition(0, 0)->addView(view3);
  theWindow->getGridPosition(0, 0)->addView(view4);

  // Specify the key press callback
  theWindow->setKeyPressCallback(KeyPressCallback);

  theWindow->startThread(); // Start window animation

  theWindow->join(); // Wait for window animation to finish

  return 0;
}
