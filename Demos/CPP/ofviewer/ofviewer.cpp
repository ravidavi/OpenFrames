/***********************************
   Copyright 2020 Ravishankar Mathur

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

#include <OpenFrames/FrameManager.hpp>
#include <OpenFrames/Model.hpp>
#include <OpenFrames/WindowProxy.hpp>
#include <osg/ArgumentParser>
#include <osg/io_utils>
#include <osgDB/FileNameUtils>
#include <osgDB/FileUtils>

#ifdef USE_OPENVR
#include <openvr.h>
#endif

using namespace OpenFrames;

// Global pointer to the WindowProxy created in main()
// In a real program, this would probably be wrapped in a class and not in the global namespace
WindowProxy *theWinProxy;

osgText::Text* hudText;

// Callback to handle key press events
void KeyPressCallback(unsigned int *winID, unsigned int *row, unsigned int *col, int *key)
{
  // Pause/unpause animation
  if(*key == 'p')
  {
    theWinProxy->pauseTime(!theWinProxy->isTimePaused());
  }

  else if ((*key == 'v') || (*key == 'V'))
  {
    std::string frameName = theWinProxy->getGridPosition(0, 0)->getCurrentView()->getViewFrame()->getName();
    hudText->setText("Viewing: " + frameName);
  }
}

// Callback to handle VR controller events
// This is called by WindowProxy when a new OpenVR event is detected
#ifdef USE_OPENVR
void VREventCallback(unsigned int *winID, unsigned int *row, unsigned int *col, const OpenFrames::OpenVREvent *vrEvent)
{
  // Get the OpenFrames::OpenVRDevice object that handles OpenVR rendering & interaction
  const OpenFrames::OpenVRDevice* ovrDevice = theWinProxy->getOpenVRDevice();
  vr::IVRSystem* vrSystem = ovrDevice->getVRSystem();

  // Get OpenVR event data; see openvr.h and online documentation/examples for details
  const vr::VREvent_t *ovrEvent = vrEvent->_ovrEvent;  // OpenVR data type
  vr::TrackedDeviceIndex_t deviceID = ovrEvent->trackedDeviceIndex; // Device index that generated event
  const OpenVRDevice::DeviceModel* deviceModel = ovrDevice->getDeviceModel(deviceID);

  // OpenVR will sometimes send a dummy event with an invalid device ID; ignore those, as well as
  // any other events that don't come from a controller
  if ((deviceModel == nullptr) || (deviceModel->_class != OpenVRDevice::CONTROLLER)) return;

  const vr::VRControllerState_t *state = ovrDevice->getDeviceModel(deviceID)->_controllerState; // Current device state

  // Process OpenVR controller event
  // Note that events are sent ONCE when they happen. e.g. if the grip button is held down, then
  // a vr::VREvent_ButtonPress event is sent just once, not continuously while the button is held.
  switch (ovrEvent->eventType)
  {
  // A controller button was pressed
  case(vr::VREvent_ButtonPress):
  {
    // If only touchpad is pressed, then play/pause time
    if (state->ulButtonPressed == vr::ButtonMaskFromId(vr::k_EButton_SteamVR_Touchpad))
    {
      theWinProxy->pauseTime(!theWinProxy->isTimePaused());
    }

    // If grip and touchpad are pressed at same time, then switch views
    // Note that for this to work properly, the grip button should be pressed first, then touchpad.
    // Otherwise if touchpad is pressed first, then the above if() statement will also execute.
    else if ((state->ulButtonPressed & vr::ButtonMaskFromId(vr::k_EButton_Grip)) &&
      (state->ulButtonPressed & vr::ButtonMaskFromId(vr::k_EButton_SteamVR_Touchpad)))
    {
      theWinProxy->getGridPosition(*row, *col)->nextView();
      std::string frameName = theWinProxy->getGridPosition(0, 0)->getCurrentView()->getViewFrame()->getName();
      hudText->setText("Viewing: " + frameName);
    }

    // If only trigger is pressed, then compute controller origin in various reference frames
    else if (state->ulButtonPressed & vr::ButtonMaskFromId(vr::k_EButton_SteamVR_Trigger))
    {
      // Get the currently active OpenVRTrackball
      OpenVRTrackball *vrTrackball = dynamic_cast<OpenVRTrackball*>(theWinProxy->getGridPosition(*row, *col)->getCurrentView()->getTrackball());
      if (vrTrackball)
      {
        // Get the controller origin in room coordinates
        // Note that transforming the origin is the same as getting the translation component of the device transformation matrix
        osg::Vec3d originRoom = ovrDevice->getDeviceModel(deviceID)->_rawDeviceToWorld.getTrans() * ovrDevice->getWorldUnitsPerMeter();
        osg::notify(osg::NOTICE) << "  - Origin in Room frame  = " << originRoom << std::endl;

        // Get the controller origin in view coordinates by multiplying Room->Trackball with Trackball->View matrices
        // Here, the TrackballManipulator parent transforms from Trackball space to View space
        osg::Vec3d originView = originRoom * vrTrackball->getRoomToTrackballMatrix() * vrTrackball->osgGA::TrackballManipulator::getMatrix();
        osg::notify(osg::NOTICE) << "  - Origin in View frame  = " << originView << std::endl;

        // Get the controller origin in world coordinates by multiplying Room->Trackball with Trackball->World matrices
        // Here, the FollowingTrackball parent transforms from Trackball space to World space
        osg::Vec3d originWorld = originRoom * vrTrackball->getRoomToTrackballMatrix() * vrTrackball->FollowingTrackball::getMatrix();
        osg::notify(osg::NOTICE) << "  - Origin in World frame = " << originWorld << std::endl;
      }
    }

    break;
  }
  }
}
#else // OpenVR not available, so just create a stub VR event callback function
void VREventCallback(unsigned int *winID, unsigned int *row, unsigned int *col, const OpenFrames::OpenVREvent *vrEvent)
{
}
#endif

/** This example loads and displays a model using OpenFrames.
  **/
int main(int argc, char** argv)
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
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(x, y, width, height, nrows, ncols, isEmbedded, useVR);
  theWinProxy = myWindow;
  myWindow->setWorldUnitsPerMeter(worldUnitsPerMeter);
  myWindow->setWorldUnitsPerMeterLimits(1.0, DBL_MAX);
  
  // Set key press callback
  myWindow->setKeyPressCallback(KeyPressCallback);

  // Set VR callback if needed
  if (useVR) myWindow->setVREventCallback(VREventCallback);

  // Create the root frame that will hold all specified models
  osg::ref_ptr<ReferenceFrame> rootFrame = new ReferenceFrame("Root");
  rootFrame->showAxes(ReferenceFrame::NO_AXES);
  rootFrame->showAxesLabels(ReferenceFrame::NO_AXES);
  rootFrame->showNameLabel(false);

  // Create a frame manager to handle the scene
  osg::ref_ptr<FrameManager> fm = new FrameManager;
  fm->setFrame(rootFrame);

  // Set up the scene
  myWindow->setScene(fm, 0, 0);
  //myWindow->getGridPosition(0, 0)->setBackgroundColor(0, 0, 0);
  
  // Initialize window title
  std::string windowName = "OpenFrames Viewer:";

  // Create Models to hold user-specified models
  for (int i = 0; i < files.size(); ++i)
  {
    Model *theModel = new Model("Model", 0.5, 0.5, 0.5, 0.9);
    if (!theModel->setModel(files[i])) continue;
    std::string fname = osgDB::getSimpleFileName(files[i]);
    std::string fname_noext = osgDB::getNameLessAllExtensions(fname);
    theModel->setName(fname_noext);
    windowName += " " + fname_noext;

    // Reset model pivot so its origin coincides with the root scene origin
    theModel->setModelPivot(0.0, 0.0, 0.0);

    // Add model to scene
    rootFrame->addChild(theModel);

    // Create a view for the model
    View *modelView = new View(rootFrame, theModel);
    myWindow->getGridPosition(0, 0)->addView(modelView);
  }

  if (rootFrame->getNumChildren() == 0)
  {
    OSG_WARN << "No models loaded, exiting." << std::endl;
    return 1;
  }
  
  // Create text to go in HUD
  osg::ref_ptr<osgText::Text> hudText_BottomLeft = new osgText::Text;
  hudText = hudText_BottomLeft;
  hudText_BottomLeft->setFont("arial.ttf");
  hudText_BottomLeft->setColor(osg::Vec4(1, 1, 0, 1));
  hudText_BottomLeft->setCharacterSizeMode(osgText::Text::SCREEN_COORDS);
  hudText_BottomLeft->setCharacterSize(20.0);    // In screen coordinates (pixels)
  hudText_BottomLeft->setFontResolution(40, 40); // In texels (texture pixels)
  hudText_BottomLeft->setLineSpacing(0.25);

  // Position HUD text
  // Screen coordinates go from (0,0) bottom-left to (1,1) top-right
  hudText_BottomLeft->setAlignment(osgText::Text::LEFT_BOTTOM);
  hudText_BottomLeft->setPosition(osg::Vec3(0.0, 0.0, 0.0));

  // Some graphics drivers have a bug where text can't be properly changed.
  // Get around this by initializing text using all likely characters.
  std::string dummyText("the quick brown fox jumps over the lazy dog");
  dummyText += "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG";
  dummyText += "1234567890";
  dummyText += "[]{}()<>,.;:+-*/_";
  hudText_BottomLeft->setText(dummyText);

  // Now set text to actual model name
  std::string frameName = theWinProxy->getGridPosition(0, 0)->getCurrentView()->getViewFrame()->getName();
  hudText_BottomLeft->setText("Viewing: " + frameName);

  // Attach HUD text
  osg::Geode* geode = new osg::Geode;
  geode->addDrawable(hudText_BottomLeft);
  myWindow->getGridPosition(0, 0)->getHUD()->addChild(geode);

  myWindow->setWindowName(windowName);
  myWindow->startThread(); // Start window animation
  myWindow->join(); // Wait for window animation to finish

  return 0;
}
