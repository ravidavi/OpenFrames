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

#include <OpenFrames/FrameManager.hpp>
#include <OpenFrames/Model.hpp>
#include <OpenFrames/WindowProxy.hpp>
#include <osg/ArgumentParser>

using namespace OpenFrames;

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

  // Get model filename
  std::string filename;
  for (int pos = 1; pos < arguments.argc(); ++pos)
  {
    if (!arguments.isOption(pos))
    {
      filename = arguments[pos];
      break;
    }
  }

  if (filename.length() == 0)
  {
    OSG_NOTICE << "Please specify a filename" << std::endl;
    return 1;
  }

  // Create the window interface
  unsigned int x = 30, y = 30;
  unsigned int nrows = 1, ncols = 1;
  bool isEmbedded = false;
  osg::ref_ptr<WindowProxy> myWindow = new WindowProxy(x, y, width, height, nrows, ncols, isEmbedded, useVR);
  myWindow->setWorldUnitsPerMeter(worldUnitsPerMeter);
  myWindow->setWorldUnitsPerMeterLimits(1.0, DBL_MAX);

  // Create Model for to hold user-specified model
  Model *theModel = new Model("Model", 0.5, 0.5, 0.5, 0.9);
  if (!theModel->setModel(filename))
  {
    return 1;
  }
  theModel->showNameLabel(false);

  // Create a frame manager to handle the scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(theModel);

  // Set up the scene
  myWindow->setScene(fm, 0, 0);
  //myWindow->getGridPosition(0, 0)->setBackgroundColor(0, 0, 0);

  myWindow->startThread(); // Start window animation
  myWindow->join(); // Wait for window animation to finish

  return 0;
}
