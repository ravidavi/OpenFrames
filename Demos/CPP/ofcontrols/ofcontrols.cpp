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

#include "ControlPanel.hpp"

#include <OpenFrames/WindowProxy.hpp>

#include <QApplication>

/// The period of event checking in the main loop (milliseconds)
static const double MAIN_LOOP_PERIOD = 100;


int main(int argc, char **argv)
{
  OpenFrames::WindowProxy *winProxy = new OpenFrames::WindowProxy(100, 100, 640, 480, 1, 1, false, false);

  OpenFrames::ReferenceFrame *root = new OpenFrames::ReferenceFrame("root");
  root->showNameLabel(false);
  root->showAxes(0U);

  OpenFrames::ControlPanel *panel = new OpenFrames::ControlPanel("panel");
  panel->setHalfLengths(1.0, 1.0, 0.1);
  panel->setTextureMap("../Images/marble.jpg");
  root->addChild(panel);

  OpenFrames::FrameManager *frameManager = new OpenFrames::FrameManager();
  frameManager->setFrame(root);
  winProxy->setScene(frameManager, 0U, 0U);

  OpenFrames::FramerateLimiter waitLimiter;
  waitLimiter.setDesiredFramerate(1000.0/MAIN_LOOP_PERIOD); // FPS at which to check events

  winProxy->start();
  while (winProxy->isRunning())
  {
    // check events periodically
    QCoreApplication::processEvents(QEventLoop::AllEvents, MAIN_LOOP_PERIOD);
	  waitLimiter.frame();
  }

  return 0;
}
