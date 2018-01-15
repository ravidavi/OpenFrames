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

// wxWidgets program using OpenFrames
#include "ofwxwidgets.hpp"

#include <wx/sizer.h>
#include <wx/dcclient.h>

#include <iostream>
#include <sstream>

#include <OpenFrames/CoordinateAxes.hpp>
#include <OpenFrames/CurveArtist.hpp>
#include <OpenFrames/DrawableTrajectory.hpp>
#include <OpenFrames/FrameManager.hpp>
#include <OpenFrames/MarkerArtist.hpp>
#include <OpenFrames/Model.hpp>
#include <OpenFrames/RadialPlane.hpp>
#include <OpenFrames/SegmentArtist.hpp>
using namespace OpenFrames;

const unsigned int winWidth = 1024, winHeight = 768;

// Initialize static variable
MyGLCanvas* MyGLCanvas::currCanvas = NULL;

// Main WindowProxy
WindowProxy *theWindow;

/** The function called when the user presses a key */
void KeyPressCallback(unsigned int *winID, unsigned int *row, unsigned int *col, int *key)
{
  // Pause/unpause animation
  if(*key == 'p')
  {
    theWindow->pauseTime(!theWindow->isTimePaused());
  }

  // Reset time to epoch. All ReferenceFrames that are following
  // a Trajectory will return to their starting positions.
  else if(*key == 'r')
  {
    theWindow->setTime(0.0);
  }

  // Speed up time
  else if((*key == '+') || (*key == '='))
  {
    theWindow->setTimeScale(theWindow->getTimeScale() + 0.1);
  }

  // Slow down time
  else if((*key == '-') || (*key == '_'))
  {
    theWindow->setTimeScale(theWindow->getTimeScale() - 0.1);
  }

  // Shift time forward
  else if(*key == WXK_RIGHT)
  {
    theWindow->setTime(theWindow->getTime() + 0.1);
  }

  // Shift time backward
  else if(*key == WXK_LEFT)
  {
    theWindow->setTime(theWindow->getTime() - 0.1);
  }

}

MyGLCanvas::MyGLCanvas(wxFrame* parent, int* args) 
: wxGLCanvas(parent, wxID_ANY, args, wxDefaultPosition, wxDefaultSize)
{
  m_context = new wxGLContext(this);

  // Create embedded WindowProxy that handles all OpenFrames drawing
  int winX = 0, winY = 0; // Unused since wxWidgets sets window origin
  unsigned int nRow = 2, nCol = 1;
  bool embedded = true;
  bool useVR = false;
  m_winproxy = new OpenFrames::WindowProxy(winX, winY, winWidth, winHeight, nRow, nCol, embedded, useVR);
  theWindow = m_winproxy;
  m_winproxy->setID(0);
  m_winproxy->setMakeCurrentFunction(MyGLCanvas::makecurrent);
  m_winproxy->setSwapBuffersFunction(MyGLCanvas::swapbuffers);
  
  // On OSX, wxWidgets uses Cocoa which requires the context to be updated on reshape (resize)
  // This is done by calling the makecurrent function
#ifdef __APPLE__
  m_winproxy->setUpdateContextFunction(MyGLCanvas::makecurrent);
#endif
  
  m_winproxy->setDesiredFramerate(20);

  // Create the models that will populate the scene using
  // Model(name, color(r,g,b,a))
  Model *spacestation = new Model("Space Station", 0, 1, 0, 0.9);
  Model *hubble = new Model("Hubble", 1, 0, 0, 0.9);

  // Set the 3D models
  spacestation->setModel("Models/SpaceStation.3ds");
  hubble->setModel("Models/Hubble.3ds");

  // Create the trajectory using
  // Trajectory(DOF, number of optionals)
  Trajectory *traj = new Trajectory(3, 1);

  // Create a drawable trajectory for the spacecraft window using
  // DrawableTrajectory(name, color(r,g,b,a))
  DrawableTrajectory *drawtraj = new DrawableTrajectory("traj", 1, 0, 0, 0.9);
  drawtraj->showAxes(ReferenceFrame::NO_AXES);
  drawtraj->showAxesLabels(ReferenceFrame::NO_AXES);
  drawtraj->showNameLabel(false);

  // Create a CurveArtist for the trajectory.  By default the CurveArtist
  // will use x/y/z positions from the trajectory for plotting.
  CurveArtist *ca = new CurveArtist(traj);
  ca->setWidth(2.0); // Line width for the trajectory
  ca->setColor(1, 0, 0);
  drawtraj->addArtist(ca);

  Trajectory::DataSource data; // Data source for artists
  data._src = Trajectory::POSOPT;

  // Create an artist for velocity vectors
  SegmentArtist *sa = new SegmentArtist(traj);
  sa->setColor(0.5, 0, 0);
  data._element = 0;
  sa->setStartXData(data);
  data._element = 1;
  sa->setStartYData(data);
  data._element = 2;
  sa->setStartZData(data);
  data._element = 0;
  data._opt = 1;
  sa->setEndXData(data);
  data._element = 1;
  sa->setEndYData(data);
  data._element = 2;
  sa->setEndZData(data);
  drawtraj->addArtist(sa);

  // Create a drawable trajectory for the time history window
  DrawableTrajectory *timehist = new DrawableTrajectory("TimeHist", 1, 0, 0, 0.9);
  timehist->showAxes(ReferenceFrame::NO_AXES);
  timehist->showAxesLabels(ReferenceFrame::NO_AXES);
  timehist->showNameLabel(false);

  // Create an artist to draw position vs time
  ca = new CurveArtist(traj);
  ca->setColor(1, 0, 0);
  data._src = Trajectory::POSOPT;
  data._opt = 0;
  data._element = 0; // Use X position for X coordinate
  data._scale = 0.01; // Scale down since positions are large
  ca->setXData(data);
  data._element = 2; // Use Z position for Y coordinate
  ca->setYData(data);
  data._src = Trajectory::TIME; // Use time for Z coordinate
  data._scale = 1.0; // Don't scale time
  ca->setZData(data);
  timehist->addArtist(ca);

  // Create an artist to draw start/intermediate/end markers
  MarkerArtist *ma = new MarkerArtist(traj);
  ma->setMarkers(MarkerArtist::START + MarkerArtist::INTERMEDIATE + MarkerArtist::END);
  ma->setAutoAttenuate(true);
  ma->setMarkerColor(MarkerArtist::START, 0, 1, 0);
  ma->setMarkerColor(MarkerArtist::END,   1, 0, 0);
  ma->setMarkerColor(MarkerArtist::INTERMEDIATE, 1, 1, 0);
  ma->setMarkerImage("Images/fuzzyparticle.tiff");
  ma->setMarkerSize(10);
  data._src = Trajectory::POSOPT;
  data._element = 0; // Use X position for X coordinate
  data._scale = 0.01; // Scale down since positions are large
  ma->setXData(data);
  data._element = 2; // Use Z position for Y coordinate
  ma->setYData(data);
  data._src = Trajectory::TIME; // Use time for Z coordinate
  data._scale = 1.0; // Don't scale time
  ma->setZData(data);
  timehist->addArtist(ma);

  // Draw markers at equally spaced distances
  ma->setIntermediateType(MarkerArtist::DISTANCE);
  ma->setIntermediateSpacing(10.0); // Every 10 distance units
  ma->setIntermediateDirection(MarkerArtist::END); // From end of trajectory

  // Create a set of Coordinate Axes for time history plot
  CoordinateAxes *axes = new CoordinateAxes("axes", 0.0, 0.8, 0.8, 1);
  axes->setAxisLength(2.0*M_PI);
  axes->setTickSpacing(M_PI, 0.25*M_PI);
  axes->setTickSize(8, 5);
  axes->setTickImage("Images/circle.tiff");
  axes->setXLabel("X");
  axes->setYLabel("Z");
  axes->setZLabel("t");

  // Create a ReferenceFrame to show model's position in time history plot
  ReferenceFrame *trace = new ReferenceFrame("trace", 1, 1, 1, 1);

  // Create a drawable trajectory to hold the trace center marker
  DrawableTrajectory *drawcenter = new DrawableTrajectory("center marker", 1, 0, 0, 1);
  drawcenter->showAxes(ReferenceFrame::NO_AXES);
  drawcenter->showAxesLabels(ReferenceFrame::NO_AXES);
  drawcenter->showNameLabel(false);

  // Create a MarkerArtist to draw the center marker
  MarkerArtist *centermarker = new MarkerArtist();
  centermarker->setMarkerImage("Images/target.tiff");
  centermarker->setMarkerSize(10);

  // Add the markerartist to the drawable trajectory
  drawcenter->addArtist(centermarker);

  // Create a RadialPlane to show trace frame's orientation
  RadialPlane *rp = new RadialPlane("radial", 1, 1, 1, 1);
  rp->showAxes(ReferenceFrame::NO_AXES);
  rp->showAxesLabels(ReferenceFrame::NO_AXES);
  rp->showNameLabel(false);
  rp->setParameters(10.0, 2.5, 60.0*M_PI/180.0);

  // Set up reference frame heirarchies.
  spacestation->addChild(drawtraj);
  spacestation->addChild(hubble);
  axes->addChild(timehist);
  axes->addChild(trace);
  trace->addChild(drawcenter);
  axes->addChild(rp);

  // Tell model to follow trajectory (by default in LOOP mode)
  TrajectoryFollower *tf = new TrajectoryFollower(traj);
  hubble->getTransform()->setUpdateCallback(tf);

  // Tell trace frame to follow time history
  tf = new TrajectoryFollower(traj);
  data._src = Trajectory::POSOPT;
  data._opt = 0;
  data._element = 0; // Use X position for X coordinate
  data._scale = 0.01; // Scale down since positions are large
  tf->setXData(data);
  data._element = 2; // Use Z position for Y coordinate
  tf->setYData(data);
  data._src = Trajectory::TIME; // Use time for Z coordinate
  data._scale = 1.0;
  tf->setZData(data);
  trace->getTransform()->setUpdateCallback(tf);

  // Tell radial frame to follow time history's orientation
  tf = new TrajectoryFollower(traj);
  tf->setFollowType(TrajectoryFollower::ATTITUDE, TrajectoryFollower::LOOP);
  rp->getTransform()->setUpdateCallback(tf);
  rp->setPosition(0.0, 0.0, 0.0);

  // Create views
  View *view = new View(spacestation, spacestation);
  View *view2 = new View(spacestation, hubble);
  View *view3 = new View(axes, axes);
  View *view4 = new View(axes, trace);

  // Create a manager to handle the spatial scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(spacestation);

  // Create a manager to handle the time history scene
  FrameManager* fm2 = new FrameManager;
  fm2->setFrame(axes);

  // Set up the scene
  theWindow->setScene(fm, 0, 0);
  theWindow->setScene(fm2, 1, 0);
  theWindow->getGridPosition(0, 0)->setSkySphereTexture("Images/StarMap.tif");
  theWindow->getGridPosition(0, 0)->addView(view);
  theWindow->getGridPosition(0, 0)->addView(view2);
  theWindow->getGridPosition(1, 0)->addView(view3);
  theWindow->getGridPosition(1, 0)->addView(view4);

  // Add the actual positions and attitudes for the trajectory.
  osg::Quat att; // Quaternion for attitude transformations
  double pos[3], vel[3];
  pos[1] = vel[1] = 0.0;
  for(double t = 0.0; t <= 2.0*M_PI; t += M_PI/90.0)
  {
    pos[0] = 500.0*sin(t);
    pos[2] = 500.0*cos(t);
    vel[0] = pos[0] + 0.5*pos[2];
    vel[2] = pos[2] - 0.5*pos[0];
    att.makeRotate(t, 0, 1, 0);

    traj->addTime(t);
    traj->addPosition(pos);
    traj->setOptional(0, vel);
    traj->addAttitude(att[0], att[1], att[2], att[3]);
  }

  // Specify the key press callback
  theWindow->setKeyPressCallback(KeyPressCallback);

  // To avoid flashing on MSW
  SetBackgroundStyle(wxBG_STYLE_CUSTOM);
}

MyGLCanvas::~MyGLCanvas()
{
  m_winproxy->shutdown();
  m_winproxy->join();
  delete m_context;
}

void MyGLCanvas::makecurrent(unsigned int *winID, bool *success)
{
  *success = currCanvas->m_context->SetCurrent(*currCanvas);
}

void MyGLCanvas::swapbuffers(unsigned int *winID)
{
  currCanvas->SwapBuffers();
}

void MyGLCanvas::StartOpenFrames()
{
  m_winproxy->startThread();
  while(!m_winproxy->isAnimating())
    OpenThreads::Thread::YieldCurrentThread();
}

void MyGLCanvas::resized(wxSizeEvent& event) 
{
  wxFrame *p_frm = wxStaticCast(GetParent(), wxFrame);
  std::ostringstream os;
  os << "Window resized to (" << event.GetSize().GetWidth() << ", " << event.GetSize().GetHeight() << ")";
  p_frm->SetStatusText(os.str());

  if(m_winproxy->isAnimating()) 
  {
    m_winproxy->resizeWindow(0, 0, event.GetSize().GetWidth(), event.GetSize().GetHeight());
  }
}

void MyGLCanvas::mouseMoved(wxMouseEvent& event) 
{
  wxFrame *p_frm = wxStaticCast(GetParent(), wxFrame);
  std::ostringstream os;
  os << "mouseMoved to (" << event.GetX() << ", " << event.GetY() << ")";
  p_frm->SetStatusText(os.str());

  m_winproxy->mouseMotion(event.GetX(), event.GetY());
}

void MyGLCanvas::mouseDownLeft(wxMouseEvent& event)
{
  wxFrame *p_frm = wxStaticCast(GetParent(), wxFrame);
  std::ostringstream os;
  os << "mouseDown left at (" << event.GetX() << ", " << event.GetY() << ")";
  p_frm->SetStatusText(os.str());

  m_winproxy->buttonPress(event.GetX(), event.GetY(), 1);
}

void MyGLCanvas::mouseUpLeft(wxMouseEvent& event)
{
  wxFrame *p_frm = wxStaticCast(GetParent(), wxFrame);
  std::ostringstream os;
  os << "mouseUp left at (" << event.GetX() << ", " << event.GetY() << ")";
  p_frm->SetStatusText(os.str());

  m_winproxy->buttonRelease(event.GetX(), event.GetY(), 1);
}

void MyGLCanvas::mouseDownRight(wxMouseEvent& event)
{
  wxFrame *p_frm = wxStaticCast(GetParent(), wxFrame);
  std::ostringstream os;
  os << "mouseDown right at (" << event.GetX() << ", " << event.GetY() << ")";
  p_frm->SetStatusText(os.str());

  m_winproxy->buttonPress(event.GetX(), event.GetY(), 3);
}

void MyGLCanvas::mouseUpRight(wxMouseEvent& event)
{
  wxFrame *p_frm = wxStaticCast(GetParent(), wxFrame);
  std::ostringstream os;
  os << "mouseUp right at (" << event.GetX() << ", " << event.GetY() << ")";
  p_frm->SetStatusText(os.str());

  m_winproxy->buttonRelease(event.GetX(), event.GetY(), 3);
}

void MyGLCanvas::keyPressed(wxKeyEvent& event)
{
  wxFrame *p_frm = wxStaticCast(GetParent(), wxFrame);
  std::ostringstream os;
  os << "keyPressed " << event.GetKeyCode() << " (" << (char)event.GetKeyCode() << ")";
  p_frm->SetStatusText(os.str());

  m_winproxy->keyPress(event.GetKeyCode());
}

enum
{
  ID_Hello = 1
};

wxBEGIN_EVENT_TABLE(MyFrame, wxFrame)
  EVT_MENU(ID_Hello,   MyFrame::OnHello)
  EVT_MENU(wxID_EXIT,  MyFrame::OnExit)
  EVT_MENU(wxID_ABOUT, MyFrame::OnAbout)
wxEND_EVENT_TABLE()

wxBEGIN_EVENT_TABLE(MyGLCanvas, wxGLCanvas)
  EVT_MOTION(MyGLCanvas::mouseMoved)
  EVT_LEFT_DOWN(MyGLCanvas::mouseDownLeft)
  EVT_LEFT_UP(MyGLCanvas::mouseUpLeft)
  EVT_RIGHT_DOWN(MyGLCanvas::mouseDownRight)
  EVT_RIGHT_UP(MyGLCanvas::mouseUpRight)
  EVT_SIZE(MyGLCanvas::resized)
  EVT_CHAR(MyGLCanvas::keyPressed)
wxEND_EVENT_TABLE()

wxIMPLEMENT_APP(MyApp);

bool MyApp::OnInit()
{
  // Increase winHeight to account for title bar
  frame = new MyFrame( "Hello OpenFrames World", wxPoint(50, 50), wxSize(winWidth, winHeight+40) );

  wxBoxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);

  int args[] = {WX_GL_RGBA, WX_GL_DOUBLEBUFFER, WX_GL_DEPTH_SIZE, 16, 0};

  glCanvas = new MyGLCanvas( (wxFrame*) frame, args);
  MyGLCanvas::currCanvas = glCanvas;
  sizer->Add(glCanvas, 1, wxEXPAND);

  frame->SetSizer(sizer);
  frame->SetAutoLayout(true);

  frame->Show( true );
  glCanvas->StartOpenFrames();
  return true;
}

MyFrame::MyFrame(const wxString& title, const wxPoint& pos, const wxSize& size)
: wxFrame(NULL, wxID_ANY, title, pos, size)
{
  wxMenu *menuFile = new wxMenu;
  menuFile->Append(ID_Hello, "&Hello...\tCtrl-H",
      "Help string shown in status bar for this menu item");
  menuFile->AppendSeparator();
  menuFile->Append(wxID_EXIT);
  wxMenu *menuHelp = new wxMenu;
  menuHelp->Append(wxID_ABOUT);
  wxMenuBar *menuBar = new wxMenuBar;
  menuBar->Append( menuFile, "&File" );
  menuBar->Append( menuHelp, "&Help" );
  SetMenuBar( menuBar );
  CreateStatusBar();
  SetStatusText( "Welcome to wxWidgets!" );

  // To avoid flashing on MSW
  SetBackgroundStyle(wxBG_STYLE_CUSTOM);
}

void MyFrame::OnExit(wxCommandEvent& event)
{
  Close( true );
}

void MyFrame::OnAbout(wxCommandEvent& event)
{
  wxMessageBox( "This is a wxWidgets' Hello world sample",
      "About Hello World", wxOK | wxICON_INFORMATION );
}

void MyFrame::OnHello(wxCommandEvent& event)
{
  wxLogMessage("Hello world from wxWidgets!");
}

