/***********************************
  Copyright 2015 Ravishankar Mathur

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

// wxWidgets "Hello world" Program
// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>
#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif
#include <wx/glcanvas.h>
#include <wx/sizer.h>
#include <wx/dcclient.h>

#include <iostream>
#include <sstream>

#include <OpenFrames/FrameManager>
#include <OpenFrames/WindowProxy>

class MyGLCanvas : public wxGLCanvas
{
  private:
  wxGLContext* m_context = NULL;
  OpenFrames::WindowProxy* m_winproxy = NULL;

  public:
  MyGLCanvas(wxFrame* parent, int* args);
  virtual ~MyGLCanvas();

  static MyGLCanvas* currCanvas;

  static void makecurrent(unsigned int *winID, bool *success)
  {
    *success = currCanvas->m_context->SetCurrent(*currCanvas);

    // check OpenGL errors
    GLenum err;
    while ((err = glGetError()) != GL_NO_ERROR)
    {
      std::cout << "OpenGL error: " << err << std::endl;
    }
  }

  static void swapbuffers(unsigned int *winID)
  {
    currCanvas->SwapBuffers();
  }

  void StartOpenFrames();

  // events
  void resized(wxSizeEvent& event) 
  {
    wxFrame *p_frm = wxStaticCast(GetParent(), wxFrame);
    std::ostringstream os;
    os << "Window resized to (" << GetSize().GetWidth() << ", " << GetSize().GetHeight() << ")";
    p_frm->SetStatusText(os.str());

    if(m_winproxy->isAnimating()) 
    {
      m_winproxy->resizeWindow(0, 0, GetSize().GetWidth(), GetSize().GetHeight());
    }
    else
      std::cerr<< "Resized before animating" << std::endl;
  }

  void eraseBackground(wxEraseEvent& event)
  {
  }

  void paint(wxPaintEvent& event) 
  {
  }

  void mouseMoved(wxMouseEvent& event) 
  {
    wxFrame *p_frm = wxStaticCast(GetParent(), wxFrame);
    std::ostringstream os;
    os << "mouseMoved to (" << event.GetX() << ", " << event.GetY() << ")";
    p_frm->SetStatusText(os.str());

    m_winproxy->mouseMotion(event.GetX(), event.GetY());
  }

  void mouseDownLeft(wxMouseEvent& event)
  {
    wxFrame *p_frm = wxStaticCast(GetParent(), wxFrame);
    std::ostringstream os;
    os << "mouseDown left at (" << event.GetX() << ", " << event.GetY() << ")";
    p_frm->SetStatusText(os.str());

    m_winproxy->buttonPress(event.GetX(), event.GetY(), 1);
  }

  void mouseUpLeft(wxMouseEvent& event)
  {
    wxFrame *p_frm = wxStaticCast(GetParent(), wxFrame);
    std::ostringstream os;
    os << "mouseUp left at (" << event.GetX() << ", " << event.GetY() << ")";
    p_frm->SetStatusText(os.str());

    m_winproxy->buttonRelease(event.GetX(), event.GetY(), 1);
  }

  void mouseDownRight(wxMouseEvent& event) {}
  void mouseUpRight(wxMouseEvent& event) {}

  void mouseWheelMoved(wxMouseEvent& event) {}
  void mouseLeftWindow(wxMouseEvent& event) {}
  void keyPressed(wxKeyEvent& event) {}
  void keyReleased(wxKeyEvent& event) {}

  wxDECLARE_EVENT_TABLE();
};

MyGLCanvas* MyGLCanvas::currCanvas = NULL;

class MyApp: public wxApp
{
  public:
    virtual bool OnInit();

  private:
    wxFrame *frame;
    MyGLCanvas *glCanvas;
};

class MyFrame: public wxFrame
{
  public:
    MyFrame(const wxString& title, const wxPoint& pos, const wxSize& size);
  private:
    void OnHello(wxCommandEvent& event);
    void OnExit(wxCommandEvent& event);
    void OnAbout(wxCommandEvent& event);
    wxDECLARE_EVENT_TABLE();
};

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
  EVT_LEAVE_WINDOW(MyGLCanvas::mouseLeftWindow)
  EVT_SIZE(MyGLCanvas::resized)
  EVT_KEY_DOWN(MyGLCanvas::keyPressed)
  EVT_KEY_UP(MyGLCanvas::keyReleased)
  EVT_MOUSEWHEEL(MyGLCanvas::mouseWheelMoved)
  //EVT_ERASE_BACKGROUND(MyGLCanvas::eraseBackground)
  //EVT_PAINT(MyGLCanvas::paint)
wxEND_EVENT_TABLE()

wxIMPLEMENT_APP(MyApp);

bool MyApp::OnInit()
{
  frame = new MyFrame( "Hello OpenFrames World", wxPoint(50, 50), wxSize(450, 340) );

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

MyGLCanvas::MyGLCanvas(wxFrame* parent, int* args) 
: wxGLCanvas(parent, wxID_ANY, args, wxDefaultPosition, wxDefaultSize, wxFULL_REPAINT_ON_RESIZE)
{
  m_context = new wxGLContext(this);
  
  // Create embedded WindowProxy that handles all OpenFrames drawing
  m_winproxy = new OpenFrames::WindowProxy(0, 0, 450, 300, 1, 1, true);
  m_winproxy->setID(0);
  m_winproxy->setMakeCurrentFunction(MyGLCanvas::makecurrent);
  m_winproxy->setUpdateContextFunction(MyGLCanvas::makecurrent);
  m_winproxy->setSwapBuffersFunction(MyGLCanvas::swapbuffers);
  m_winproxy->setDesiredFramerate(20);

  // Create a basic ReferenceFrame
  OpenFrames::ReferenceFrame* refframe = new OpenFrames::ReferenceFrame("Hello World!");

  // Create a FrameManager that will synchronize access to the scene
  OpenFrames::FrameManager* fm = new OpenFrames::FrameManager;
  fm->setFrame(refframe);

  m_winproxy->setScene(fm, 0, 0);

  // To avoid flashing on MSW
  SetBackgroundStyle(wxBG_STYLE_CUSTOM);
}

MyGLCanvas::~MyGLCanvas()
{
  m_winproxy->shutdown();
  m_winproxy->join();
  delete m_context;
}

void MyGLCanvas::StartOpenFrames()
{
  m_winproxy->startThread();
  while(!m_winproxy->isAnimating())
    OpenThreads::Thread::YieldCurrentThread();
}
