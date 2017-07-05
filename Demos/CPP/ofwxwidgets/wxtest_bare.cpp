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

// include OpenGL
#ifdef __WXMAC__
#include "OpenGL/glu.h"
#include "OpenGL/gl.h"
#else
#include <GL/glu.h>
#include <GL/gl.h>
#endif

#include <iostream>
#include <sstream>

class BasicGLPane : public wxGLCanvas
{
  wxGLContext*  m_context;

  public:
  BasicGLPane(wxFrame* parent, int* args);
  virtual ~BasicGLPane();

  void resized(wxSizeEvent& evt);

  int getWidth();
  int getHeight();

  void render(wxPaintEvent& evt);
  void prepare3DViewport(int topleft_x, int topleft_y, int bottomright_x, int bottomright_y);
  void prepare2DViewport(int topleft_x, int topleft_y, int bottomright_x, int bottomright_y);

  // events
  void mouseMoved(wxMouseEvent& event) 
  {
    wxFrame *p_frm = wxStaticCast(GetParent(), wxFrame);
    std::ostringstream os;
    os << "mouseMoved to (" << event.GetX() << ", " << event.GetY() << ")";
    p_frm->SetStatusText(os.str());
  }
  void mouseDown(wxMouseEvent& event)
  {
    wxFrame *p_frm = wxStaticCast(GetParent(), wxFrame);
    std::ostringstream os;
    os << "mouseDown at (" << event.GetX() << ", " << event.GetY() << ")";
    p_frm->SetStatusText(os.str());
  }
  void mouseWheelMoved(wxMouseEvent& event) {}
  void mouseReleased(wxMouseEvent& event) {}
  void rightClick(wxMouseEvent& event) {}
  void mouseLeftWindow(wxMouseEvent& event) {}
  void keyPressed(wxKeyEvent& event) {}
  void keyReleased(wxKeyEvent& event) {}

  wxDECLARE_EVENT_TABLE();
};

class MyApp: public wxApp
{
  public:
    virtual bool OnInit();

  private:
    wxFrame *frame;
    BasicGLPane *glPane;
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

wxBEGIN_EVENT_TABLE(BasicGLPane, wxGLCanvas)
  EVT_MOTION(BasicGLPane::mouseMoved)
  EVT_LEFT_DOWN(BasicGLPane::mouseDown)
  EVT_LEFT_UP(BasicGLPane::mouseReleased)
  EVT_RIGHT_DOWN(BasicGLPane::rightClick)
  EVT_LEAVE_WINDOW(BasicGLPane::mouseLeftWindow)
  EVT_SIZE(BasicGLPane::resized)
  EVT_KEY_DOWN(BasicGLPane::keyPressed)
  EVT_KEY_UP(BasicGLPane::keyReleased)
  EVT_MOUSEWHEEL(BasicGLPane::mouseWheelMoved)
  EVT_PAINT(BasicGLPane::render)
wxEND_EVENT_TABLE()

wxIMPLEMENT_APP(MyApp);

bool MyApp::OnInit()
{
  wxBoxSizer *sizer = new wxBoxSizer(wxVERTICAL);
  frame = new MyFrame( "Hello GL World", wxPoint(50, 50), wxSize(450, 340) );

  int args[] = {WX_GL_RGBA, WX_GL_DOUBLEBUFFER, WX_GL_DEPTH_SIZE, 16, 0};

  glPane = new BasicGLPane( (wxFrame*) frame, args);
  sizer->Add(glPane, 1, wxEXPAND);

  frame->SetSizer(sizer);
  frame->SetAutoLayout(true);

  frame->Show( true );

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

// Vertices and faces of a simple cube to demonstrate 3D render
// source: http://www.opengl.org/resources/code/samples/glut_examples/examples/cube.c
GLfloat v[8][3];
GLint faces[6][4] = {  /* Vertex indices for the 6 faces of a cube. */
  {0, 1, 2, 3}, {3, 2, 6, 7}, {7, 6, 5, 4},
  {4, 5, 1, 0}, {5, 6, 2, 1}, {7, 4, 0, 3} };

BasicGLPane::BasicGLPane(wxFrame* parent, int* args) 
: wxGLCanvas(parent, wxID_ANY, args, wxDefaultPosition, wxDefaultSize, wxFULL_REPAINT_ON_RESIZE)
{
  m_context = new wxGLContext(this);
  // prepare a simple cube to demonstrate 3D render
  // source: http://www.opengl.org/resources/code/samples/glut_examples/examples/cube.c
  v[0][0] = v[1][0] = v[2][0] = v[3][0] = -1;
  v[4][0] = v[5][0] = v[6][0] = v[7][0] = 1;
  v[0][1] = v[1][1] = v[4][1] = v[5][1] = -1;
  v[2][1] = v[3][1] = v[6][1] = v[7][1] = 1;
  v[0][2] = v[3][2] = v[4][2] = v[7][2] = 1;
  v[1][2] = v[2][2] = v[5][2] = v[6][2] = -1;    

  // To avoid flashing on MSW
  SetBackgroundStyle(wxBG_STYLE_CUSTOM);
}

BasicGLPane::~BasicGLPane()
{
  delete m_context;
}

void BasicGLPane::resized(wxSizeEvent& evt)
{
  //  wxGLCanvas::OnSize(evt);

  wxGLCanvas::SetCurrent(*m_context);
  Refresh();
}

/** Inits the OpenGL viewport for drawing in 3D. */
void BasicGLPane::prepare3DViewport(int topleft_x, int topleft_y, int bottomright_x, int bottomright_y)
{

  glClearColor(1.0f, 0.0f, 1.0f, 1.0f); // Black Background
  glClearDepth(1.0f);	// Depth Buffer Setup
  glEnable(GL_DEPTH_TEST); // Enables Depth Testing
  glDepthFunc(GL_LEQUAL); // The Type Of Depth Testing To Do
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  glEnable(GL_COLOR_MATERIAL);

  glViewport(topleft_x, topleft_y, bottomright_x-topleft_x, bottomright_y-topleft_y);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  float ratio_w_h = (float)(bottomright_x-topleft_x)/(float)(bottomright_y-topleft_y);
  gluPerspective(45 /*view angle*/, ratio_w_h, 0.1 /*clip close*/, 200 /*clip far*/);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

}

/** Inits the OpenGL viewport for drawing in 2D. */
void BasicGLPane::prepare2DViewport(int topleft_x, int topleft_y, int bottomright_x, int bottomright_y)
{
  glClearColor(1.0f, 1.0f, 0.0f, 1.0f); // Black Background
  glEnable(GL_TEXTURE_2D);   // textures
  glEnable(GL_COLOR_MATERIAL);
  glEnable(GL_BLEND);
  glDisable(GL_DEPTH_TEST);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);

  glViewport(topleft_x, topleft_y, bottomright_x-topleft_x, bottomright_y-topleft_y);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  gluOrtho2D(topleft_x, bottomright_x, bottomright_y, topleft_y);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}

int BasicGLPane::getWidth()
{
  return GetSize().x;
}

int BasicGLPane::getHeight()
{
  return GetSize().y;
}


void BasicGLPane::render( wxPaintEvent& evt )
{
  if(!IsShown()) return;

  static int i = 2;
  if(i > 0)
  {
    wxGLCanvas::SetCurrent(*m_context);
    i--;
  }

  wxPaintDC(this); // only to be used in paint events. use wxClientDC to paint outside the paint event

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  // ------------- draw some 2D ----------------
  prepare2DViewport(0,0,getWidth()/2, getHeight());
  glLoadIdentity();

  // white background
  glColor4f(1, 1, 1, 1);
  glBegin(GL_QUADS);
  glVertex3f(0,0,0);
  glVertex3f(getWidth(),0,0);
  glVertex3f(getWidth(),getHeight(),0);
  glVertex3f(0,getHeight(),0);
  glEnd();

  // red square
  glColor4f(1, 0, 0, 1);
  glBegin(GL_QUADS);
  glVertex3f(getWidth()/8, getHeight()/3, 0);
  glVertex3f(getWidth()*3/8, getHeight()/3, 0);
  glVertex3f(getWidth()*3/8, getHeight()*2/3, 0);
  glVertex3f(getWidth()/8, getHeight()*2/3, 0);
  glEnd();

  // ------------- draw some 3D ----------------
  prepare3DViewport(getWidth()/2,0,getWidth(), getHeight());
  glLoadIdentity();

  glColor4f(0,0,1,1);
  glTranslatef(0,0,-5);
  glRotatef(50.0f, 0.0f, 1.0f, 0.0f);

  glColor4f(1, 0, 0, 1);
  for (int i = 0; i < 6; i++)
  {
    glBegin(GL_LINE_STRIP);
    glVertex3fv(&v[faces[i][0]][0]);
    glVertex3fv(&v[faces[i][1]][0]);
    glVertex3fv(&v[faces[i][2]][0]);
    glVertex3fv(&v[faces[i][3]][0]);
    glVertex3fv(&v[faces[i][0]][0]);
    glEnd();
  }

  glFlush();
  SwapBuffers();

  // check OpenGL error
  GLenum err;
  while ((err = glGetError()) != GL_NO_ERROR) 
  {
    std::cout << "OpenGL error: " << err << std::endl;
  }
}
