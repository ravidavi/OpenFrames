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

// wxWidgets "Hello world" Program, using OpenFrames
// For compilers that support precompilation, includes "wx/wx.h".
#include <wx/wxprec.h>
#ifndef WX_PRECOMP
#include <wx/wx.h>
#endif
#include <wx/glcanvas.h>

#include <OpenFrames/WindowProxy.hpp>

class MyGLCanvas : public wxGLCanvas
{
  private:
  wxGLContext* m_context = NULL;
  OpenFrames::WindowProxy* m_winproxy = NULL;

  public:
  MyGLCanvas(wxFrame* parent, int* args);
  virtual ~MyGLCanvas();

  // Functions that allow OpenFrames to manage the OpenGL context
  // created by wxWidgets from a separate thread
  static MyGLCanvas* currCanvas;
  static void makecurrent(unsigned int *winID, bool *success);
  static void swapbuffers(unsigned int *winID);

  // Start OpenFrames in a separate thread
  void StartOpenFrames();

  // events
  void resized(wxSizeEvent& event);

  void mouseMoved(wxMouseEvent& event);
  void mouseDownLeft(wxMouseEvent& event);
  void mouseUpLeft(wxMouseEvent& event);
  void mouseDownRight(wxMouseEvent& event);
  void mouseUpRight(wxMouseEvent& event);
  void keyPressed(wxKeyEvent& event);

  wxDECLARE_EVENT_TABLE();
};

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
