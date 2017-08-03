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

#ifndef RENDERWINDOW_H
#define RENDERWINDOW_H

#include <QOpenGLFunctions>
#include <QOpenGLBuffer>
#include <QMatrix4x4>
#include <QVector>
#include "ofwindow.h"
#include "ofrenderpool.h"

#include <OpenFrames/WindowProxy.hpp>
#include <OpenFrames/CoordinateAxes.hpp>
#include <OpenFrames/Model.hpp>

// forward declaration to avoid circular dependencies
QT_FORWARD_DECLARE_CLASS(QWindow)
QT_FORWARD_DECLARE_CLASS(QOpenGLContext)
QT_FORWARD_DECLARE_CLASS(QOpenGLShaderProgram)

/**********************************************************
 * Matthew Ruschmann
 * OpenFrames ofqt Example, class RenderProxy
 * Manages an OpenFrames WindowProxy and the objectst that
 * it renders. Provides a callbacks for WindowProxy for
 * controlling the OpenGL Surface provided by m_window.
**********************************************************/
class RenderProxy : public OFRendererIF, protected QOpenGLFunctions
{
public:
    /** Constructor */
    RenderProxy(QObject *parent = 0x0);
    /** Destructor */
    virtual ~RenderProxy();

    /** Implementations of OFRenderIF interfaces */
    void begin(QWindow *w) override;
    void end() override;
    OpenFrames::WindowProxy *winproxy() override { return m_winproxy; }
    bool makeCurrent() override;
    void swapBuffers() override;
    void keyPressCallback(int key) override;

private:
    /** The surface that m_winproxy will render to */
    QWindow *m_window;
    /** An context for rendering to m_window */
    QOpenGLContext *m_context;

    /** A proxy that will be rendering the OpenFrames scene */
    OpenFrames::WindowProxy *m_winproxy;
    /** OpenFrames objects for rendering the scene */
    OpenFrames::Model *m_spacestation;
    OpenFrames::CoordinateAxes *m_axes;
    OpenFrames::TimeManagementVisitor *m_timeManVisitor;

    /** States of the OpenFrames scene */
    double m_tscale; // Animation speedup relative to real time
    double m_toffset; // Animation time offset
    bool m_paused;
    bool m_stereo;
};

#endif
