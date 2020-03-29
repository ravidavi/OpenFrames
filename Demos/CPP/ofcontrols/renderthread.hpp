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

#ifndef RENDERTHREAD_H
#define RENDERTHREAD_H

#include <QThread>
#include <QOpenGLFunctions>
#include "ofrenderpool.hpp"

#include <OpenFrames/WindowProxy.hpp>

// forward declaration to avoid circular dependencies
QT_FORWARD_DECLARE_CLASS(QWindow)
QT_FORWARD_DECLARE_CLASS(QOpenGLContext)
QT_FORWARD_DECLARE_CLASS(QOpenGLShaderProgram)

/**********************************************************
 * Matthew Ruschmann
 * OpenFrames ofqt Example, class RenderProxy
 * Manages an OpenFrames WindowProxy and the objectst that
 * it renders. Provides a callbacks for WindowProxy for
 * controlling the OpenGL Surface provided by _window.
**********************************************************/
class RenderThread : public QThread, public OFRendererIF, protected QOpenGLFunctions
{
    /** Enable Qt signals and slots for this object */
    Q_OBJECT

public:
    /** Constructor */
    RenderThread(QObject *parent, int width, int height, int nRows, int nCols, bool useStereo);
    /** Destructor */
    virtual ~RenderThread();

    /** Overrides QThread::run() implement the main loop of OpenFrames::WindowProxy */
    void run() override;

    /** Implementations of OFRenderIF interfaces */
    void begin(QWindow *w) override;
    void end() override;
    OpenFrames::WindowProxy *winproxy() override { return _winproxy; }
    bool makeCurrent() override;
    void swapBuffers() override;
    void keyPressCallback(int key) override;

signals:
    /** Use a signal to implement a message whenever the view may have changed */
    void userSelectedView(int upperIndex, int lowerIndex);

private:
    /** The surface that _winproxy will render to */
    QWindow *_window;
    /** An context for rendering to _window */
    QOpenGLContext *_context;

    /** A proxy that will be rendering the OpenFrames scene */
    osg::ref_ptr<OpenFrames::WindowProxy> _winproxy;
};

#endif
