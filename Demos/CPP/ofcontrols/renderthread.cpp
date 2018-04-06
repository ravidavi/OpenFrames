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

#include "renderthread.hpp"
#include <QOpenGLContext>
#include <QWindow>

#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

RenderThread::RenderThread(QObject *parent, int width, int height, int nRows, int nCols, bool useStereo)
    : QThread(parent),
      _window(nullptr),
      _context(nullptr),
      _winproxy(nullptr)
{
    // Create embedded WindowProxy that handles all OpenFrames drawing
    _winproxy = new OpenFrames::WindowProxy(0, 0, width, height, nRows, nCols, true, useStereo);
    _winproxy->setID(0);
    _winproxy->setMakeCurrentFunction(OFRenderPool::dealMakeCurrent);
    _winproxy->setUpdateContextFunction(OFRenderPool::dealMakeCurrent);
    _winproxy->setSwapBuffersFunction(OFRenderPool::dealSwapBuffers);
    _winproxy->setTimeScale(1.0);

    // Specify the key press callback
    _winproxy->setKeyPressCallback(OFRenderPool::dealKeyPressCallback);
}

RenderThread::~RenderThread()
{
    // Wait for the window proxy to shutdown before deleting the context
    if (_context != nullptr) {
        delete _context;
        _context = nullptr;
    }
    // else No _context to delete
}

void RenderThread::begin(QWindow *w)
{
    if (w != nullptr) {
        _window = w;
        start();
    }
    // else Do not start
}

void RenderThread::end()
{
    _winproxy->shutdown();
    wait();
}

void RenderThread::run()
{
    bool success = false;

    if (_context == nullptr) {
        if (_window != nullptr) {
            _context = new QOpenGLContext();
            _context->create();
            success = _context->makeCurrent(_window);
            if (success) {
                initializeOpenGLFunctions();
                _context->doneCurrent();
            }
        }
    }
    // else Continue using the old context

    if (success) {
        _winproxy->run();
    }
    // else Had an error, do not start running
}

void RenderThread::keyPressCallback(int key)
{
    if (key == 'p') {
        // Pause/unpause animation
        _winproxy->pauseTime(!_winproxy->isTimePaused());
    }
    else if (key == 'r') {
        // Reset time to epoch. All ReferenceFrames that are following
        // a Trajectory will return to their starting positions.
      _winproxy->setTime(0.0);
    }
    else if ((key == Qt::Key_Plus) || (key == Qt::Key_Equal)) {
        // Speed up time
        _winproxy->setTimeScale(_winproxy->getTimeScale() + 0.1);
    }
    else if ((key == Qt::Key_Minus) || (key == Qt::Key_Underscore)) {
        // Slow down time
        _winproxy->setTimeScale(_winproxy->getTimeScale() - 0.1);
    }
    else if (key == Qt::Key_Right) {
        // Shift time forward
        _winproxy->setTime(_winproxy->getTime() + 0.1);
    }
    else if (key == Qt::Key_Left) {
        // Shift time backward
        _winproxy->setTime(_winproxy->getTime() - 0.1);
    }
    // else Ignore key press
}

bool RenderThread::makeCurrent()
{
    bool success;

    if (_context != nullptr && _window != nullptr) {
        success = _context->makeCurrent(_window);

        // check OpenGL errors
        GLenum err;
        while ((err = glGetError()) != GL_NO_ERROR) {
            std::cout << "OpenGL error: " << err << std::endl;
        }
    }
    // else Cannot make current unless initialized properly

    return success;
}

void RenderThread::swapBuffers()
{
    if (_context != nullptr && _window != nullptr) {
        // call swapbuffers
        _context->swapBuffers(_window);
    }
    // else Cannot swap buffers unless initialized properly
}
