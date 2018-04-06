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

#include "ofrenderpool.hpp"
#include "ofwindow.hpp"
#include <QMouseEvent>
#include <QDebug>

const bool OFWindow::VERBOSE_CONSOLE = false;

OFWindow::OFWindow(OFRendererIF &renderer, QWindow *parent)
    : QWindow(parent),
      _renderer(renderer),
      _alreadyExposed(false),
      _timerID(0)
{
    // Surface type shall be OpenGL
    setSurfaceType(QWindow::OpenGLSurface);
}

OFWindow::~OFWindow()
{
    // Wait for the RenderThread to stop
    _renderer.end();
}

void OFWindow::exposeEvent(QExposeEvent *)
{
    // The renderer shall not be started until the window is exposed for the first time
    if (!_alreadyExposed) {
        _renderer.begin(this);
        _alreadyExposed = true;

        // Start checking for animation to resize the window
        _timerID = startTimer(50);
    }
    // else Do not attempt to start the renderer twice!
}

unsigned int OFWindow::mapQtButtonToOFButton(Qt::MouseButtons qButton)
{
    unsigned int button;

    switch (qButton) {
      case Qt::LeftButton: {
          button = 1;
          break;
      }
      case Qt::RightButton: {
          button = 3;
          break;
      }
      case Qt::MiddleButton: {
          button = 2;
          break;
      }
      case Qt::BackButton: {
          button = 6;
          break;
      }
      case Qt::ForwardButton: {
          button = 7;
          break;
      }
      default: {
          button = 0;
          break;
      }
    }

    return button;
}

int OFWindow::mapQtKeyEventToOsgKey(QKeyEvent *event) {
    int key;

    // Convert uppercase to lowercase as necessary
    if (Qt::Key_A && event->key() <= Qt::Key_Z) {
        if (event->modifiers() & Qt::ShiftModifier) {
            key = event->key();
        }
        else {
            key = event->key() + 0x20;
        }
    }
    else {
        key = event->key();
    }

    return key;
}

void OFWindow::mousePressEvent(QMouseEvent *event)
{
    unsigned int button = mapQtButtonToOFButton(event->button());

    if (_renderer.winproxy() != nullptr) {
        if (_renderer.winproxy()->isAnimating()) {
            if (button != 0) {
                if (VERBOSE_CONSOLE) {
                    qDebug() << "mouseDown " << button << " at (" << event->x() << ", " << event->y() << ")";
                }
                if (_renderer.winproxy() != nullptr) {
                    _renderer.winproxy()->buttonPress(event->x(), event->y(), button);
                }
            }
        }
    }
}

void OFWindow::mouseReleaseEvent(QMouseEvent *event)
{
    unsigned int button = mapQtButtonToOFButton(event->button());

    if (_renderer.winproxy() != nullptr) {
        if (_renderer.winproxy()->isAnimating()) {
            if (button != 0) {
                if (VERBOSE_CONSOLE) {
                    qDebug() << "mouseUp " << button << " at (" << event->x() << ", " << event->y() << ")";
                }
                if (_renderer.winproxy() != nullptr) {
                    _renderer.winproxy()->buttonRelease(event->x(), event->y(), button);
                }
            }
        }
    }
}

void OFWindow::wheelEvent(QWheelEvent *event) {
    QWindow::wheelEvent(event);
}

void OFWindow::mouseMoveEvent(QMouseEvent *event)
{
    if (_renderer.winproxy() != nullptr) {
        if (_renderer.winproxy()->isAnimating()) {
            if (VERBOSE_CONSOLE) {
                qDebug() << "mouseMoved to (" << event->x() << ", " << event->y() << ")";
            }
            if (_renderer.winproxy() != nullptr) {
                _renderer.winproxy()->mouseMotion(event->x(), event->y());
            }
        }
    }
}

void OFWindow::keyPressEvent(QKeyEvent *event)
{
    int key;
    
    if (_renderer.winproxy() != nullptr) {
        if (_renderer.winproxy()->isAnimating()) {
            key = mapQtKeyEventToOsgKey(event);
            if (VERBOSE_CONSOLE) {
                qDebug() << "keyPressed " << key << " (" << (char)key << ")";
            }
            if (_renderer.winproxy() != nullptr) {
                _renderer.winproxy()->keyPress(key);
            }
        }
    }
}

void OFWindow::resizeEvent(QResizeEvent *event) {
    if (_renderer.winproxy() != nullptr) {
        if (_renderer.winproxy()->isAnimating()) {
            _renderer.winproxy()->resizeWindow(0, 0, event->size().width(), event->size().height());
        }
    }
    QWindow::resizeEvent(event);
}

void OFWindow::timerEvent(QTimerEvent *event) {
    if (event->timerId() == _timerID) {
        // Check if winproxy has started animating, and resize the window when it has started
        if (_renderer.winproxy() != nullptr) {
            if (_renderer.winproxy()->isAnimating()) {
                _renderer.winproxy()->resizeWindow(0, 0, size().width(), size().height());
                killTimer(_timerID);
                _timerID = 0;
            }
        }
    }
}
