/****************************************************************************
**
** Copyright (C) 2016 The Qt Company Ltd.
** Contact: https://www.qt.io/licensing/
**
** This file is part of the examples of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:BSD$
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and The Qt Company. For licensing terms
** and conditions see https://www.qt.io/terms-conditions. For further
** information use the contact form at https://www.qt.io/contact-us.
**
** BSD License Usage
** Alternatively, you may use this file under the terms of the BSD license
** as follows:
**
** "Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are
** met:
**   * Redistributions of source code must retain the above copyright
**     notice, this list of conditions and the following disclaimer.
**   * Redistributions in binary form must reproduce the above copyright
**     notice, this list of conditions and the following disclaimer in
**     the documentation and/or other materials provided with the
**     distribution.
**   * Neither the name of The Qt Company Ltd nor the names of its
**     contributors may be used to endorse or promote products derived
**     from this software without specific prior written permission.
**
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
** "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
** LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
** A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
** OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
** SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
** LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
** DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
** THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
** OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
**
** $QT_END_LICENSE$
**
****************************************************************************/

#include "ofwindow.h"
#include <QMouseEvent>

const bool OFWindow::VERBOSE_CONSOLE = false;

OFWindow::OFWindow(QWindow *parent)
    : QWindow(parent),
      m_renderer(*this),
      m_alreadyExposed(false)
{
    // Surface type shall be OpenGL
    setSurfaceType(QWindow::OpenGLSurface);
}

OFWindow::~OFWindow()
{
    // Wait for the RenderThread to stop
    m_renderer.stop();
    m_renderer.wait();
}

void OFWindow::exposeEvent(QExposeEvent *)
{
    // The renderer shall not be started until the window is exposed for the first time
    if (!m_alreadyExposed) {
        m_renderer.start();
        m_alreadyExposed = true;
    }
    // else Do not attempt to start the renderer twice!
}

unsigned int OFWindow::mapQtButtonToOFButton(Qt::MouseButtons qButton)
{
    unsigned int button = 0;

    if (qButton == Qt::LeftButton) {
        button = 1;
    }
    else if (qButton == Qt::RightButton) {
        button = 3;
    }

    return button;
}

int OFWindow::mapQtKeyEventToOsgKey(QKeyEvent *event) {
    int key;

    // Convert uppercase to lowercase as necessary
    if (event->key() >= Qt::Key_A && event->key() <= Qt::Key_Z) {
        key = static_cast<int>(event->text().at(0).toLatin1());
    }
    else {
        key = event->key();
    }

    return key;
}

void OFWindow::mousePressEvent(QMouseEvent *event)
{
    unsigned int button = mapQtButtonToOFButton(event->button());

    if (m_alreadyExposed) {
        if (button != 0) {
            if (VERBOSE_CONSOLE) {
                if (event->button() == 1) {
                    qDebug() << "mouseDown left at (" << event->x() << ", " << event->y() << ")";
                }
                else if (event->button() == 3) {
                    qDebug() << "mouseDown right at (" << event->x() << ", " << event->y() << ")";
                }
            }

            if (m_renderer.winproxy() != 0x0) {
                m_renderer.winproxy()->buttonPress(event->x(), event->y(), button);
            }
        }
    }
}

void OFWindow::mouseReleaseEvent(QMouseEvent *event)
{
    unsigned int button = mapQtButtonToOFButton(event->button());

    if (m_alreadyExposed) {
        if (button != 0) {
            if (VERBOSE_CONSOLE) {
                if (event->button() == 1) {
                    qDebug() << "mouseUp left at (" << event->x() << ", " << event->y() << ")";
                }
                else if (event->button() == 3) {
                    qDebug() << "mouseUp right at (" << event->x() << ", " << event->y() << ")";
                }
            }

            if (m_renderer.winproxy() != 0x0) {
                m_renderer.winproxy()->buttonRelease(event->x(), event->y(), button);
            }
        }
    }
}

void OFWindow::mouseMoveEvent(QMouseEvent *event)
{
    if (m_alreadyExposed) {
        if (VERBOSE_CONSOLE) {
            qDebug() << "mouseMoved to (" << event->x() << ", " << event->y() << ")";
        }
        if (m_renderer.winproxy() != 0x0) {
            m_renderer.winproxy()->mouseMotion(event->x(), event->y());
        }
    }
}

void OFWindow::keyPressEvent(QKeyEvent *event)
{
    int key;
    
    if (m_alreadyExposed) {
        if (VERBOSE_CONSOLE) {
            qDebug() << "keyPressed " << event->key() << " (" << (char)event->key() << ")";
        }
        if (m_renderer.winproxy() != 0x0) {
            key = mapQtKeyEventToOsgKey(event);
            m_renderer.winproxy()->keyPress(key);
        }
    }
}

void OFWindow::resizeEvent(QResizeEvent *event) {
    if (m_renderer.winproxy() != 0x0) {
        if (m_renderer.winproxy()->isAnimating()) {
            m_renderer.winproxy()->resizeWindow(0, 0, event->size().width(), event->size().height());
        }
    }
    QWindow::resizeEvent(event);
}
