/***********************************
Copyright 2023 Ravishankar Mathur

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

#ifndef OFWINDOW_H
#define OFWINDOW_H

#include "renderthread.hpp"
#include <QWindow>

class OFRendererIF;

class OFWindow : public QWindow
{
public:
    OFWindow(OFRendererIF &renderer, QWindow *parent = nullptr);
    virtual ~OFWindow();

    void exposeEvent(QExposeEvent *event) override;

    static const bool VERBOSE_CONSOLE;

protected:
    void mousePressEvent(QMouseEvent *event) override;
    void mouseReleaseEvent(QMouseEvent *event) override;
    void mouseMoveEvent(QMouseEvent *event) override;
    void wheelEvent(QWheelEvent *event) override;
    void keyPressEvent(QKeyEvent *event) override;
    void resizeEvent(QResizeEvent *event) override;
    void timerEvent(QTimerEvent *event) override;

private:
    unsigned int mapQtButtonToOFButton(Qt::MouseButtons qButton);
    int mapQtKeyEventToOsgKey(QKeyEvent *event);

    OFRendererIF &_renderer;
    bool _alreadyExposed;
    int _timerID;
};

#endif
