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

#include "ofwindow.h"
#include "ofrenderpool.h"
#include "ofwidget.h"
#include <QGridLayout>

OFWidget::OFWidget(OFRendererIF &renderer, QWidget *parent)
    : QWidget(parent),
      m_sizeHint(400, 350),
      m_renderer(renderer),
      m_window(0x0),
      m_container(0x0)
{
    // Create a window for OpenFrames to render to
    m_window = new OFWindow(m_renderer);
    // Create a QWidget window container for the QWindow above
    m_container = QWidget::createWindowContainer(m_window, this);
    // Place the container widget inside of this widget
    setLayout(new QGridLayout());
    layout()->addWidget(m_container);
}

OFWidget::~OFWidget()
{
    if (m_container != 0x0) {
        delete m_container;
        // m_window gets deleted by m_container
    }
    else {
        // delete m_window because m_container delete was not called
        if (m_window != 0x0) {
            delete m_window;
        }
    }
}

QSize OFWidget::sizeHint() const {
    return m_sizeHint;
}

void OFWidget::setSizeHint(int width, int height) {
    m_sizeHint.setWidth(width);
    m_sizeHint.setHeight(height);
}
