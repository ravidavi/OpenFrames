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

#include "ofwidget.h"
#include "renderthread.h"
#include "renderproxy.h"
#include "mainwindow.h"
#include <QComboBox>
#include <QVBoxLayout>
#include <QKeyEvent>
#include <QApplication>

MainWindow::MainWindow()
    : m_renderer(0x0),
      m_containerWidget(0x0),
      m_topComboBox(0x0),
      m_bottomComboBox(0x0)
{
    // Create a renderer for the OFWidget
    if (QApplication::arguments().contains(QStringLiteral("--qthread"))) {
        // OpenFrames runs on a QThread
        m_renderer = new RenderThread();
    }
    else {
        // OpenFrames runs its own thread
        m_renderer = new RenderProxy();
    }

    // Create and connect QComboboxes
    m_topComboBox = createTopViewComboBox();
    m_bottomComboBox = createBottomViewComboBox();
    connect(m_topComboBox, QOverload<int>::of(&QComboBox::currentIndexChanged), this, &MainWindow::handleTopViewChanged);
    connect(m_bottomComboBox, QOverload<int>::of(&QComboBox::currentIndexChanged), this, &MainWindow::handleBottomViewChanged);

    // Create another widget to hold the container widget and provide size hints
    m_containerWidget = new OFWidget(*m_renderer);

    // Add widgets to a horizontal layout
    QVBoxLayout *container = new QVBoxLayout();
    container->addWidget(m_topComboBox);
    container->addWidget(m_containerWidget);
    container->addWidget(m_bottomComboBox);

    // Add layout to a central widget
    QWidget *w = new QWidget();
    w->setLayout(container);
    setCentralWidget(w);

    setWindowTitle(tr("Hello OpenFrames from Qt"));
}

MainWindow::~MainWindow()
{
    if (m_containerWidget != 0x0) {
        delete m_containerWidget;
        m_containerWidget = 0x0;
    }
    if (m_topComboBox != 0x0) {
        delete m_topComboBox;
        m_topComboBox = 0x0;
    }
    if (m_bottomComboBox != 0x0) {
        delete m_bottomComboBox;
        m_bottomComboBox = 0x0;
    }
    if (m_renderer != 0x0) {
        delete m_renderer;
        m_renderer = 0x0;
    }
}

QComboBox *MainWindow::createTopViewComboBox()
{
    QComboBox *box = new QComboBox();
    box->addItem("Spacestation View");
    box->addItem("Hubble View");
    return box;
}

QComboBox *MainWindow::createBottomViewComboBox()
{
    QComboBox *box = new QComboBox();
    box->addItem("Axes View");
    box->addItem("Trace View");
    return box;
}

void MainWindow::handleTopViewChanged(int index) {
    if (m_renderer != 0x0) {
        if (m_renderer->winproxy() != 0x0) {
            m_renderer->winproxy()->getGridPosition(0, 0)->selectView(index);
        }
    }
}

void MainWindow::handleBottomViewChanged(int index) {
    if (m_renderer != 0x0) {
        if (m_renderer->winproxy() != 0x0) {
            m_renderer->winproxy()->getGridPosition(1, 0)->selectView(index);
        }
    }
}

void MainWindow::keyPressEvent(QKeyEvent *e)
{
    if (e->key() == Qt::Key_Escape) {
        close();
    }
    else {
        QWidget::keyPressEvent(e);
    }
}
