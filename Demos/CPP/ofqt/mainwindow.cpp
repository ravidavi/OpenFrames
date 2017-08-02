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

    setWindowTitle(tr("Hello OpenFrames Qt"));
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
    qDebug() << "Selected index = " << index;
}

void MainWindow::handleBottomViewChanged(int index) {
    qDebug() << "Selected index = " << index;
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
