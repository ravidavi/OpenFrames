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

#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QMainWindow>

QT_FORWARD_DECLARE_CLASS(QComboBox)

class OFRendererIF;
class OFWidget;

/**********************************************************
 * Matthew Ruschmann
 * OpenFrames ofqt Example, class MainWindow
 * This class is the main window of the example. It
 * instantiates and contains all other widgets.
**********************************************************/
class MainWindow : public QMainWindow
{
    /** Enable Qt signals and slots for this object */
    Q_OBJECT

public:
    /** Constructor */
    MainWindow();
    /** Destructor */
    virtual ~MainWindow();

public slots:
    /** Slot for currentIndexChanged signal from upper QCombobox */
    void handleTopViewChanged(int index);
    /** Slot for currentIndexChanged signal from lower QCombobox */
    void handleBottomViewChanged(int index);

protected:
    /** Overrides the default key press handler, pass unrecognized keys */
    void keyPressEvent(QKeyEvent *event) override;

private:
    /** Creates and populates the upper QCombobox */
    QComboBox *createTopViewComboBox();
    /** Creates and populates the lower QCombobox */
    QComboBox *createBottomViewComboBox();

    /** A renderer object that builds the OpenFrames scene and handles callbacks */
    OFRendererIF *m_renderer;
    /** A widget that provides a surface for the renderer to draw on */
    OFWidget *m_containerWidget;
    /** Pointers to the comboboxes */
    QComboBox *m_topComboBox;
    QComboBox *m_bottomComboBox;
};

#endif
