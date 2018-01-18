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

#ifndef OFWIDGET_H
#define OFWIDGET_H

#include <QWidget>

class OFRendererIF;
class OFWindow;

/**********************************************************
 * Matthew Ruschmann
 * OpenFrames ofqt Example, class OFWidget
 * Encapsulates an OFWindow with an OpenGL surface and its
 * associated widget container to override QWidget
 * interfaces such as sizeHint().
**********************************************************/
class OFWidget : public QWidget
{
public:
    /** Constructor, passed an renderer that is in charge of instantiating OpenFrames */
    OFWidget(OFRendererIF &renderer, QWidget *parent = 0x0);
    /** Destructor */
    virtual ~OFWidget();

    /** QWidget interface that provides a widget size recommendation */
    QSize sizeHint() const override;

    /** Sets the size hint that will be provided */
    void setSizeHint(int width, int height);

private:
    /** The recommended size returned by sizeHint() */
    QSize m_sizeHint;
    /** The renderer to associate with m_window */
    OFRendererIF &m_renderer;
    /** A window with an OpenGL surface that is contained inside this widget */
    OFWindow *m_window;
    /** A generic window container widget returned by QWidget::createWindowContainer(), which cannot have overridden functions */
    QWidget *m_container;
};

#endif
