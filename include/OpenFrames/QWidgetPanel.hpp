/***********************************
   Copyright 2019 Ravishankar Mathur

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

/** \file QWidgetPanel.hpp
 * Declaration of QWidgetPanel class.
 */

#ifndef _OF_QWIDGETPANEL_
#define _OF_QWIDGETPANEL_

// OpenFrames and OpenSceneGraph headers
#include <OpenFrames/Export.h>
#include <OpenFrames/ReferenceFrame.hpp>
#include <osg/Geode>
#include <osg/Geometry>
#include <osgViewer/ViewerEventHandlers>

// Qt forward declarations
#include <qglobal.h> // for QT_FORWARD_DECLARE_CLASS
QT_FORWARD_DECLARE_CLASS(QWidget);

// OpenFrames forward declarations
namespace OpenFrames
{
  class QWidgetImage;
}

namespace OpenFrames
{
  /**
   * \class QWidgetPanel
   *
   * \brief ReferenceFrame with Qt control panel on plane.
   *
   * A QWidgetPanel is a ReferenceFrame with a Qt control panel on a
   * plane. The size of the control panel is adjustable.
   */
  class OF_EXPORT QWidgetPanel : public ReferenceFrame
  {
    public:
      QWidgetPanel( const std::string &name );
      QWidgetPanel( const std::string &name, const osg::Vec3 &color );
      QWidgetPanel( const std::string &name, const osg::Vec4 &color );
      QWidgetPanel( const std::string &name , float r, float g, float b, float a = 1.0 );

      // Show/hide this frame's contents, e.g. everything a frame shows (excluding axes, labels, and children)
      // Inherited from ReferenceFrame
      virtual void showContents(bool showContents);
      virtual bool getContentsShown() const;

      /** Set the width and height of the panel, given wrt the ReferenceFrame origin */
      void setSize(const double &width, const double &height);
      void getSize(double &width, double &height);

      /** Set the QWidget containing controls for the control box */
      bool setWidget( QWidget *widget );

      /** Sets whether a widget will ignore keyboard and mouse events */
      void setIgnoreWidget( QWidget *widget, bool ignore );

      /** Get the image onto which the Qt widget is drawn. Useful when creating custom image handlers.
          Only valid if setWidget() has been called with a valid QWidget, returns NULL otherwise. */
      osg::Image* getImage() { return (osg::Image*)_image.get(); }

      /** Set the image handler that translates user actions into image actions */
      void setImageHandler(osgViewer::InteractiveImageHandler *handler);
      osgViewer::InteractiveImageHandler* getImageHandler() const;

      /** Inherited from ReferenceFrame.
        Set the color of the box.  If a texture is applied, the color is
        used to filter the texture. */
      virtual void setColor( const osg::Vec4 &color );
      using ReferenceFrame::setColor; // Unhide other setColor() functions

      /** Inherited from ReferenceFrame. */
      virtual const osg::BoundingSphere& getBound() const;

    protected:
      virtual ~QWidgetPanel();

      osg::ref_ptr<osg::Geode> _geode; // Node containing the box
      osg::ref_ptr<osg::Geometry> _panel; // A panel for rendering the controls
      osg::ref_ptr<osg::Geometry> _panelBack; // Controls mirrored on back of panel
      osg::ref_ptr<QWidgetImage> _image; // The image to which controls are drawn

    private:
      static const double DEFAULT_LENGTH;
      static const double DEFAULT_PIXELS_PER_UNIT; // Only when QWidget has an invalid preferred size

      std::vector<QWidget *> _ignoredWidgets;

      void _init();
      void _rescaleWidget();
  };

} // !namespace OpenFrames

#endif
