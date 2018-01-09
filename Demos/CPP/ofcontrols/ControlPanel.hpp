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

#ifndef _OF_CONTROLPANEL_
#define _OF_CONTROLPANEL_

// OpenFrames and OpenSceneGraph headers
#include <OpenFrames/ReferenceFrame.hpp>
#include <osg/Geode>
#include <osg/Geometry>

// Qt forward declarations
#include <qglobal.h> // for QT_FORWARD_DECLARE_CLASS
QT_FORWARD_DECLARE_CLASS(QWidget);

// OpenFrames forward declarations
class QWidgetImage;

namespace OpenFrames
{

  /*******************************************************************
   * Matthew Ruschmann
   * OpenFrames API, class ControlPanel
   * A ControlPanel is a ReferenceFrame with a Qt control panel on a
   * plane. The size of the control panel is adjustable.
   ******************************************************************/
  class ControlPanel : public ReferenceFrame
  {
    public:
      ControlPanel( const std::string &name );
      ControlPanel( const std::string &name, const osg::Vec3 &color );
      ControlPanel( const std::string &name, const osg::Vec4 &color );
      ControlPanel( const std::string &name , float r, float g, float b, float a = 1.0 );

      /** Set the half lengths for sides of the box, given wrt the origin of
          the box's reference frame */
      void setHalfLengths( const double &xHalfLength, const double &yHalfLength, const double &zHalfLength );
      void getHalfLengths( double &xHalfLength, double &yHalfLength, double &zHalfLength ) const;

      /** Set the pixels per unit for scaling the controls widget */
      void setPixelsPerUnit( double pixelsPerUnit );
      double getPixelsPerUnit();

      /** Set the QWidget containing controls for the control box */
      bool setWidget( QWidget *widget );

      /** Inherited from ReferenceFrame.
        Set the color of the box.  If a texture is applied, the color is
        used to filter the texture. */
      virtual void setColor( const osg::Vec4 &color );
      using ReferenceFrame::setColor; // Unhide other setColor() functions

      /** Inherited from ReferenceFrame. */
      virtual const osg::BoundingSphere& getBound() const;

    protected:
      virtual ~ControlPanel();
      void buildPanelGeometry(const osg::Vec3 &halfLengths);

      osg::ref_ptr<osg::Geode> _geode; // Node containing the box
      osg::ref_ptr<osg::Geometry> _panel; // A panel for rendering the controls
      osg::ref_ptr<QWidgetImage> _image; // The image to which controls are drawn

    private:
      static const double DEFAULT_LENGTH;
      static const double DEFAULT_PIXELS_PER_UNIT;

      double _pixelsPerUnit;

      void _init();
      void _rescaleWidget();
  };

} // !namespace OpenFrames

#endif
