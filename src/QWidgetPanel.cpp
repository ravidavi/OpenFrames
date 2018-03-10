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

#include <OpenFrames/QWidgetPanel.hpp>
#include <OpenFrames/QtOSGAdapters.hpp>

#include <osg/Vec3d>
#include <osg/Quat>
#include <osg/Geode>
#include <osg/PolygonOffset>
#include <osg/Shape>
#include <osg/Texture2D>
#include <osgDB/ReadFile>
#include <osgUtil/CullVisitor>

#include <iostream>
#include <algorithm>
#include <cmath>

namespace OpenFrames
{

  /// Default half length for the hyperrectangle
  const double QWidgetPanel::DEFAULT_LENGTH = 1.0;
  /// Only used when the QWidget has an invalid preferred size
  const double QWidgetPanel::DEFAULT_PIXELS_PER_UNIT = 100.0;

  QWidgetPanel::QWidgetPanel(const std::string &name)
    : ReferenceFrame(name)
  {
    _init();
  }

  QWidgetPanel::QWidgetPanel(const std::string &name, const osg::Vec3 &color)
    : ReferenceFrame(name, color)
  {
    _init();
  }

  QWidgetPanel::QWidgetPanel(const std::string &name, const osg::Vec4 &color)
    : ReferenceFrame(name, color)
  {
    _init();
  }

  QWidgetPanel::QWidgetPanel(const std::string &name, float r, float g, float b, float a)
    : ReferenceFrame(name, r, g, b, a)
  {
    _init();
  }

  QWidgetPanel::~QWidgetPanel() { }

  /** Create the panel */
  void QWidgetPanel::_init()
  {
    // Create the panel as a textured quad
    _panel = osg::createTexturedQuadGeometry(
      osg::Vec3(), // Bottom-left corner (panel origin)
      osg::Vec3(DEFAULT_LENGTH, 0, 0), // Width vector
      osg::Vec3(0, DEFAULT_LENGTH, 0)); // Height vector
    _panel->setName("QWidgetPanel Front");
    _panel->setUseDisplayList(false);
    _panel->setUseVertexBufferObjects(true);

    // Create the back panel as a textured quad
    // Note that we slightly offset the back panel Z coordinate so that it is not
    // coplanar with the front panel (causes picking issues)
    _panelBack = osg::createTexturedQuadGeometry(
      osg::Vec3(DEFAULT_LENGTH, 0, -0.001),  // Origin of reversed panel
      osg::Vec3(-DEFAULT_LENGTH, 0, 0), // Width of reversed panel
      osg::Vec3(0, DEFAULT_LENGTH, 0)); // Height same as front panel
    _panelBack->setName("QWidgetPanel Back");
    _panelBack->setUseDisplayList(false);
    _panelBack->setUseVertexBufferObjects(true);
    _panelBack->setColorArray(_panel->getColorArray(), osg::Array::BIND_OVERALL); // Use color from front panel

    // Set rendering properties
    osg::StateSet* stateset = _panel->getOrCreateStateSet();
    _panelBack->setStateSet(stateset); // Share stateset between front & back panels
    stateset->setMode(GL_CULL_FACE, osg::StateAttribute::ON); // Don't draw panel backfaces
    stateset->setMode(GL_LIGHTING, osg::StateAttribute::OFF); // Panel not altered by lighting
    stateset->setMode(GL_BLEND, osg::StateAttribute::ON); // Enable transparency
    stateset->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);

    // Create the node that contains the QWidgetPanel
    _geode = new osg::Geode;
    _geode->setName(_name);
    _geode->addDrawable(_panel);
    _geode->addDrawable(_panelBack);

    // Add the box to the ReferenceFrame
    _xform->addChild(_geode.get());
  }

  void QWidgetPanel::showContents(bool showContents)
  {
    if (showContents) _geode->setNodeMask(0xffffffff);
    else _geode->setNodeMask(0x0);
  }

  bool QWidgetPanel::getContentsShown() const
  {
    return (_geode->getNodeMask() != 0x0);
  }

  void QWidgetPanel::setSize(const double &width, const double &height)
  {
    // Resize front panel (its normals and colors don't change)
    // Quad vertices are defined as CCW starting from top-left, with origin at bottom-left corner
    // See osg::createTexturedQuadGeometry() for details
    osg::Vec3Array* coords = dynamic_cast<osg::Vec3Array*>(_panel->getVertexArray());
    osg::Vec3 corner = (*coords)[1];
    osg::Vec3 widthVec(width, 0, 0);
    osg::Vec3 heightVec(0, height, 0);
    (*coords)[0] = corner + heightVec; // Top-left vertex
    (*coords)[2] = corner + widthVec; // Bottom-right vertex
    (*coords)[3] = corner + widthVec + heightVec; // Top-right vertex

    // Resize back panel
    osg::Vec3Array* coordsBack = dynamic_cast<osg::Vec3Array*>(_panelBack->getVertexArray());
    float backZ = (*coordsBack)[0].z(); // Save Z-offset for back panel
    (*coordsBack)[0] = (*coords)[3]; // Back top-left = Front top-right
    (*coordsBack)[1] = (*coords)[2]; // Back bottom-left = Front bottom-right
    (*coordsBack)[2] = (*coords)[1]; // Back bottom-right = Front bottom-left
    (*coordsBack)[3] = (*coords)[0]; // Back top-right = Front top-left
    (*coordsBack)[0].z() = backZ; // Restore Z-offset for back panel
    (*coordsBack)[1].z() = backZ; // Restore Z-offset for back panel
    (*coordsBack)[2].z() = backZ; // Restore Z-offset for back panel
    (*coordsBack)[3].z() = backZ; // Restore Z-offset for back panel

    // Indicate that panel data has changed
    coords->dirty();
    coordsBack->dirty();
    _panel->dirtyBound();
    _panelBack->dirtyBound();

    // Move axes to compensate for size change
    double averageSize = (width + height) / 2.0;
    moveXAxis(osg::Vec3(width, 0, 0), 0.5*averageSize);
    moveYAxis(osg::Vec3(0, height, 0), 0.5*averageSize);
    moveZAxis(osg::Vec3(0, 0, 0.5*averageSize), 0.5*averageSize);

    // Resize the underlying QWidget
    _rescaleWidget();
  }

  void QWidgetPanel::getSize(double &width, double &height)
  {
    // Quad vertices are defined as CCW starting from top-left, with origin at bottom-left
    // See osg::createTexturedQuadGeometry() for details
    osg::Vec3Array* coords = dynamic_cast<osg::Vec3Array*>(_panel->getVertexArray());
    width = (*coords)[2].x(); // Bottom-right vertex
    height = (*coords)[0].y(); // Top-left vertex
  }


  bool QWidgetPanel::setWidget(QWidget *widget)
  {
    osg::StateSet* stateset = _panel->getStateSet();
    if (stateset == nullptr) return false;

    if(widget == nullptr) // Remove existing texture
    {
      _image.release(); // Release the old controls

      stateset->removeTextureAttribute(0, osg::StateAttribute::TEXTURE);
      stateset->removeTextureAttribute(0, osg::StateAttribute::TEXENV);

      // Revert color from white to reference frame color
      osg::Vec4Array* colors = dynamic_cast<osg::Vec4Array*>(_panel->getColorArray());
      (*colors)[0] = getColor();
      colors->dirty();

      return false;
    }
    else
    {
      // Check if there is already a texture being used.
      osg::Texture2D* texture = dynamic_cast<osg::Texture2D*>(stateset->getTextureAttribute(0, osg::StateAttribute::TEXTURE));

      // Disable context menus when embedding. They can popup outside the scene graph.
      if (widget->contextMenuPolicy() == Qt::DefaultContextMenu)
        widget->setContextMenuPolicy(Qt::NoContextMenu);
      QList<QWidget*> children = widget->findChildren<QWidget*>();
      for(QList<QWidget*>::iterator child = children.begin(); child < children.end(); child++)
      {
        if ((*child)->contextMenuPolicy() == Qt::DefaultContextMenu)
        (*child)->setContextMenuPolicy(Qt::NoContextMenu);
      }

      // Wrap the QWidget into an osg::Image
      _image = new QWidgetImage(widget);
#if (QT_VERSION >= QT_VERSION_CHECK(4, 5, 0))
      _image->getQWidget()->setAttribute(Qt::WA_TranslucentBackground);
#endif
      const osg::Vec4 &color = getColor();
      _image->getQGraphicsViewAdapter()->setBackgroundColor(QColor(255.0f*color[0], 255.0f*color[1], 255.0f*color[2], 255.0f*color[3]));
      _rescaleWidget();
      _image->getQGraphicsViewAdapter()->setIgnoredWidgets(_ignoredWidgets);

      // Create texture using image, and make sure it wraps around the
      // box without a seam at the edges.
      texture = new osg::Texture2D;
      texture->setResizeNonPowerOfTwoHint(false);
      texture->setImage(_image);
      texture->setFilter(osg::Texture::MIN_FILTER, osg::Texture::LINEAR);
      texture->setWrap(osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE);
      texture->setWrap(osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE);

      // Set the texture to the box
      stateset->setTextureAttributeAndModes(0, texture, osg::StateAttribute::ON);

      // Don't use the box's color when mapping the texture.
      osg::TexEnv* texenv = new osg::TexEnv;
      texenv->setMode(osg::TexEnv::MODULATE);
      stateset->setTextureAttribute(0, texenv);

      // Set default image handler to convert user events to Qt widget selections
      if (getImageHandler() == NULL) setImageHandler(new osgViewer::InteractiveImageHandler(_image.get()));

      // Set color to white for modulation of texture
      osg::Vec4Array* colors = dynamic_cast<osg::Vec4Array*>(_panel->getColorArray());
      (*colors)[0] = { 1.0, 1.0, 1.0, 1.0 };
      colors->dirty();

      return true;
    }
  }

  void QWidgetPanel::setIgnoreWidget( QWidget *widget, bool ignore )
  {
    auto widgetIteratorInList = std::find(_ignoredWidgets.begin(), _ignoredWidgets.end(), widget);
    bool notInList = ( widgetIteratorInList ==  _ignoredWidgets.end() );
    if (ignore)
    {
      if (notInList)
      {
        _ignoredWidgets.push_back(widget);
        if (_image.valid())
          _image->getQGraphicsViewAdapter()->setIgnoredWidgets(_ignoredWidgets);
      }
    }
    else
    {
      if (!notInList)
      {
        _ignoredWidgets.erase(widgetIteratorInList);
        if (_image.valid())
          _image->getQGraphicsViewAdapter()->setIgnoredWidgets(_ignoredWidgets);
      }
    }
  }

  void QWidgetPanel::setImageHandler(osgViewer::InteractiveImageHandler *handler)
  {
    // Add event handler to geode so that it can handle front and back panel events
    _geode->setEventCallback(handler);

    // NOTE: If the cull handler is added to the geode, then no images are rendered
    // at all for Qt widgets. This may be because the osg/Qt adapter thinks it has
    // already rendered the image. This may require future investigation. For now,
    // we only add the cull handler to the front panel which solves the problem.
    _panel->setCullCallback(handler);
  }

  osgViewer::InteractiveImageHandler* QWidgetPanel::getImageHandler() const
  {
    return dynamic_cast<osgViewer::InteractiveImageHandler*>(_panel->getCullCallback());
  }

  void QWidgetPanel::setColor( const osg::Vec4 &color )
  {
    ReferenceFrame::setColor(color);
    if (_image.valid())
    {
      // Set the QWidget background color, keep geometry white
      _image->getQGraphicsViewAdapter()->setBackgroundColor(QColor(255.0f*color[0], 255.0f*color[1], 255.0f*color[2], 255.0f*color[3]));
    }
    else
    {
      // Set the geometry color
      osg::Vec4Array* colors = dynamic_cast<osg::Vec4Array*>(_panel->getColorArray());
      (*colors)[0] = color;
      colors->dirty();
    }
  }

  const osg::BoundingSphere& QWidgetPanel::getBound() const
  {
    osg::BoundingSphere bs = _geode->getBound();

    // Keep bound center but expand to include axes/labels
    ReferenceFrame::getBound();
    bs.expandRadiusBy(_bound);
    _bound = bs;

    return _bound;
  }

  void QWidgetPanel::_rescaleWidget()
  {
    if (_image.valid())
    {
      // Scale the QWidget to the X-Y plane size
      double panelWidth, panelHeight;
      double imageWidth, imageHeight;

      getSize(panelWidth, panelHeight); // Use x,y as current panel width,height

      QSize preferredSize = _image->getQGraphicsViewAdapter()->getQGraphicsView()->sizeHint();
      if (preferredSize.isValid())
      {
        double preferredWidth = static_cast<double>(preferredSize.width());
        double preferredHeight = static_cast<double>(preferredSize.height());
        if ((panelWidth / panelHeight) > (preferredWidth / preferredHeight))
        {
          // Panel is taller than the preferred size
          imageHeight = preferredHeight;
          // Qt may get upset if we accidently round down below the minimum size
          imageWidth = ceil(panelWidth * preferredHeight / panelHeight);
          std::cout << " y = " << imageHeight << " x = " << imageWidth << std::endl;
        }
        else
        {
          // Panel is wider than the preferred size
          imageWidth = preferredWidth;
          // Qt may get upset if we accidently round down below the minimum size
          imageHeight = ceil(panelHeight * preferredWidth / panelWidth);
          std::cout << "x = " << imageWidth << " y = " << imageHeight << std::endl;
        }
      }
      else
      {
        imageWidth = DEFAULT_PIXELS_PER_UNIT * panelWidth;
        imageHeight = DEFAULT_PIXELS_PER_UNIT * panelHeight;
      }

      _image->scaleImage(imageWidth, imageHeight, 0, 0U);
    }
  }

} // !namespace OpenFrames

