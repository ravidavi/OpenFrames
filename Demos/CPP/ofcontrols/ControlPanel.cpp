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

#include "ControlPanel.hpp"
#include "QWidgetImage.hpp"

#include <osg/Vec3d>
#include <osg/Quat>
#include <osg/Geode>
#include <osg/PolygonOffset>
#include <osg/Shape>
#include <osg/Texture2D>
#include <osgDB/ReadFile>
#include <osgUtil/CullVisitor>
#include <osgViewer/ViewerEventHandlers>

#include <iostream>

namespace OpenFrames
{

  const double ControlPanel::DEFAULT_LENGTH = 1.0;

  ControlPanel::ControlPanel(const std::string &name)
    : ReferenceFrame(name)
  {
    _init();
  }

  ControlPanel::ControlPanel(const std::string &name, const osg::Vec3 &color)
    : ReferenceFrame(name, color)
  {
    _init();
  }

  ControlPanel::ControlPanel(const std::string &name, const osg::Vec4 &color)
    : ReferenceFrame(name, color)
  {
    _init();
  }

  ControlPanel::ControlPanel(const std::string &name, float r, float g, float b, float a)
    : ReferenceFrame(name, r, g, b, a)
  {
    _init();
  }

  ControlPanel::~ControlPanel() { }

  /** Create the box with default radius = 1,
    and color = the color of the box's reference frame. */
  void ControlPanel::_init()
  {
    // Set the shape to be drawn
    _panel = new osg::Geometry();
    osg::Box* box = new osg::Box;
    box->setHalfLengths(osg::Vec3(1.0, 1.0, 1.0));
    _panel->setShape(box);
    _panel->setName("ControlPanelDrawable");
    _panel->setUseDisplayList(false);
    _panel->setUseVertexBufferObjects(true);
    buildPanelGeometry(osg::Vec3(1.0, 1.0, 1.0));

    // Create the node that contains the ControlPanel
    _geode = new osg::Geode;
    _geode->setName(_name);
    _geode->addDrawable(_panel);

    // Add the box to the ReferenceFrame
    _xform->addChild(_geode.get());

    // Hide name and axes by default
    showNameLabel(false);
    showAxes(0U);
    showAxesLabels(0U);
  }

  void ControlPanel::setHalfLengths(const double &xHalfLength, const double &yHalfLength, const double &zHalfLength)
  {
    // Set shape half lengths
    osg::Box *box = static_cast<osg::Box*>(_panel->getShape());
    box->setHalfLengths(osg::Vec3(xHalfLength, yHalfLength, zHalfLength));

    _panel->dirtyBound();
    buildPanelGeometry(osg::Vec3(xHalfLength, yHalfLength, zHalfLength));

    // Move axes to compensate for size change
    double averageHalfLength = (xHalfLength + yHalfLength + zHalfLength) / 3.0;
    moveXAxis(osg::Vec3(xHalfLength, 0, 0), 0.5*averageHalfLength);
    moveYAxis(osg::Vec3(0, yHalfLength, 0), 0.5*averageHalfLength);
    moveZAxis(osg::Vec3(0, 0, zHalfLength), 0.5*averageHalfLength);
  }

  void ControlPanel::getHalfLengths(double &xHalfLength, double &yHalfLength, double &zHalfLength) const
  {
    osg::Box *box = static_cast<osg::Box*>(_panel->getShape());
    const osg::Vec3 halfLenghts = box->getHalfLengths();
    xHalfLength = halfLenghts[0];
    yHalfLength = halfLenghts[1];
    zHalfLength = halfLenghts[2];
  }

  bool ControlPanel::setWidgetControls(QWidget *widget)
  {
    if(widget == nullptr) // Remove existing texture
    {
      _image.release(); // Release the old controls

      osg::StateSet* stateset = _panel->getStateSet();
      if(stateset)
      {
        stateset->removeTextureAttribute(0, osg::StateAttribute::TEXTURE);
        stateset->removeTextureAttribute(0, osg::StateAttribute::TEXENV);
      }

      // Revert color to setting
      osg::Vec4Array* colours = new osg::Vec4Array(1);
      (*colours)[0] = getColor();
      _panel->setColorArray(colours, osg::Array::BIND_OVERALL);

      return false;
    }
    else
    {
      // Check if there is already a texture being used.
      osg::StateSet* stateset = _panel->getOrCreateStateSet();
      osg::Texture2D* texture = dynamic_cast<osg::Texture2D*>(stateset->getTextureAttribute(0, osg::StateAttribute::TEXTURE));

      // Wrap the QWidget into an osg::Image
      _image = new QWidgetImage(widget);
#if (QT_VERSION >= QT_VERSION_CHECK(4, 5, 0))
      _image->getQWidget()->setAttribute(Qt::WA_TranslucentBackground);
#endif
      const osg::Vec4 &color = getColor();
      _image->getQGraphicsViewAdapter()->setBackgroundColor(QColor(255.0f*color[0], 255.0f*color[1], 255.0f*color[2], 255.0f*color[3]));
      _image->getQGraphicsViewAdapter()->setBackgroundWidget(widget);

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
      stateset->setAttribute(new osg::Program); // TODO What is this for??
      stateset->setMode(GL_LIGHTING, osg::StateAttribute::ON);
      stateset->setMode(GL_BLEND, osg::StateAttribute::ON);
      stateset->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);

      // Image handler
      osgViewer::InteractiveImageHandler* handler = new osgViewer::InteractiveImageHandler(_image.get());
      _panel->setEventCallback(handler);
      _panel->setCullCallback(handler);

      // Set color to white for modulation of texture
      osg::Vec4Array* colours = new osg::Vec4Array(1);
      (*colours)[0] = { 1.0, 1.0, 1.0, 1.0 };
      _panel->setColorArray(colours, osg::Array::BIND_OVERALL);

      return true;
    }
  }

  void ControlPanel::setColor( const osg::Vec4 &color )
  {
    ReferenceFrame::setColor(color);
    if (_image.valid())
    {
      _image->getQGraphicsViewAdapter()->setBackgroundColor(QColor(255.0f*color[0], 255.0f*color[1], 255.0f*color[2], 255.0f*color[3]));
    }
    else
    {
      osg::Vec4Array* colours = new osg::Vec4Array(1);
      (*colours)[0] = color;
      _panel->setColorArray(colours, osg::Array::BIND_OVERALL);
    }
  }

  const osg::BoundingSphere& ControlPanel::getBound() const
  {
    osg::BoundingSphere bs = _geode->getBound();

    // Keep bound center but expand to include axes/labels
    ReferenceFrame::getBound();
    bs.expandRadiusBy(_bound);
    _bound = bs;

    return _bound;
  }

  void ControlPanel::buildPanelGeometry(const osg::Vec3 &halfLengths)
  {
    float l = 0.0;
    float b = 0.0;
    float r = 1.0;
    float t = 1.0;

    _panel->setVertexArray(nullptr);
    _panel->setColorArray(nullptr);
    _panel->setNormalArray(nullptr);
    _panel->getTexCoordArrayList().clear();
    _panel->getPrimitiveSetList().clear();

    // Set panel half lengths and rebuild
    osg::Vec3 corner(-halfLengths[0], -halfLengths[1], -halfLengths[2]);
    osg::Vec3 heightVec(halfLengths[0] * 2.0f, 0.0f, 0.0f);
    osg::Vec3 widthVec(0.0f, halfLengths[1] * 2.0f, 0.0f);
    osg::Vec3 lengthVec(0.0f, 0.0f, halfLengths[2] * 2.0f);
;
    osg::Vec3Array* coords = new osg::Vec3Array(24);
    (*coords)[0] = corner + heightVec;
    (*coords)[1] = corner;
    (*coords)[2] = corner + widthVec;
    (*coords)[3] = corner + widthVec + heightVec;
    (*coords)[4] = corner + widthVec + heightVec + lengthVec;
    (*coords)[5] = corner + widthVec + lengthVec;
    (*coords)[6] = corner + lengthVec;
    (*coords)[7] = corner + heightVec + lengthVec;
    (*coords)[8] = (*coords)[7];
    (*coords)[9] = (*coords)[6];
    (*coords)[10] = (*coords)[1];
    (*coords)[11] = (*coords)[0];
    (*coords)[12] = (*coords)[3];
    (*coords)[13] = (*coords)[2];
    (*coords)[14] = (*coords)[5];
    (*coords)[15] = (*coords)[4];
    (*coords)[16] = (*coords)[1];
    (*coords)[17] = (*coords)[6];
    (*coords)[18] = (*coords)[5];
    (*coords)[19] = (*coords)[2];
    (*coords)[20] = (*coords)[7];
    (*coords)[21] = (*coords)[0];
    (*coords)[22] = (*coords)[3];
    (*coords)[23] = (*coords)[4];
    _panel->setVertexArray(coords);

    setColor(getColor());

    osg::Vec2Array* tcoords = new osg::Vec2Array(24);
    (*tcoords)[0].set(l, t);
    (*tcoords)[1].set(l, b);
    (*tcoords)[2].set(r, b);
    (*tcoords)[3].set(r, t);
    (*tcoords)[4].set(l, t);
    (*tcoords)[5].set(l, b);
    (*tcoords)[6].set(r, b);
    (*tcoords)[7].set(r, t);
    (*tcoords)[8].set(l, t);
    (*tcoords)[9].set(l, b);
    (*tcoords)[10].set(l, b);
    (*tcoords)[11].set(l, t);
    (*tcoords)[12].set(r, t);
    (*tcoords)[13].set(r, b);
    (*tcoords)[14].set(r, b);
    (*tcoords)[15].set(r, t);
    (*tcoords)[16].set(l, b);
    (*tcoords)[17].set(r, b);
    (*tcoords)[18].set(l, b);
    (*tcoords)[19].set(r, b);
    (*tcoords)[20].set(r, t);
    (*tcoords)[21].set(l, t);
    (*tcoords)[22].set(r, t);
    (*tcoords)[23].set(l, t);
    _panel->setTexCoordArray(0, tcoords);

    osg::Vec3Array* normals = new osg::Vec3Array(6);
    (*normals)[0].set(0.0f, 0.0f, -1.0f);
    (*normals)[1].set(0.0f, 0.0f, 1.0f);
    (*normals)[2].set(0.0f, -1.0f, 0.0f);
    (*normals)[3].set(0.0f, 1.0f, 0.0f);
    (*normals)[4].set(-1.0f, 0.0f, 0.0f);
    (*normals)[5].set(1.0f, 0.0f, 0.0f);
    _panel->setNormalArray(normals, osg::Array::BIND_PER_PRIMITIVE_SET);

#if defined(OSG_GLES1_AVAILABLE) || defined(OSG_GLES2_AVAILABLE)
    osg::DrawElementsUByte* elems = new osg::DrawElementsUByte(osg::PrimitiveSet::TRIANGLES);
    elems->push_back(0);
    elems->push_back(1);
    elems->push_back(2);
    elems->push_back(2);
    elems->push_back(3);
    elems->push_back(0);
    _panel->addPrimitiveSet(elems);

    elems = new osg::DrawElementsUByte(osg::PrimitiveSet::TRIANGLES);
    elems->push_back(4);
    elems->push_back(5);
    elems->push_back(6);
    elems->push_back(6);
    elems->push_back(7);
    elems->push_back(4);
    _panel->addPrimitiveSet(elems);

    elems = new osg::DrawElementsUByte(osg::PrimitiveSet::TRIANGLES);
    elems->push_back(8);
    elems->push_back(9);
    elems->push_back(10);
    elems->push_back(10);
    elems->push_back(11);
    elems->push_back(8);
    _panel->addPrimitiveSet(elems);

    elems = new osg::DrawElementsUByte(osg::PrimitiveSet::TRIANGLES);
    elems->push_back(12);
    elems->push_back(13);
    elems->push_back(14);
    elems->push_back(14);
    elems->push_back(15);
    elems->push_back(12);
    _panel->addPrimitiveSet(elems);

    elems = new osg::DrawElementsUByte(osg::PrimitiveSet::TRIANGLES);
    elems->push_back(16);
    elems->push_back(17);
    elems->push_back(18);
    elems->push_back(18);
    elems->push_back(19);
    elems->push_back(16);
    _panel->addPrimitiveSet(elems);

    elems = new osg::DrawElementsUByte(osg::PrimitiveSet::TRIANGLES);
    elems->push_back(20);
    elems->push_back(21);
    elems->push_back(22);
    elems->push_back(22);
    elems->push_back(23);
    elems->push_back(20);
    _panel->addPrimitiveSet(elems);
#else
    _panel->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS, 0, 4));
    _panel->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS, 4, 4));
    _panel->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS, 8, 4));
    _panel->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS, 12, 4));
    _panel->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS, 16, 4));
    _panel->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS, 20, 4));
#endif
  }

} // !namespace OpenFrames

