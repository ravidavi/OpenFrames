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

#ifndef OSGQUICKIMAGE
#define OSGQUICKIMAGE

#include "OSGQuickAdapter.hpp"

#include <osg/Image>


class OSGQuickImage : public osg::Image
{
public:
  OSGQuickImage(const QString &qmlScene);

  OSGQuickAdapter* getQQuickAdapter() { return _adapter; }

  virtual bool requiresUpdateCall() const { return true; }

  virtual void update(osg::NodeVisitor* /*nv*/) { render(); }

  void clearWriteBuffer();

  void render();

  /// Overridden scaleImage used to catch cases where the image is
  /// fullscreen and the window is resized.
  virtual void scaleImage(int s, int t, int r, GLenum newDataType);

  virtual bool sendFocusHint(bool focus);

  virtual bool sendPointerEvent(int x, int y, int buttonMask);

  virtual bool sendKeyEvent(int key, bool keyDown);

  virtual void setFrameLastRendered(const osg::FrameStamp* frameStamp);

protected:
  QPointer<OSGQuickAdapter>  _adapter;
};

#endif
