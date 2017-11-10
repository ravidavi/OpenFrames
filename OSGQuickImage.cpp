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

#include "OSGQuickImage.hpp"

#include <QQuickWindow>


OSGQuickImage::OSGQuickImage(const QString &qmlScene)
{
  // make sure we have a valid QApplication before we start creating widgets.
  OSGQuickAdapter::getOrCreateQApplication();

  _adapter = new OSGQuickAdapter(this, qmlScene);
}


bool OSGQuickImage::sendFocusHint(bool focus)
{
  // TODO renable this if its necessary for quick
  //QFocusEvent event(focus ? QEvent::FocusIn : QEvent::FocusOut, Qt::OtherFocusReason);
  //QCoreApplication::sendEvent(_adapter->getQuickWindow(), &event);
  return true;
}


void OSGQuickImage::clearWriteBuffer()
{
  _adapter->clearWriteBuffer();
}


void OSGQuickImage::render()
{
  if (_adapter->requiresRendering())
  {
    _adapter->render();
  }
}


void OSGQuickImage::scaleImage(int s, int t, int /*r*/, GLenum /*newDataType*/)
{
  _adapter->resize(s, t);
}


void OSGQuickImage::setFrameLastRendered(const osg::FrameStamp* frameStamp)
{
  _adapter->setFrameLastRendered(frameStamp);
}


bool OSGQuickImage::sendPointerEvent(int x, int y, int buttonMask)
{
  return _adapter->sendPointerEvent(x, y, buttonMask);
}


bool OSGQuickImage::sendKeyEvent(int key, bool keyDown)
{
  return _adapter->sendKeyEvent(key, keyDown);
}
