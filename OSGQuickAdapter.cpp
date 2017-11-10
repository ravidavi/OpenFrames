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

#include "OSGQuickAdapter.hpp"
#include "OSGQuickImage.hpp"

// TODO Remove these headers
#include <QtOpenGL/QGLWidget>
#include <QGraphicsItem>
#include <QGraphicsProxyWidget>

#include <osg/Version>
#include <osgGA/GUIEventAdapter>

#include <osg/NodeVisitor>
#include <osg/io_utils>

#include <QOpenGLContext>
#include <QOpenGLFunctions>
#include <QOpenGLFramebufferObject>
#include <QOffscreenSurface>
#include <QQuickRenderControl>
#include <QQuickWindow>
#include <QQmlEngine>
#include <QQmlComponent>
#include <QQuickItem>

#define MYQKEYEVENT 2000
#define MYQPOINTEREVENT 2001


QCoreApplication* OSGQuickAdapter::getOrCreateQApplication()
{
  if (QApplication::instance() == 0)
  {
    static char** argv = 0;
    static int argc = 0;
    static QApplication app(argc, argv);
  }
  return QApplication::instance();
}


class MyQKeyEvent : public QEvent
{
public:
  MyQKeyEvent(int key, bool down) :
    QEvent(QEvent::Type(MYQKEYEVENT)),
    _key(key), _down(down) {}

  int         _key;
  bool        _down;
};


struct MyQPointerEvent : public QEvent
{
  MyQPointerEvent(int x, int y, unsigned int buttonMask) :
    QEvent(QEvent::Type(MYQPOINTEREVENT)),
    _x(x), _y(y), _buttonMask(buttonMask) {}

  int _x, _y;
  unsigned int _buttonMask;
};


const QImage::Format s_imageFormat = QImage::Format_ARGB32_Premultiplied;


class RenderControl : public QQuickRenderControl
{
public:
  RenderControl(QObject *p, QWindow *w) : QQuickRenderControl(p), m_window(w) { }
  QWindow *renderWindow(QPoint *offset) Q_DECL_OVERRIDE;

private:
  QWindow *m_window;
};

QWindow *RenderControl::renderWindow(QPoint *offset)
{
  if (offset)
    *offset = QPoint(0, 0);
  return m_window;
}


OSGQuickAdapter::OSGQuickAdapter(osg::Image* image, const QString &qmlScene) :
  _image(image),
  _previousButtonMask(0),
  _previousMouseX(-1),
  _previousMouseY(-1),
  _previousQtMouseX(-1),
  _previousQtMouseY(-1),
  _previousSentEvent(false),
  _requiresRendering(false),
  _qtKeyModifiers(Qt::NoModifier),
  _backgroundColor(255, 255, 255),
  m_quickInitialized(false),
  m_quickReady(false),
  _qmlResource(qmlScene)
{
  // make sure we have a valid QApplication before we start creating widgets.
  getOrCreateQApplication();
  setUpKeyMap();

  QSurfaceFormat format;
  // Qt Quick may need a depth and stencil buffer. Always make sure these are available.
  format.setDepthBufferSize(16);
  format.setStencilBufferSize(8);

  m_context = new QOpenGLContext;
  m_context->setFormat(format);
  m_context->create();

  m_offscreenSurface = new QOffscreenSurface;
  // Pass m_context->format(), not format. Format does not specify and color buffer
  // sizes, while the context, that has just been created, reports a format that has
  // these values filled in. Pass this to the offscreen surface to make sure it will be
  // compatible with the context's configuration.
  m_offscreenSurface->setFormat(m_context->format());
  m_offscreenSurface->create();

  // TODO do we really need a QWindow? If so track its pointer and delete it.
  // We cannot implement QQuickRenderControl::renderWindow because we are not rendering
  // to a window. See https://doc.qt.io/qt-5/qquickrendercontrol.html#renderWindow
  m_renderControl = new RenderControl(this, new QWindow);

  // Create a QQuickWindow that is associated with out render control. Note that this
  // window never gets created or shown, meaning that it will never get an underlying
  // native (platform) window.
  m_quickWindow = new QQuickWindow(m_renderControl);

  // Create a QML engine.
  m_qmlEngine = new QQmlEngine;
  if (!m_qmlEngine->incubationController())
  {
    m_qmlEngine->setIncubationController(m_quickWindow->incubationController());
  }

  // TODO: get the size from something useful! (MCR 11/10/2017)
  _size = m_quickWindow->size();
  _size = QSize(1024, 1024);

  _qimages[0] = QImage(_size, s_imageFormat);
  _qimages[1] = QImage(_size, s_imageFormat);
  _qimages[2] = QImage(_size, s_imageFormat);

  _currentRead = 0;
  _currentWrite = 1;
  _previousWrite = 2;
  _previousFrameNumber = osg::UNINITIALIZED_FRAME_NUMBER;
  _newImageAvailable = false;

  // Now hook up the signals. For simplicy we don't differentiate between
  // renderRequested (only render is needed, no sync) and sceneChanged (polish and sync
  // is needed too).
  connect(m_quickWindow, &QQuickWindow::sceneGraphInitialized, this, &OSGQuickAdapter::createFbo);
  connect(m_quickWindow, &QQuickWindow::sceneGraphInvalidated, this, &OSGQuickAdapter::destroyFbo);
  connect(m_renderControl, &QQuickRenderControl::renderRequested, this, &OSGQuickAdapter::requestUpdate);
  connect(m_renderControl, &QQuickRenderControl::sceneChanged, this, &OSGQuickAdapter::requestUpdate);

  // Just recreating the FBO on resize is not sufficient, when moving between screens
  // with different devicePixelRatio the QWindow size may remain the same but the FBO
  // dimension is to change regardless.
  //TODO MCR 11/20/2017
  //connect(this, &QWindow::screenChanged, this, &WindowSingleThreaded::handleScreenChange);

  assignImage(0);

  connect(this, &OSGQuickAdapter::constructed, this, &OSGQuickAdapter::startQuick);
  emit constructed();
}


OSGQuickAdapter::~OSGQuickAdapter()
{
  // Make sure the context is current while doing cleanup. Note that we use the
  // offscreen surface here because passing 'this' at this point is not safe: the
  // underlying platform window may already be destroyed. To avoid all the trouble, use
  // another surface that is valid for sure.
  m_context->makeCurrent(m_offscreenSurface);

  m_context->doneCurrent();
}


void OSGQuickAdapter::createFbo()
{
  // The scene graph has been initialized. It is now time to create an FBO and associate
  // it with the QQuickWindow.
  m_fbo = new QOpenGLFramebufferObject(_size, QOpenGLFramebufferObject::CombinedDepthStencil);
  m_quickWindow->setRenderTarget(m_fbo);
}


void OSGQuickAdapter::destroyFbo()
{
  delete m_fbo;
  m_fbo = nullptr;
}


void OSGQuickAdapter::requestUpdate()
{
  // OSG_NOTICE<<"OSGQuickAdapter::requestUpdate"<<std::endl;
  _requiresRendering = true;
}


void OSGQuickAdapter::customEvent(QEvent * event)
{
  if (event->type() == MYQKEYEVENT)
  {
    MyQKeyEvent* keyEvent = (MyQKeyEvent*)event;
    handleKeyEvent(keyEvent->_key, keyEvent->_down);
  }
  else if (event->type() == MYQPOINTEREVENT)
  {
    MyQPointerEvent* pointerEvent = (MyQPointerEvent*)event;
    handlePointerEvent(pointerEvent->_x, pointerEvent->_y, pointerEvent->_buttonMask);
  }
}


void OSGQuickAdapter::setUpKeyMap()
{
  _keyMap[osgGA::GUIEventAdapter::KEY_BackSpace] = Qt::Key_Backspace;
  _keyMap[osgGA::GUIEventAdapter::KEY_Tab] = Qt::Key_Tab;
  _keyMap[osgGA::GUIEventAdapter::KEY_Linefeed] = Qt::Key_Return; // No LineFeed in Qt!
  _keyMap[osgGA::GUIEventAdapter::KEY_Clear] = Qt::Key_Clear;
  _keyMap[osgGA::GUIEventAdapter::KEY_Return] = Qt::Key_Return;
  _keyMap[osgGA::GUIEventAdapter::KEY_Pause] = Qt::Key_Pause;
  _keyMap[osgGA::GUIEventAdapter::KEY_Scroll_Lock] = Qt::Key_ScrollLock;
  _keyMap[osgGA::GUIEventAdapter::KEY_Sys_Req] = Qt::Key_SysReq;
  _keyMap[osgGA::GUIEventAdapter::KEY_Escape] = Qt::Key_Escape;
  _keyMap[osgGA::GUIEventAdapter::KEY_Delete] = Qt::Key_Delete;

  _keyMap[osgGA::GUIEventAdapter::KEY_Home] = Qt::Key_Home;
  _keyMap[osgGA::GUIEventAdapter::KEY_Left] = Qt::Key_Left;
  _keyMap[osgGA::GUIEventAdapter::KEY_Up] = Qt::Key_Up;
  _keyMap[osgGA::GUIEventAdapter::KEY_Right] = Qt::Key_Right;
  _keyMap[osgGA::GUIEventAdapter::KEY_Down] = Qt::Key_Down;
  _keyMap[osgGA::GUIEventAdapter::KEY_Prior] = Qt::Key_Left; // no Prior in Qt
  _keyMap[osgGA::GUIEventAdapter::KEY_Page_Up] = Qt::Key_PageUp;
  _keyMap[osgGA::GUIEventAdapter::KEY_Next] = Qt::Key_Right; // No Next in Qt
  _keyMap[osgGA::GUIEventAdapter::KEY_Page_Down] = Qt::Key_PageDown;
  _keyMap[osgGA::GUIEventAdapter::KEY_End] = Qt::Key_End;
  _keyMap[osgGA::GUIEventAdapter::KEY_Begin] = Qt::Key_Home; // No Begin in Qt

  _keyMap[osgGA::GUIEventAdapter::KEY_Select] = Qt::Key_Select;
  _keyMap[osgGA::GUIEventAdapter::KEY_Print] = Qt::Key_Print;
  _keyMap[osgGA::GUIEventAdapter::KEY_Execute] = Qt::Key_Execute;
  _keyMap[osgGA::GUIEventAdapter::KEY_Insert] = Qt::Key_Insert;
  //_keyMap[osgGA::GUIEventAdapter::KEY_Undo] = Qt::Key_; // no Undo
  //_keyMap[osgGA::GUIEventAdapter::KEY_Redo] = Qt::Key_; // no Redo
  _keyMap[osgGA::GUIEventAdapter::KEY_Menu] = Qt::Key_Menu;
  _keyMap[osgGA::GUIEventAdapter::KEY_Find] = Qt::Key_Search; // no Qt Find
  _keyMap[osgGA::GUIEventAdapter::KEY_Cancel] = Qt::Key_Cancel;
  _keyMap[osgGA::GUIEventAdapter::KEY_Help] = Qt::Key_Help;
  _keyMap[osgGA::GUIEventAdapter::KEY_Break] = Qt::Key_Escape; // no break
  _keyMap[osgGA::GUIEventAdapter::KEY_Mode_switch] = Qt::Key_Mode_switch;
  _keyMap[osgGA::GUIEventAdapter::KEY_Script_switch] = Qt::Key_Mode_switch; // no Script switch
  _keyMap[osgGA::GUIEventAdapter::KEY_Num_Lock] = Qt::Key_NumLock;

  _keyMap[osgGA::GUIEventAdapter::KEY_Shift_L] = Qt::Key_Shift;
  _keyMap[osgGA::GUIEventAdapter::KEY_Shift_R] = Qt::Key_Shift;
  _keyMap[osgGA::GUIEventAdapter::KEY_Control_L] = Qt::Key_Control;
  _keyMap[osgGA::GUIEventAdapter::KEY_Control_R] = Qt::Key_Control;
  _keyMap[osgGA::GUIEventAdapter::KEY_Caps_Lock] = Qt::Key_CapsLock;
  _keyMap[osgGA::GUIEventAdapter::KEY_Shift_Lock] = Qt::Key_CapsLock;

  _keyMap[osgGA::GUIEventAdapter::KEY_Meta_L] = Qt::Key_Meta; // Qt doesn't have a Meta L
  _keyMap[osgGA::GUIEventAdapter::KEY_Meta_R] = Qt::Key_Meta; // Qt doesn't have a Meta R
  _keyMap[osgGA::GUIEventAdapter::KEY_Alt_L] = Qt::Key_Alt; // Qt doesn't have a Alt L
  _keyMap[osgGA::GUIEventAdapter::KEY_Alt_R] = Qt::Key_Alt; // Qt doesn't have a Alt R
  _keyMap[osgGA::GUIEventAdapter::KEY_Super_L] = Qt::Key_Super_L;
  _keyMap[osgGA::GUIEventAdapter::KEY_Super_R] = Qt::Key_Super_R;
  _keyMap[osgGA::GUIEventAdapter::KEY_Hyper_L] = Qt::Key_Hyper_L;
  _keyMap[osgGA::GUIEventAdapter::KEY_Hyper_R] = Qt::Key_Hyper_R;

  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Space] = Qt::Key_Space;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Tab] = Qt::Key_Tab;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Enter] = Qt::Key_Enter;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_F1] = Qt::Key_F1;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_F2] = Qt::Key_F2;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_F3] = Qt::Key_F3;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_F4] = Qt::Key_F4;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Home] = Qt::Key_Home;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Left] = Qt::Key_Left;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Up] = Qt::Key_Up;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Right] = Qt::Key_Right;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Down] = Qt::Key_Down;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Prior] = Qt::Key_Left;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Page_Up] = Qt::Key_PageUp;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Next] = Qt::Key_Right;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Page_Down] = Qt::Key_PageDown;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_End] = Qt::Key_End;

  // _keyMap[osgGA::GUIEventAdapter::KEY_KP_Begin] = Qt::Key_Begin;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Insert] = Qt::Key_Insert;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Delete] = Qt::Key_Delete;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Equal] = Qt::Key_Equal;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Multiply] = Qt::Key_Asterisk;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Add] = Qt::Key_Plus;
  //_keyMap[osgGA::GUIEventAdapter::KEY_KP_Separator] = Qt::Key_;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Subtract] = Qt::Key_Minus;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Decimal] = Qt::Key_Period;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_Divide] = Qt::Key_division;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_0] = Qt::Key_0;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_1] = Qt::Key_1;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_2] = Qt::Key_2;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_3] = Qt::Key_3;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_4] = Qt::Key_4;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_5] = Qt::Key_5;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_6] = Qt::Key_6;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_7] = Qt::Key_7;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_8] = Qt::Key_8;
  _keyMap[osgGA::GUIEventAdapter::KEY_KP_9] = Qt::Key_9;

  _keyMap[osgGA::GUIEventAdapter::KEY_F1] = Qt::Key_F1;
  _keyMap[osgGA::GUIEventAdapter::KEY_F2] = Qt::Key_F2;
  _keyMap[osgGA::GUIEventAdapter::KEY_F3] = Qt::Key_F3;
  _keyMap[osgGA::GUIEventAdapter::KEY_F4] = Qt::Key_F4;
  _keyMap[osgGA::GUIEventAdapter::KEY_F5] = Qt::Key_F5;
  _keyMap[osgGA::GUIEventAdapter::KEY_F6] = Qt::Key_F6;
  _keyMap[osgGA::GUIEventAdapter::KEY_F7] = Qt::Key_F7;
  _keyMap[osgGA::GUIEventAdapter::KEY_F8] = Qt::Key_F8;
  _keyMap[osgGA::GUIEventAdapter::KEY_F9] = Qt::Key_F9;
  _keyMap[osgGA::GUIEventAdapter::KEY_F10] = Qt::Key_F10;
  _keyMap[osgGA::GUIEventAdapter::KEY_F11] = Qt::Key_F11;
  _keyMap[osgGA::GUIEventAdapter::KEY_F12] = Qt::Key_F12;
  _keyMap[osgGA::GUIEventAdapter::KEY_F13] = Qt::Key_F13;
  _keyMap[osgGA::GUIEventAdapter::KEY_F14] = Qt::Key_F14;
  _keyMap[osgGA::GUIEventAdapter::KEY_F15] = Qt::Key_F15;
  _keyMap[osgGA::GUIEventAdapter::KEY_F16] = Qt::Key_F16;
  _keyMap[osgGA::GUIEventAdapter::KEY_F17] = Qt::Key_F17;
  _keyMap[osgGA::GUIEventAdapter::KEY_F18] = Qt::Key_F18;
  _keyMap[osgGA::GUIEventAdapter::KEY_F19] = Qt::Key_F19;
  _keyMap[osgGA::GUIEventAdapter::KEY_F20] = Qt::Key_F20;
  _keyMap[osgGA::GUIEventAdapter::KEY_F21] = Qt::Key_F21;
  _keyMap[osgGA::GUIEventAdapter::KEY_F22] = Qt::Key_F22;
  _keyMap[osgGA::GUIEventAdapter::KEY_F23] = Qt::Key_F23;
  _keyMap[osgGA::GUIEventAdapter::KEY_F24] = Qt::Key_F24;
  _keyMap[osgGA::GUIEventAdapter::KEY_F25] = Qt::Key_F25;
  _keyMap[osgGA::GUIEventAdapter::KEY_F26] = Qt::Key_F26;
  _keyMap[osgGA::GUIEventAdapter::KEY_F27] = Qt::Key_F27;
  _keyMap[osgGA::GUIEventAdapter::KEY_F28] = Qt::Key_F28;
  _keyMap[osgGA::GUIEventAdapter::KEY_F29] = Qt::Key_F29;
  _keyMap[osgGA::GUIEventAdapter::KEY_F30] = Qt::Key_F30;
  _keyMap[osgGA::GUIEventAdapter::KEY_F31] = Qt::Key_F31;
  _keyMap[osgGA::GUIEventAdapter::KEY_F32] = Qt::Key_F32;
  _keyMap[osgGA::GUIEventAdapter::KEY_F33] = Qt::Key_F33;
  _keyMap[osgGA::GUIEventAdapter::KEY_F34] = Qt::Key_F34;
  _keyMap[osgGA::GUIEventAdapter::KEY_F35] = Qt::Key_F35;

}


/*QWidget* OSGQuickAdapter::getWidgetAt(const QPoint& pos)
{
  QWidget* childAt = _graphicsView->childAt(pos);
  if (childAt)
  {
    return childAt;
  }

  QGraphicsItem* item = _graphicsView->itemAt(pos);
  if (item /*&& item->contains(item->mapFromScene(pos))*//*)
  {
    QGraphicsProxyWidget* p = qgraphicsitem_cast<QGraphicsProxyWidget*>(item);
    if (p)
    {
      childAt = p->widget();
      QWidget* c;
      while ((c = childAt->childAt(childAt->mapFromGlobal(pos))) != 0)
      {
        childAt = c;
      }

      // Widgets like QTextEdit will automatically add child scroll area widgets
      // that will be selected by childAt(), we have to change to parents at that moment
      // Hardcoded by the internal widget's name 'qt_scrollarea_viewport' at present
      if (childAt->objectName() == "qt_scrollarea_viewport")
      {
        childAt = childAt->parentWidget();
      }
      return childAt;
    }
  }
  return NULL;
}*/


bool OSGQuickAdapter::sendPointerEvent(int x, int y, int buttonMask)
{
  _previousQtMouseX = x;
  _previousQtMouseY = _size.height() - y; // TODO m_rootItem does not provide its size, using _size

  QPoint pos(_previousQtMouseX, _previousQtMouseY);

  /*if (_backgroundWidget && _backgroundWidget == targetWidget)
  {
    // Mouse is at background widget, so ignore such events
    return false;
  }*/

  if (_previousSentEvent && buttonMask != 0) // targetWidget != NULL
  {
    QCoreApplication::postEvent(this, new MyQPointerEvent(x, y, buttonMask));
    _previousSentEvent = true;
    return true;
  }

  _previousSentEvent = false;
  return false;
}


bool OSGQuickAdapter::handlePointerEvent(int x, int y, int buttonMask)
{
  y = _size.height() - y; // TODO m_rootItem does not provide its size, using _size

  bool leftButtonPressed = (buttonMask & osgGA::GUIEventAdapter::LEFT_MOUSE_BUTTON) != 0;
  bool middleButtonPressed = (buttonMask & osgGA::GUIEventAdapter::MIDDLE_MOUSE_BUTTON) != 0;
  bool rightButtonPressed = (buttonMask & osgGA::GUIEventAdapter::RIGHT_MOUSE_BUTTON) != 0;

  bool prev_leftButtonPressed = (_previousButtonMask & osgGA::GUIEventAdapter::LEFT_MOUSE_BUTTON) != 0;
  bool prev_middleButtonPressed = (_previousButtonMask & osgGA::GUIEventAdapter::MIDDLE_MOUSE_BUTTON) != 0;
  bool prev_rightButtonPressed = (_previousButtonMask & osgGA::GUIEventAdapter::RIGHT_MOUSE_BUTTON) != 0;

  Qt::MouseButtons qtMouseButtons =
    (leftButtonPressed ? Qt::LeftButton : Qt::NoButton) |
    (middleButtonPressed ? Qt::MidButton : Qt::NoButton) |
    (rightButtonPressed ? Qt::RightButton : Qt::NoButton);

  const QPoint globalPos(x, y);

  if (buttonMask != _previousButtonMask)
  {
    Qt::MouseButton qtButton = Qt::NoButton;
    QEvent::Type eventType = QEvent::None;
    if (leftButtonPressed != prev_leftButtonPressed)
    {
      qtButton = Qt::LeftButton;
      eventType = leftButtonPressed ? QEvent::MouseButtonPress : QEvent::MouseButtonRelease;
    }
    else if (middleButtonPressed != prev_middleButtonPressed)
    {
      qtButton = Qt::MidButton;
      eventType = middleButtonPressed ? QEvent::MouseButtonPress : QEvent::MouseButtonRelease;
    }
    else if (rightButtonPressed != prev_rightButtonPressed)
    {
      qtButton = Qt::RightButton;
      eventType = rightButtonPressed ? QEvent::MouseButtonPress : QEvent::MouseButtonRelease;
      if (!rightButtonPressed)
      {
        /*if (targetWidget)
        {
          QPoint localPos = targetWidget->mapFromGlobal(globalPos);
          QContextMenuEvent* cme = new QContextMenuEvent(QContextMenuEvent::Mouse, localPos, globalPos);
          QCoreApplication::postEvent(targetWidget, cme);
        }*/
      }
    }

    if (eventType == QEvent::MouseButtonPress)
    {
      _image->sendFocusHint(true);
      //if (targetWidget) targetWidget->setFocus(Qt::MouseFocusReason);
    }

    QMouseEvent event(eventType, globalPos, qtButton, qtMouseButtons, 0);
    QCoreApplication::sendEvent(m_quickWindow.data(), &event);

    _previousButtonMask = buttonMask;
  }
  else if (x != _previousMouseX || y != _previousMouseY)
  {
    QMouseEvent event(QEvent::MouseMove, globalPos, Qt::NoButton, qtMouseButtons, 0);
    QCoreApplication::sendEvent(m_quickWindow.data(), &event);

    _previousMouseX = x;
    _previousMouseY = y;
  }

  return true;
}


bool OSGQuickAdapter::sendKeyEvent(int key, bool keyDown)
{
  QPoint pos(_previousQtMouseX, _previousQtMouseY);
  //QWidget* targetWidget = getWidgetAt(pos);
  /*if (_backgroundWidget && _backgroundWidget == targetWidget)
  {
    // Mouse is at background widget, so ignore such events
    return false;
  }*/

  //if (targetWidget != NULL)
  {
    QCoreApplication::postEvent(this, new MyQKeyEvent(key, keyDown));
    return true;
  }

  return false;
}


bool OSGQuickAdapter::handleKeyEvent(int key, bool keyDown)
{
  QEvent::Type eventType = keyDown ? QEvent::KeyPress : QEvent::KeyRelease;

  if (key == Qt::Key_Shift)
  {
    _qtKeyModifiers = (_qtKeyModifiers & ~Qt::ShiftModifier) | (keyDown ? Qt::ShiftModifier : Qt::NoModifier);
  }

  if (key == Qt::Key_Control)
  {
    _qtKeyModifiers = (_qtKeyModifiers & ~Qt::ControlModifier) | (keyDown ? Qt::ControlModifier : Qt::NoModifier);
  }

  if (key == Qt::Key_Alt || key == Qt::Key_AltGr)
  {
    _qtKeyModifiers = (_qtKeyModifiers & ~Qt::ControlModifier) | (keyDown ? Qt::ControlModifier : Qt::NoModifier);
  }

  if (key == Qt::Key_Meta)
  {
    _qtKeyModifiers = (_qtKeyModifiers & ~Qt::MetaModifier) | (keyDown ? Qt::MetaModifier : Qt::NoModifier);
  }

  Qt::Key qtkey;
  QChar input;

  KeyMap::iterator itr = _keyMap.find(key);
  if (itr != _keyMap.end())
  {
    qtkey = itr->second;
  }
  else
  {
    qtkey = (Qt::Key)key;
    input = QChar(key);
  }

  QKeyEvent event(eventType, qtkey, _qtKeyModifiers, input);
  QCoreApplication::sendEvent(m_quickWindow.data(), &event);
  return true;
}


void OSGQuickAdapter::setFrameLastRendered(const osg::FrameStamp* frameStamp)
{
  if (_newImageAvailable && _previousFrameNumber != frameStamp->getFrameNumber())
  {
    {
      OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_qimagesMutex);

      // make sure that _previousFrameNumber hasn't been updated by another thread since we entered this branch.
      if (_previousFrameNumber == frameStamp->getFrameNumber()) return;
      _previousFrameNumber = frameStamp->getFrameNumber();

      std::swap(_currentRead, _previousWrite);
      _newImageAvailable = false;
    }

    assignImage(_currentRead);
  }
}


void OSGQuickAdapter::clearWriteBuffer()
{
  QImage& image = _qimages[_currentWrite];
  image.fill(_backgroundColor.rgba());
  image = QGLWidget::convertToGLFormat(image);

  // swap the write buffers in a thread safe way
  OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_qimagesMutex);
  std::swap(_currentWrite, _previousWrite);
  _newImageAvailable = true;
}


void OSGQuickAdapter::render()
{
  QImage& image = _qimages[_currentWrite];
  _requiresRendering = false;

  // If we got a resize, act on it, first by resizing the view, then the current image

  {
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_qresizeMutex);
    /*if (_graphicsView->size().width() != _size.width() || _graphicsView->size().height() != _size.height())
    {
      _graphicsView->setGeometry(0, 0, _size.width(), _size.height());
      _graphicsView->viewport()->setGeometry(0, 0, _size.width(), _size.height());

      _widget->setGeometry(0, 0, _size.width(), _size.height());
    }*/

    if (image.width() != _size.width() || image.height() != _size.height())
    {
      _qimages[_currentWrite] = QImage(_size.width(), _size.height(), s_imageFormat);
      image = _qimages[_currentWrite];
    }
  }

  // ***************************************************************************
  // * Main render functionality is here
  // ***************************************************************************
  if (!m_context->makeCurrent(m_offscreenSurface))
    return;

  // Polish, synchronize and render the next frame (into our fbo).  In this example
  // everything happens on the same thread and therefore all three steps are performed
  // in succession from here. In a threaded setup the render() call would happen on a
  // separate thread.
  m_renderControl->polishItems();
  m_renderControl->sync();
  m_renderControl->render();

  m_quickWindow->resetOpenGLState();
  QOpenGLFramebufferObject::bindDefault();

  m_context->functions()->glFlush();

  m_quickReady = true;

  // Get the result for rendering
  image = m_fbo->toImage();

  // ***************************************************************************

  // convert into OpenGL format - flipping around the Y axis and swizzling the pixels
  image = QGLWidget::convertToGLFormat(image);

  // swap the write buffers in a thread safe way
  OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_qimagesMutex);
  std::swap(_currentWrite, _previousWrite);
  _newImageAvailable = true;
}


void OSGQuickAdapter::assignImage(unsigned int i)
{
  QImage& image = _qimages[i];
  unsigned char* data = image.bits();

  _image->setImage(image.width(), image.height(), 1,
                   4, GL_RGBA, GL_UNSIGNED_BYTE,
                   data, osg::Image::NO_DELETE, 1);
}


void OSGQuickAdapter::resize(int width, int height)
{
  // Save the new width and height which will take effect on the next render() (in the Qt thread).
  {
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(_qresizeMutex);
    _size.setWidth(width);
    _size.setHeight(height);
  }

  // Make current here okay? Certainly not when Quick is on another thread!
  if (!m_rootItem.isNull() && m_context->makeCurrent(m_offscreenSurface)) {
    destroyFbo();
    createFbo();
    m_context->doneCurrent();
    updateSizes();
  }

  // Force an update so render() will be called.
  //_graphicsScene->update(_graphicsScene->sceneRect());
}


void OSGQuickAdapter::run()
{
  disconnect(m_qmlComponent, &QQmlComponent::statusChanged, this, &OSGQuickAdapter::run);

  if (m_qmlComponent->isError()) {
    const QList<QQmlError> errorList = m_qmlComponent->errors();
    for (const QQmlError &error : errorList)
      qWarning() << error.url() << error.line() << error;
    return;
  }

  QObject *rootObject = m_qmlComponent->create();
  if (m_qmlComponent->isError()) {
    const QList<QQmlError> errorList = m_qmlComponent->errors();
    for (const QQmlError &error : errorList)
      qWarning() << error.url() << error.line() << error;
    return;
  }

  m_rootItem = qobject_cast<QQuickItem *>(rootObject);
  if (!m_rootItem) {
    qWarning("run: Not a QQuickItem");
    delete rootObject;
    return;
  }

  // The root item is ready. Associate it with the window.
  m_rootItem->setParentItem(m_quickWindow->contentItem());

  // Update item and rendering related geometries.
  updateSizes();

  // Initialize the render control and our OpenGL resources.
  m_context->makeCurrent(m_offscreenSurface);
  m_renderControl->initialize(m_context);
  m_quickInitialized = true;
}


void OSGQuickAdapter::startQuick()
{
  m_qmlComponent = new QQmlComponent(m_qmlEngine, QUrl(_qmlResource));
  if (m_qmlComponent->isLoading())
    connect(m_qmlComponent, &QQmlComponent::statusChanged, this, &OSGQuickAdapter::run);
  else
    run();
}


void OSGQuickAdapter::updateSizes()
{
  // Behave like SizeRootObjectToView.
  m_rootItem->setWidth(_size.width());
  m_rootItem->setHeight(_size.height());

  m_quickWindow->setGeometry(0, 0, _size.width(), _size.height());
}
