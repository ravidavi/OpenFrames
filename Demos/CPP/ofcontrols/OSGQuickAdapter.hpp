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

#ifndef OSGQUICKADAPTER
#define OSGQUICKADAPTER

#include <osg/Image>
#include <osg/observer_ptr>

#include <QObject>
#include <QPointer>
#include <QScopedPointer>
#include <QtEvents>
 // TODO Remove these headers
#include <QGraphicsScene>
#include <QGraphicsView>
#include <QApplication>
#include <QPainter>

QT_FORWARD_DECLARE_CLASS(QOpenGLContext)
QT_FORWARD_DECLARE_CLASS(QOpenGLFramebufferObject)
QT_FORWARD_DECLARE_CLASS(QOffscreenSurface)
QT_FORWARD_DECLARE_CLASS(QQuickRenderControl)
QT_FORWARD_DECLARE_CLASS(QQuickWindow)
QT_FORWARD_DECLARE_CLASS(QQmlEngine)
QT_FORWARD_DECLARE_CLASS(QQmlComponent)
QT_FORWARD_DECLARE_CLASS(QQuickItem)


class OSGQuickAdapter : public QObject
{
  Q_OBJECT

public:
  static QCoreApplication* getOrCreateQApplication();

  OSGQuickAdapter(osg::Image* image, const QUrl &qmlScene);
  virtual ~OSGQuickAdapter();

  void setUpKeyMap();

  bool sendPointerEvent(int x, int y, int buttonMask);


  bool sendKeyEvent(int key, bool keyDown);


  void setFrameLastRendered(const osg::FrameStamp* frameStamp);

  void clearWriteBuffer();

  bool requiresRendering() const { return _requiresRendering; }
  void render();

  void assignImage(unsigned int i);

  void resize(int width, int height);

  void setBackgroundColor(QColor color) { _backgroundColor = color; }
  QColor getBackgroundColor() const { return _backgroundColor; }

  QQuickWindow *getQuickWindow();

signals:
  void constructed();

protected:
  bool handlePointerEvent(int x, int y, int buttonMask);
  bool handleKeyEvent(int key, bool keyDown);
  //QWidget* getWidgetAt(const QPoint& pos);

  osg::observer_ptr<osg::Image>   _image;

  int                             _previousButtonMask;
  int                             _previousMouseX;
  int                             _previousMouseY;
  int                             _previousQtMouseX;
  int                             _previousQtMouseY;
  bool                            _previousSentEvent;
  bool                            _requiresRendering;

  QSize _size;

  typedef std::map<int, Qt::Key> KeyMap;
  KeyMap                          _keyMap;
  Qt::KeyboardModifiers           _qtKeyModifiers;

  QColor                          _backgroundColor;
  QPointer<QQuickItem> m_rootItem;
  QPointer<QQuickRenderControl> m_renderControl;
  QPointer<QQmlComponent> m_qmlComponent;
  QPointer<QQuickWindow> m_quickWindow;
  QPointer<QQmlEngine> m_qmlEngine;
  QOpenGLFramebufferObject *m_fbo;
  QPointer<QOffscreenSurface> m_offscreenSurface;
  QPointer<QOpenGLContext> m_context;
  bool m_quickInitialized;
  bool m_quickReady;

  OpenThreads::Mutex              _qimagesMutex;
  OpenThreads::Mutex              _qresizeMutex;
  unsigned int                    _previousFrameNumber;
  bool                            _newImageAvailable;
  unsigned int                    _currentRead;
  unsigned int                    _currentWrite;
  unsigned int                    _previousWrite;
  QImage                          _qimages[3];
  QUrl                         _qmlResource;

  virtual void customEvent(QEvent * event);

private slots:
  void startQuick();
  void run();
  void createFbo();
  void destroyFbo();
  void requestUpdate();
  //void repaintRequestedSlot(const QList<QRectF> &regions);
  //void repaintRequestedSlot(const QRectF &region);

private:
  void updateSizes();
};

#endif
