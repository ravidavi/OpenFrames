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

#ifndef _OF_CONTROLS_
#define _OF_CONTROLS_

#include <string>
#include <map>

#include <QObject>

#include <osg/ref_ptr>
#include <osg/Vec3d>
#include <osg/Vec4>

QT_FORWARD_DECLARE_CLASS(QPushButton);
QT_FORWARD_DECLARE_CLASS(QCheckBox);
QT_FORWARD_DECLARE_CLASS(QListWidget);
QT_FORWARD_DECLARE_CLASS(QListWidgetItem);

namespace OpenFrames
{
  class WindowProxy;
  class ReferenceFrame;
  class QWidgetPanel;
  class Sphere;
}

/*******************************************************************
 * Matthew Ruschmann
 * OpenFrames Demo of embedded Qt control panels
 ******************************************************************/
class OFControls : public QObject
{
  Q_OBJECT

public:
  OFControls(bool useVR);
  virtual ~OFControls();

  // Reserve copy and assignment constructors
  OFControls(OFControls&) = delete;
  OFControls& operator=(OFControls&) = delete;

  /// The applications main loop
  int main(int argc, char **argv);

public slots:
  /// Slot for toggling visibility
  void toggleSphere();
  /// Slot for toggling visibility
  void setSphere(bool checked);
  /// Slot for toggling color
  void setColor(QListWidgetItem *item);
  /// Slot for toggling hidden panel
  void setHiddenPanel(bool checked = false);
  /// Slot for moving the sphere based on slider output
  void setXLocation(int position);
  /// Slot for moving the sphere based on slider output
  void setYLocation(int position);
  /// Slot for moving the sphere based on slider output
  void setZLocation(int position);

private:
  static const double MAIN_LOOP_PERIOD;
  static const char *LOREM_IPSUM_DOLOR;
  static const std::map<std::string, osg::Vec4> COLORS;
  static const char *DEFAULT_SPHERE_COLOR;

  /// Default Panel dimensions
  static const double panelWidth, panelHeight, panelZPos;

  QPushButton *_toggleButton;
  QCheckBox *_showCheckBox;
  QListWidget *_list;

  /// Sphere origin
  osg::Vec3d _sphereOrigin;

  osg::ref_ptr<OpenFrames::WindowProxy> _windowProxy;
  osg::ref_ptr<OpenFrames::ReferenceFrame> _root;
  osg::ref_ptr<OpenFrames::QWidgetPanel> _hiddenPanel;
  osg::ref_ptr<OpenFrames::Sphere> _sphere;
};

#endif
