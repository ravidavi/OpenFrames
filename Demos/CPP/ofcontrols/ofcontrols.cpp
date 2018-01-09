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

#include "ofcontrols.hpp"
#include "QWidgetPanel.hpp"

#include <OpenFrames/WindowProxy.hpp>
#include <OpenFrames/Sphere.hpp>

#include <QApplication>
#include <QWidget>
#include <QVBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QCheckBox>
#include <QListWidget>
#include <QSlider>

/// The period of event checking in the main loop (milliseconds)
const double OFControls::MAIN_LOOP_PERIOD = 10;

/// Prose for the text edit control
const char *OFControls::LOREM_IPSUM_DOLOR =
  "This is an example of Qt widgets in an OSG texture. The widgets control the sphere sitting above.";

/// Colors for the sphere demo
const std::map<std::string, osg::Vec4> OFControls::COLORS =
  {
    { "Red",{ 1.0, 0.0, 0.0, 0.0 } },
    { "Green",{ 0.0, 0.5, 0.0, 0.0 } },
    { "Blue",{ 0.0, 0.0, 1.0, 0.0 } },
    { "Purple",{ 1.0, 0.0, 1.0, 0.0 } },
    { "Yellow",{ 1.0, 1.0, 0.0, 0.0 } },
    { "Cyan",{ 0.0, 1.0, 1.0, 0.0 } },
    { "Orange",{ 1.0, 0.5, 0.0, 0.0 } },
    { "Navy",{ 0.0, 0.0, 0.5, 0.0 } },
    { "Gray",{ 0.5, 0.5, 0.5, 0.0 } },
};

/// Default color for the sphere
const char *OFControls::DEFAULT_SPHERE_COLOR = COLORS.begin()->first.c_str();

int main(int argc, char **argv)
{
  // Qt requires that we construct the global QApplication before creating any widgets.
  QApplication app(argc, argv);

  // Configure the default appearance for our VR world
  QFont font = app.font();
  font.setPixelSize(20);
  app.setFont(font);

  // Start main app
  static OFControls controls;
  controls.main(argc, argv);
}

OFControls::OFControls()
{
  // Instantiate and configure OpenFrames objects
  _windowProxy = new OpenFrames::WindowProxy(100, 100, 640, 480, 1, 1, false, false);

  _root = new OpenFrames::ReferenceFrame("root");
  _root->showNameLabel(false);
  _root->showAxes(0U);
  _root->showAxesLabels(0U);

  OpenFrames::QWidgetPanel *panel1 = new OpenFrames::QWidgetPanel("panel");
  panel1->setColor(0.8, 0.8, 0.8, 0.9);
  panel1->setHalfLengths(1.5, 1.2, 0.1);
  panel1->setAttitude(-0.707106781186547, 0.0, 0.0, 0.707106781186547);
  panel1->setPosition(0.0, 0.0, -1.0);
  panel1->showAxes(0U);
  panel1->showAxesLabels(0U);
  panel1->setPixelsPerUnit(100.0);
  _root->addChild(panel1);

  OpenFrames::QWidgetPanel *panel2 = new OpenFrames::QWidgetPanel("panel");
  panel2->setColor(0.8, 0.8, 0.8, 0.9);
  panel2->setHalfLengths(1.5, 1.2, 0.1);
  panel2->setPosition(3.1, 0.0, -1.0);
  panel2->setAttitude(-0.707106781186547, 0.0, 0.0, 0.707106781186547);
  panel2->showAxes(0U);
  panel2->showAxesLabels(0U);
  panel2->setPixelsPerUnit(50.0);
  _root->addChild(panel2);

  OpenFrames::QWidgetPanel *panel3 = new OpenFrames::QWidgetPanel("panel");
  panel3->setColor(0.8, 0.8, 0.8, 0.9);
  panel3->setHalfLengths(1.5, 1.2, 0.1);
  panel3->setPosition(-3.1, 0.0, -1.0);
  panel3->setAttitude(-0.707106781186547, 0.0, 0.0, 0.707106781186547);
  panel3->showAxes(0U);
  panel3->showAxesLabels(0U);
  panel3->setPixelsPerUnit(90.0);
  _root->addChild(panel3);

  _hiddenPanel = new OpenFrames::QWidgetPanel("panel");
  _hiddenPanel->setColor(0.8, 0.8, 0.8, 0.8);
  _hiddenPanel->setHalfLengths(1.5, 1.2, 0.1);
  _hiddenPanel->setPosition(3.1, 0.0, 1.5);
  _hiddenPanel->setAttitude(-0.707106781186547, 0.0, 0.0, 0.707106781186547);
  _hiddenPanel->showAxes(0U);
  _hiddenPanel->showAxesLabels(0U);
  _hiddenPanel->setPixelsPerUnit(50.0);

  _sphere = new OpenFrames::Sphere("sphere");
  _sphere->setPosition(-1.55, 0.0, 1.0);
  _sphere->setColor(COLORS.at(DEFAULT_SPHERE_COLOR));
  _sphere->showNameLabel(false);
  _sphere->showAxes(7U);
  _sphere->showAxesLabels(7U);
  _sphere->setRadius(1.0);
  _root->addChild(_sphere);

  // Create the example controls widget
  QWidget *widget1 = new QWidget;
  widget1->setLayout(new QVBoxLayout);
  QString text(LOREM_IPSUM_DOLOR);
  QLabel *textLabel = new QLabel(text);
  textLabel->setWordWrap(true);
  widget1->layout()->addWidget(textLabel);
  _toggleButton = new QPushButton("Hide Sphere");
  widget1->layout()->addWidget(_toggleButton);
  panel1->setWidget(widget1);

  // Create the example controls widget
  QWidget *widget2 = new QWidget;
  widget2->setLayout(new QVBoxLayout);
  _checkBox = new QCheckBox("Show sphere");
  _checkBox->setChecked(true);
  QCheckBox *moveSphereBox = new QCheckBox("Move sphere");
  widget2->layout()->addWidget(_checkBox);
  widget2->layout()->addWidget(moveSphereBox);
  panel2->setWidget(widget2);

  // Create the example list view
  QWidget *widget3 = new QWidget;
  widget3->setLayout(new QVBoxLayout);
  _list = new QListWidget();
  _list->setIconSize(QSize(30, 30));
  for (auto color = COLORS.begin(); color != COLORS.end(); color++)
  {
    QListWidgetItem *item = new QListWidgetItem(color->first.c_str());
    const osg::Vec4 &osgColor = color->second;
    QColor qColor(round(osgColor[0]*255.0), round(osgColor[1]*255.0), round(osgColor[2]*255.0));
    QFont font = item->font();
    font.setPixelSize(30);
    item->setFont(font);
    QPixmap pixmap(QSize(30, 30));
    pixmap.fill(qColor);
    item->setIcon(QIcon(pixmap));
    _list->addItem(item);
  }
  QList<QListWidgetItem *> matchingItems = _list->findItems(DEFAULT_SPHERE_COLOR, Qt::MatchExactly);
  if (matchingItems.size() > 0)
  {
    _list->setCurrentItem(matchingItems[0]);
  }
  widget3->layout()->addWidget(_list);
  panel3->setWidget(widget3);

  // Create the example controls widget
  QWidget *widget = new QWidget;
  widget->setLayout(new QVBoxLayout);
  QSlider *xSlider = new QSlider(Qt::Horizontal);
  xSlider->setRange(-20, 20);
  QSlider *ySlider = new QSlider(Qt::Horizontal);
  ySlider->setRange(-20, 20);
  QSlider *zSlider = new QSlider(Qt::Horizontal);
  zSlider->setRange(0, 20);
  widget->layout()->addWidget(xSlider);
  widget->layout()->addWidget(ySlider);
  widget->layout()->addWidget(zSlider);
  _hiddenPanel->setWidget(widget);

  // Connect QObject signals to our slots
  connect(moveSphereBox, &QCheckBox::clicked, this, &OFControls::toggleHiddenPanel);
  connect(_toggleButton, &QPushButton::clicked, this, &OFControls::toggleSphere);
  connect(_checkBox, &QCheckBox::clicked, this, &OFControls::toggleSphere);
  connect(_list, &QListWidget::itemActivated, this, &OFControls::setColor);
  connect(_list, &QListWidget::itemClicked, this, &OFControls::setColor);
  connect(xSlider, &QSlider::valueChanged, this, &OFControls::setXLocation);
  connect(ySlider, &QSlider::valueChanged, this, &OFControls::setYLocation);
  connect(zSlider, &QSlider::valueChanged, this, &OFControls::setZLocation);

  // Setup WindowProxy frame and views
  OpenFrames::FrameManager *frameManager = new OpenFrames::FrameManager();
  frameManager->setFrame(_root);
  _windowProxy->setScene(frameManager, 0U, 0U);
  _windowProxy->getGridPosition(0U, 0U)->addView(new OpenFrames::View(_root, _root, OpenFrames::View::ABSOLUTE_FRAME));
  OpenFrames::FollowingTrackball *trackball = _windowProxy->getGridPosition(0U, 0U)->getCurrentView()->getTrackball();
  trackball->setDistance(8.0);
  _windowProxy->getGridPosition(0U, 0U)->getCurrentView()->setTrackball(trackball);
}

OFControls::~OFControls()
{
  // Nothing to do here
}

int OFControls::main(int argc, char **argv)
{
  // Run OpenFrames
  OpenFrames::FramerateLimiter waitLimiter;
  waitLimiter.setDesiredFramerate(1000.0/MAIN_LOOP_PERIOD); // FPS at which to check events

  _windowProxy->start();
  while (_windowProxy->isRunning())
  {
    // check events periodically
    QCoreApplication::processEvents(QEventLoop::AllEvents, MAIN_LOOP_PERIOD);
    waitLimiter.frame();
  }

  return 0;
}

void OFControls::toggleSphere(bool checked)
{
  _windowProxy->getGridPosition(0U, 0U)->getFrameManager()->lock();
  if (_root->getChildIndex(_sphere) >= 0)
  {
    _checkBox->setChecked(false);
    _toggleButton->setText("Show Sphere");
    _root->removeChild(_sphere);
  }
  else
  {
    _checkBox->setChecked(true);
    _toggleButton->setText("Hide Sphere");
    _root->addChild(_sphere);
  }
  _windowProxy->getGridPosition(0U, 0U)->getFrameManager()->unlock();
}

void OFControls::setColor(QListWidgetItem *item)
{
  _sphere->setColor(COLORS.at(item->text().toStdString()));
}

void OFControls::toggleHiddenPanel(bool checked)
{
  _windowProxy->getGridPosition(0U, 0U)->getFrameManager()->lock();
  if (_root->getChildIndex(_hiddenPanel) >= 0)
  {
    _root->removeChild(_hiddenPanel);
  }
  else
  {
    _root->addChild(_hiddenPanel);
  }
  _windowProxy->getGridPosition(0U, 0U)->getFrameManager()->unlock();
}

void OFControls::setXLocation(int position)
{
  osg::Vec3d spherePosition;
  _sphere->getPosition(spherePosition);
  spherePosition[0] = static_cast<double>(position) * 0.1 - 1.55;
  _sphere->setPosition(spherePosition);
}

void OFControls::setYLocation(int position)
{
  osg::Vec3d spherePosition;
  _sphere->getPosition(spherePosition);
  spherePosition[1] = static_cast<double>(position) * 0.1;
  _sphere->setPosition(spherePosition);
}

void OFControls::setZLocation(int position)
{
  osg::Vec3d spherePosition;
  _sphere->getPosition(spherePosition);
  spherePosition[2] = 1.0 + (static_cast<double>(position) * 0.1);
  _sphere->setPosition(spherePosition);
}
