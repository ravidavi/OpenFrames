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
#include "ui_sphereoptionsform.h"

#include <OpenFrames/QWidgetPanel.hpp>
#include <OpenFrames/WindowProxy.hpp>
#include <OpenFrames/Sphere.hpp>

#include <QApplication>
#include <QWidget>
#include <QVBoxLayout>
#include <QTextEdit>
#include <QPushButton>
#include <QCheckBox>
#include <QListWidget>
#include <QSlider>
#include <QtUiTools>

/// The period of event checking in the main loop (milliseconds)
const double OFControls::MAIN_LOOP_PERIOD = 10;

/// Prose for the text edit control
const char *OFControls::LOREM_IPSUM_DOLOR =
  "This is an example of Qt widgets in an OSG texture. The widgets control the sphere sitting above.";

/// Colors for the sphere demo
const std::map<std::string, osg::Vec4> OFControls::COLORS =
  {
    { "Red", { 1.0, 0.0, 0.0, 1.0 } },
    { "Green", { 0.0, 0.5, 0.0, 1.0 } },
    { "Blue", { 0.0, 0.0, 1.0, 1.0 } },
    { "Purple", { 1.0, 0.0, 1.0, 1.0 } },
    { "Yellow", { 1.0, 1.0, 0.0, 1.0 } },
    { "Cyan", { 0.0, 1.0, 1.0, 1.0 } },
    { "Orange", { 1.0, 0.5, 0.0, 1.0 } },
    { "Navy", { 0.0, 0.0, 0.5, 1.0 } },
    { "Gray", { 0.5, 0.5, 0.5, 1.0 } },
};

/// Default color for the sphere
const char *OFControls::DEFAULT_SPHERE_COLOR = COLORS.begin()->first.c_str();

int main(int argc, char **argv)
{
  // Qt requires that we construct the global QApplication before creating any widgets.
  QApplication *app;
  app = new QApplication(argc, argv);

  // Configure the default appearance for our VR world
  QFont font = app->font();
  font.setPixelSize(20);
  app->setFont(font);

  // Start main app
  static OFControls controls;
  controls.main(argc, argv);
}

OFControls::OFControls()
{
  osg::Vec4 bgColor = { 0.8f, 0.8f, 0.8f, 0.9f };

  // Instantiate and configure OpenFrames objects
  _windowProxy = new OpenFrames::WindowProxy(100, 100, 640, 480, 1, 1, false, false);

  _root = new OpenFrames::ReferenceFrame("root");
  _root->showNameLabel(false);
  _root->showAxes(0U);
  _root->showAxesLabels(0U);

  // The sphere that will be manipulated by Qt controls
  _sphere = new OpenFrames::Sphere("sphere");
  _sphere->setPosition(-1.55, 0.0, 1.0);
  _sphere->setColor(COLORS.at(DEFAULT_SPHERE_COLOR));
  _sphere->showNameLabel(false);
  _sphere->showAxes(0U);
  _sphere->showAxesLabels(0U);
  _sphere->setRadius(1.0);
  _root->addChild(_sphere);

  // Panel that will hold a text editor
  OpenFrames::QWidgetPanel *editorPanel = new OpenFrames::QWidgetPanel("panel");
  editorPanel->setColor(bgColor);
  editorPanel->setHalfLengths(1.5, 1.2, 0.1);
  editorPanel->setAttitude(-0.707106781186547, 0.0, 0.0, 0.707106781186547);
  editorPanel->setPosition(0.0, 0.0, -1.0);
  editorPanel->showAxes(0U);
  editorPanel->showAxesLabels(0U);
  _root->addChild(editorPanel);

  // Panel that will hold checkboxes with sphere options
  OpenFrames::QWidgetPanel *sphereOptionsPanel = new OpenFrames::QWidgetPanel("panel");
  sphereOptionsPanel->setColor(bgColor);
  sphereOptionsPanel->setHalfLengths(1.5, 1.2, 0.1);
  sphereOptionsPanel->setPosition(3.1, 0.0, -1.0);
  sphereOptionsPanel->setAttitude(-0.707106781186547, 0.0, 0.0, 0.707106781186547);
  sphereOptionsPanel->showAxes(0U);
  sphereOptionsPanel->showAxesLabels(0U);
  _root->addChild(sphereOptionsPanel);

  // Panel that will hold a list of colors
  OpenFrames::QWidgetPanel *colorPanel = new OpenFrames::QWidgetPanel("panel");
  colorPanel->setColor(bgColor);
  colorPanel->setHalfLengths(1.5, 1.2, 0.1);
  colorPanel->setPosition(-3.1, 0.0, -1.0);
  colorPanel->setAttitude(-0.707106781186547, 0.0, 0.0, 0.707106781186547);
  colorPanel->showAxes(0U);
  colorPanel->showAxesLabels(0U);
  _root->addChild(colorPanel);

  // Hidden panel that will hold sliders to move the sphere
  _hiddenPanel = new OpenFrames::QWidgetPanel("panel");
  _hiddenPanel->setColor(bgColor);
  _hiddenPanel->setHalfLengths(1.5, 1.2, 0.1);
  _hiddenPanel->setPosition(3.1, 0.0, 1.5);
  _hiddenPanel->setAttitude(-0.707106781186547, 0.0, 0.0, 0.707106781186547);
  _hiddenPanel->showAxes(0U);
  _hiddenPanel->showAxesLabels(0U);

  // Create the example controls widget
  QWidget *editorParentWidget = new QWidget;
  editorParentWidget->setLayout(new QVBoxLayout);
  QString text(LOREM_IPSUM_DOLOR);
  QTextEdit *textEdit = new QTextEdit(text);
  textEdit->setReadOnly(false);
  QPalette palette = textEdit->palette();
  palette.setColor(QPalette::Highlight, Qt::darkBlue);
  palette.setColor(QPalette::HighlightedText, Qt::white);
  textEdit->setPalette(palette);
  editorParentWidget->layout()->addWidget(textEdit);
  _toggleButton = new QPushButton("Hide Sphere");
  editorParentWidget->layout()->addWidget(_toggleButton);
  // If Qt's autocomputed preferred size is poor, then you can override it
  //editorParentWidget->setMinimumSize(QSize(400, 300));
  editorPanel->setWidget(editorParentWidget);
  editorPanel->setIgnoreWidget(editorParentWidget, true);

  // Create the example controls widget, built at compile time from the ui file
  QWidget *sphereOptionsWidget = new QWidget;
  Ui_SphereOptions ui;
  ui.setupUi(sphereOptionsWidget);
  _showCheckBox = sphereOptionsWidget->findChild<QCheckBox*>("showSphereUI");
  _showCheckBox->setChecked(true);
  QCheckBox *moveSphereCheckBox = sphereOptionsWidget->findChild<QCheckBox*>("moveSphereUI");
  sphereOptionsPanel->setWidget(sphereOptionsWidget);
  sphereOptionsPanel->setIgnoreWidget(sphereOptionsWidget, true);

  // Create the example list view
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
  colorPanel->setWidget(_list);

  // Create the example slider controls widget, loaded from ui file in resources at runtime
  QFile file(":/forms/movesphereform.ui");
  file.open(QIODevice::ReadOnly);
  QUiLoader loader;
  QWidget *moveSphereWidget = loader.load(&file, nullptr);
  QSlider *xSlider = moveSphereWidget->findChild<QSlider*>("xSliderUI");
  QSlider *ySlider = moveSphereWidget->findChild<QSlider*>("ySliderUI");
  QSlider *zSlider = moveSphereWidget->findChild<QSlider*>("zSliderUI");
  _hiddenPanel->setWidget(moveSphereWidget);
  _hiddenPanel->setIgnoreWidget(moveSphereWidget, true);

  // Connect QObject signals to our slots
  QObject::connect(moveSphereCheckBox, &QCheckBox::clicked, this, &OFControls::setHiddenPanel);
  QObject::connect(_toggleButton, &QPushButton::clicked, this, &OFControls::toggleSphere);
  QObject::connect(_showCheckBox, &QCheckBox::clicked, this, &OFControls::setSphere);
  QObject::connect(_list, &QListWidget::itemActivated, this, &OFControls::setColor);
  QObject::connect(_list, &QListWidget::itemClicked, this, &OFControls::setColor);
  QObject::connect(xSlider, &QSlider::valueChanged, this, &OFControls::setXLocation);
  QObject::connect(ySlider, &QSlider::valueChanged, this, &OFControls::setYLocation);
  QObject::connect(zSlider, &QSlider::valueChanged, this, &OFControls::setZLocation);

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

void OFControls::toggleSphere()
{
  _windowProxy->getGridPosition(0U, 0U)->getFrameManager()->lock();
  if (_root->getChildIndex(_sphere) >= 0)
    setSphere(false);
  else
    setSphere(true);
  _windowProxy->getGridPosition(0U, 0U)->getFrameManager()->unlock();
}

void OFControls::setSphere(bool checked)
{
  _windowProxy->getGridPosition(0U, 0U)->getFrameManager()->lock();
  if (!checked && _root->getChildIndex(_sphere) >= 0)
  {
    _showCheckBox->setChecked(false);
    _toggleButton->setText("Show Sphere");
    _root->removeChild(_sphere);
  }
  else if (checked && _root->getChildIndex(_sphere) < 0)
  {
    _showCheckBox->setChecked(true);
    _toggleButton->setText("Hide Sphere");
    _root->addChild(_sphere);
  }
  _windowProxy->getGridPosition(0U, 0U)->getFrameManager()->unlock();
}

void OFControls::setColor(QListWidgetItem *item)
{
  _sphere->setColor(COLORS.at(item->text().toStdString()));
}

void OFControls::setHiddenPanel(bool checked)
{
  _windowProxy->getGridPosition(0U, 0U)->getFrameManager()->lock();
  if (!checked && _root->getChildIndex(_hiddenPanel) >= 0)
    _root->removeChild(_hiddenPanel);
  else if (checked && _root->getChildIndex(_hiddenPanel) < 0)
    _root->addChild(_hiddenPanel);
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
