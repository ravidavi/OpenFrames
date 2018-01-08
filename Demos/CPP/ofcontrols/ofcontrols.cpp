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

#include <OpenFrames/WindowProxy.hpp>
#include <OpenFrames/Sphere.hpp>

#include <QApplication>
#include <QWidget>
#include <QVBoxLayout>
#include <QTextEdit>
#include <QPushButton>

/// The period of event checking in the main loop (milliseconds)
static const double MAIN_LOOP_PERIOD = 100;

int main(int argc, char **argv)
{
  osg::setNotifyLevel(osg::INFO);

  // Qt requires that we construct the global QApplication before creating any widgets.
  QApplication app(argc, argv);

  // Instantiate and configure OpenFrames objects
  OpenFrames::WindowProxy *winProxy = new OpenFrames::WindowProxy(100, 100, 640, 480, 1, 1, false, false);

  OpenFrames::ReferenceFrame *root = new OpenFrames::ReferenceFrame("root");
  root->showNameLabel(false);
  root->showAxes(0U);
  root->showAxesLabels(0U);

  OpenFrames::ControlPanel *panel = new OpenFrames::ControlPanel("panel");
  panel->setColor(0.8, 0.8, 0.8, 1.0);
  panel->setHalfLengths(1.5, 1.2, 0.1);
  panel->setPosition(0.0, 1.5, 0.0);
  panel->setAttitude(0.0, 0.707106781186547, -0.707106781186547, 0.0);
  panel->showAxes(7U);
  panel->showAxesLabels(7U);
  root->addChild(panel);

  OpenFrames::ControlPanel *panel1 = new OpenFrames::ControlPanel("panel");
  panel1->setColor(0.8, 0.8, 0.8, 1.0);
  panel1->setHalfLengths(1.5, 1.2, 0.1);
  panel1->setAttitude(-0.707106781186547, 0.0, 0.0, 0.707106781186547);
  panel1->setPosition(0.0, -1.5, 0.0);
  panel1->showAxes(7U);
  panel1->showAxesLabels(7U);
  root->addChild(panel1);

  OpenFrames::ControlPanel *panel2 = new OpenFrames::ControlPanel("panel");
  panel2->setColor(0.8, 0.8, 0.8, 1.0);
  panel2->setHalfLengths(1.5, 1.2, 0.1);
  panel2->setPosition(1.5, 0.0, 0.0);
  panel2->setAttitude(-0.5, -0.5, 0.5, 0.5);
  panel2->showAxes(7U);
  panel2->showAxesLabels(7U);
  root->addChild(panel2);

  OpenFrames::ControlPanel *panel3 = new OpenFrames::ControlPanel("panel");
  panel3->setColor(0.8, 0.8, 0.8, 1.0);
  panel3->setHalfLengths(1.5, 1.2, 0.1);
  panel3->setPosition(-1.5, 0.0, 0.0);
  panel3->setAttitude(-0.5, 0.5, -0.5, 0.5);
  panel3->showAxes(7U);
  panel3->showAxesLabels(7U);
  root->addChild(panel3);

  OpenFrames::Sphere *sphere = new OpenFrames::Sphere("sphere");
  sphere->setPosition(0.0, 0.0, 1.5);
  sphere->showNameLabel(false);
  sphere->showAxes(7U);
  sphere->showAxesLabels(7U);
  sphere->setRadius(1.0);
  root->addChild(sphere);

  if (true)
  {
    // Create the example controls widget
    QWidget* widget = new QWidget;
    widget->setLayout(new QVBoxLayout);
    QString text("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque velit turpis, euismod ac ultrices "
                 "et, molestie non nisi. Nullam egestas dignissim enim, quis placerat nulla suscipit sed. Donec "
                 "molestie elementum risus sit amet sodales. Nunc consectetur congue neque, at viverra massa pharetra "
                 "fringilla. Integer vitae mi sem. Donec dapibus semper elit nec sollicitudin. Vivamus egestas "
                 "ultricies felis, in mollis mi facilisis quis. Nam suscipit bibendum eros sed cursus. Suspendisse "
                 "mollis suscipit hendrerit. Etiam magna eros, convallis non congue vel, faucibus ac augue. Integer "
                 "ante ante, porta in ornare ullamcorper, congue nec nibh. Etiam congue enim vitae enim sollicitudin "
                 "fringilla. Mauris mattis, urna in fringilla dapibus, ipsum sem feugiat purus, ac hendrerit felis "
                 "arcu sed sapien. Integer id velit quam, sit amet dignissim tortor. Sed mi tortor, placerat ac "
                 "luctus id, tincidunt et urna. Nulla sed nunc ante.Sed ut sodales enim. Ut sollicitudin ultricies "
                 "magna, vel ultricies ante venenatis id. Cras luctus mi in lectus rhoncus malesuada. Sed ac "
                 "sollicitudin nisi. Nunc venenatis congue quam, et suscipit diam consectetur id. Donec vel enim ac "
                 "enim elementum bibendum ut quis augue. Nulla posuere suscipit dolor, id convallis tortor congue "
                 "eu. Vivamus sagittis consectetur dictum. Duis a ante quis dui varius fermentum. In hac habitasse "
                 "platea dictumst. Nam dapibus dolor eu felis eleifend in scelerisque dolor ultrices. Donec arcu "
                 "lectus, fringilla ut interdum non, tristique id dolor. Morbi sagittis sagittis volutpat. "
                 "Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Duis "
                 "venenatis ultrices euismod.Nam sit amet convallis libero. Integer lectus urna, eleifend et "
                 "sollicitudin non, porttitor vel erat. Vestibulum pulvinar egestas leo, a porttitor turpis "
                 "ullamcorper et. Vestibulum in ornare turpis. Ut nec libero a sem mattis iaculis quis id purus. "
                 "Praesent ante neque, dictum vitae pretium vel, iaculis luctus dui. Etiam luctus tellus vel nunc "
                 "suscipit a ullamcorper nisl semper. Nunc dapibus, eros in sodales dignissim, orci lectus egestas "
                 "felis, sit amet vehicula tortor dolor eu quam. Vivamus pellentesque convallis quam aliquet "
                 "pellentesque. Phasellus facilisis arcu ac orci fringilla aliquet. Donec sed euismod augue. Duis eget "
                 "orci sit amet neque tempor fringilla. Vestibulum ante ipsum primis in faucibus orci luctus et "
                 "ultrices posuere cubilia Curae; In hac habitasse platea dictumst. Duis sollicitudin, lacus ac "
                 "pellentesque lacinia, lacus magna pulvinar purus, pulvinar porttitor est nibh quis augue.Duis "
                 "eleifend, massa sit amet mattis fringilla, elit turpis venenatis libero, sed convallis turpis diam "
                 "sit amet ligula. Morbi non dictum turpis. Integer porttitor condimentum elit, sit amet sagittis nibh "
                 "ultrices sit amet. Mauris ac arcu augue, id aliquet mauris. Donec ultricies urna id enim accumsan at "
                 "pharetra dui adipiscing. Nunc luctus rutrum molestie. Curabitur libero ipsum, viverra at pulvinar "
                 "ut, porttitor et neque. Aliquam sit amet dolor et purus sagittis adipiscing. Nam sit amet hendrerit "
                 "sem. Etiam varius, ligula non ultricies dignissim, sapien dui commodo urna, eu vehicula enim nunc "
                 "molestie augue. Fusce euismod, erat vitae pharetra tempor, quam eros tincidunt lorem, ut iaculis "
                 "ligula erat vitae nibh. Aenean eu ultricies dolor. Curabitur suscipit viverra bibendum.Sed egestas "
                 "adipiscing mi in egestas. Proin in neque in nibh blandit consequat nec quis tortor. Vestibulum sed "
                 "interdum justo. Sed volutpat velit vitae elit pulvinar aliquam egestas elit rutrum. Proin lorem "
                 "nibh, bibendum vitae sollicitudin condimentum, pulvinar ut turpis. Maecenas iaculis, mauris in "
                 "consequat ultrices, ante erat blandit mi, vel fermentum lorem turpis eget sem. Integer ultrices "
                 "tristique erat sit amet volutpat. In sit amet diam et nunc congue pellentesque at in dolor. Mauris "
                 "eget orci orci. Integer posuere augue ornare tortor tempus elementum. Quisque iaculis, nunc ac "
                 "cursus fringilla, magna elit cursus eros, id feugiat diam eros et tellus. Etiam consectetur "
                 "ultrices erat quis rhoncus. Mauris eu lacinia neque. Curabitur suscipit feugiat tellus in dictum. "
                 "Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Sed "
                 "aliquam tempus ante a tempor. Praesent viverra erat quis sapien pretium rutrum. Praesent dictum "
                 "scelerisque venenatis.Proin bibendum lectus eget nisl lacinia porta. Morbi eu erat in sapien "
                 "malesuada vulputate. Cras non elit quam. Ut dictum urna quis nisl feugiat ac sollicitudin libero "
                 "luctus. Donec leo mauris, varius at luctus eget, placerat quis arcu. Vestibulum ante ipsum primis in "
                 "faucibus orci luctus et ultrices posuere cubilia Curae; Etiam tristique, mauris ut lacinia "
                 "elementum, mauris erat consequat massa, ac gravida nisi tellus vitae purus. Curabitur consectetur "
                 "ultricies commodo. Cras pulvinar orci nec enim adipiscing tristique. Ut ornare orci id est fringilla "
                 "sit amet blandit libero pellentesque. Vestibulum tincidunt sapien ut enim venenatis vestibulum "
                 "ultricies ipsum tristique. Mauris tempus eleifend varius. Lorem ipsum dolor sit amet, consectetur "
                 "adipiscing elit. Suspendisse vitae dui ac quam gravida semper. In ac enim ac ligula rutrum porttitor."
                 "Integer dictum sagittis leo, at convallis sapien facilisis eget. Etiam cursus bibendum tortor, "
                 "faucibus aliquam lectus ullamcorper sed. Nulla pulvinar posuere quam, ut sagittis ligula tincidunt "
                 "ut. Nulla convallis velit ut enim condimentum pulvinar. Quisque gravida accumsan scelerisque. Proin "
                 "pellentesque nisi cursus tortor aliquet dapibus. Duis vel eros orci. Sed eget purus ligula. Lorem "
                 "ipsum dolor sit amet, consectetur adipiscing elit. Donec ullamcorper porta congue. Nunc id velit ut "
                 "neque malesuada consequat in eu nisi. Nulla facilisi. Quisque pellentesque magna vitae nisl euismod "
                 "ac accumsan tellus feugiat.Nulla facilisi. Integer quis orci lectus, non aliquam nisi. Vivamus "
                 "varius porta est, ac porttitor orci blandit mattis. Sed dapibus facilisis dapibus. Duis tincidunt "
                 "leo ac tortor faucibus hendrerit. Morbi sit amet sapien risus, vel luctus enim. Aliquam sagittis "
                 "nunc id purus aliquam lobortis. Duis posuere viverra dui, sit amet convallis sem vulputate at. Lorem "
                 "ipsum dolor sit amet, consectetur adipiscing elit. Quisque pellentesque, lectus id imperdiet "
                 "commodo, diam diam faucibus lectus, sit amet vestibulum tortor lacus viverra eros.Maecenas nec augue "
                 "lectus. Duis nec arcu eget lorem tempus sollicitudin suscipit vitae arcu. Nullam vitae mauris "
                 "lectus. Vivamus id risus neque, dignissim vehicula diam. Cras rhoncus velit sed velit iaculis ac "
                 "dignissim turpis luctus. Suspendisse potenti. Sed vitae ligula a ligula ornare rutrum sit amet ut "
                 "quam. Duis tincidunt, nibh vitae iaculis adipiscing, dolor orci cursus arcu, vel congue tortor quam "
                 "eget arcu. Suspendisse tellus felis, blandit ac accumsan vitae, fringilla id lorem. Duis tempor "
                 "lorem mollis est congue ut imperdiet velit laoreet. Nullam interdum cursus mollis. Pellentesque non "
                 "mauris accumsan elit laoreet viverra ut at risus. Proin rutrum sollicitudin sem, vitae ultricies "
                 "augue sagittis vel. Cras quis vehicula neque. Aliquam erat volutpat. Aliquam erat volutpat. Praesent "
                 "non est erat, accumsan rutrum lacus. Pellentesque tristique molestie aliquet. Cras ullamcorper "
                 "facilisis faucibus. In non lorem quis velit lobortis pulvinar.Phasellus non sem ipsum. Praesent ut "
                 "libero quis turpis viverra semper. Class aptent taciti sociosqu ad litora torquent per conubia "
                 "nostra, per inceptos himenaeos. In hac habitasse platea dictumst. Donec at velit tellus. Fusce "
                 "commodo pharetra tincidunt. Proin lacus enim, fringilla a fermentum ut, vestibulum ut nibh. Duis "
                 "commodo dolor vel felis vehicula at egestas neque bibendum. Phasellus malesuada dictum ante in "
                 "aliquam. Curabitur interdum semper urna, nec placerat justo gravida in. Praesent quis mauris massa. "
                 "Pellentesque porttitor lacinia tincidunt. Phasellus egestas viverra elit vel blandit. Sed dapibus "
                 "nisi et lectus pharetra dignissim. Mauris hendrerit lectus nec purus dapibus condimentum. Sed ac "
                 "eros nulla. Aenean semper sapien a nibh aliquam lobortis. Aliquam elementum euismod sapien, in "
                 "dapibus leo dictum et. Pellentesque augue neque, ultricies non viverra eu, tincidunt ac arcu. Morbi "
                 "ut porttitor lectus.");
    QTextEdit* textEdit = new QTextEdit(text);
    textEdit->setReadOnly(false);
    textEdit->setTextInteractionFlags(Qt::TextEditorInteraction);
    QPalette palette = textEdit->palette();
    palette.setColor(QPalette::Highlight, Qt::darkBlue);
    palette.setColor(QPalette::HighlightedText, Qt::white);
    textEdit->setPalette(palette);
    widget->layout()->addWidget(textEdit);
    QPushButton* button = new QPushButton("Button");
    widget->layout()->addWidget(button);
    widget->setGeometry(0, 0, 600, 800);

    panel->setWidgetControls(widget);
  }
  /*else
  {
    panel->setQuickControls(QUrl(QStringLiteral("qrc:/main.qml")));
  }*/

  OpenFrames::FrameManager *frameManager = new OpenFrames::FrameManager();
  frameManager->setFrame(root);
  winProxy->setScene(frameManager, 0U, 0U);
  winProxy->getGridPosition(0U, 0U)->addView(new OpenFrames::View(root, root, OpenFrames::View::ABSOLUTE_FRAME));
  winProxy->getGridPosition(0U, 0U)->addView(new OpenFrames::View(root, panel, OpenFrames::View::ABSOLUTE_FRAME));

  // Run OpenFrames
  OpenFrames::FramerateLimiter waitLimiter;
  waitLimiter.setDesiredFramerate(1000.0/MAIN_LOOP_PERIOD); // FPS at which to check events

  OSG_INFO << "starting window proxy" << std::endl;
  winProxy->start();
  while (winProxy->isRunning())
  {
    // check events periodically
    QCoreApplication::processEvents(QEventLoop::AllEvents, MAIN_LOOP_PERIOD);
	  waitLimiter.frame();
  }

  return 0;
}
