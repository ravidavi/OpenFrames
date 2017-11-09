/* OpenSceneGraph example, osgtexture3D.
*
*  Permission is hereby granted, free of charge, to any person obtaining a copy
*  of this software and associated documentation files (the "Software"), to deal
*  in the Software without restriction, including without limitation the rights
*  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*  copies of the Software, and to permit persons to whom the Software is
*  furnished to do so, subject to the following conditions:
*
*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
*  THE SOFTWARE.
*/

#include "QWidgetImage.hpp"

#include <QPushButton>
#include <QVBoxLayout>

#include <osg/Node>
#include <osg/Geometry>
#include <osg/Notify>
#include <osg/Texture2D>
#include <osg/TexGen>
#include <osg/Geode>

#include <osgDB/Registry>
#include <osgDB/ReadFile>

#include <osgViewer/Viewer>
#include <osgViewer/ViewerEventHandlers>

#include <iostream>


// Thread that runs the viewer's frame loop as we can't run Qt in the background...
class ViewerFrameThread : public OpenThreads::Thread
{
    public:

        ViewerFrameThread(osgViewer::ViewerBase* viewerBase, bool doQApplicationExit):
            _viewerBase(viewerBase),
            _doQApplicationExit(doQApplicationExit) {}

        ~ViewerFrameThread()
        {
            cancel();
            while(isRunning())
            {
                OpenThreads::Thread::YieldCurrentThread();
            }
        }

        int cancel()
        {
            _viewerBase->setDone(true);
            return 0;
        }

        void run()
        {
            int result = _viewerBase->run();
            if (_doQApplicationExit) QApplication::exit(result);
        }

        osg::ref_ptr<osgViewer::ViewerBase> _viewerBase;
        bool _doQApplicationExit;
};


//
// A simple demo demonstrating use osg::Texture3D to create a blended animation between four separate images packed together into a 3d texture
//

osgViewer::InteractiveImageHandler* handler_;
osg::Geometry* geom_;
QPushButton *button_;

osg::StateSet* createState()
{
    getOrCreateQApplication();
    button_ = new QPushButton("Hello World", nullptr);
    QWidget* widget = new QWidget;                                                
    QVBoxLayout* layout = new QVBoxLayout( ) ;                                                 
    widget->setLayout(layout);                                                    
    widget->layout()->addWidget(button_); 
    widget->setGeometry(0, 0, 800, 600);

    osg::ref_ptr<QWidgetImage> widgetImage = new QWidgetImage(widget);
    widgetImage->getQGraphicsViewAdapter()->setBackgroundWidget(widget);
    widgetImage->getQWidget()->setAttribute(Qt::WA_TranslucentBackground);
    osg::Texture2D* texture2D = new osg::Texture2D(widgetImage.get());
    texture2D->setResizeNonPowerOfTwoHint(false);
    texture2D->setFilter(osg::Texture::MIN_FILTER,osg::Texture::LINEAR);            
    texture2D->setWrap(osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE);          
    texture2D->setWrap(osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE);

    // create the StateSet to store the texture data
    osg::StateSet* stateset = new osg::StateSet;
    stateset->setTextureAttributeAndModes(0,texture2D,osg::StateAttribute::ON);

    handler_ = new osgViewer::InteractiveImageHandler(widgetImage.get());

    stateset->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
    //stateset->setMode(GL_BLEND, osg::StateAttribute::ON);
    stateset->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
    //stateset->setAttribute(new osg::Program);

    return stateset;
}


class UpdateStateCallback : public osg::NodeCallback
{
    public:
        UpdateStateCallback() {}

        void animateState(osg::StateSet* stateset)
        {
            QCoreApplication::processEvents(QEventLoop::AllEvents, 100);
        }

        virtual void operator()(osg::Node* node, osg::NodeVisitor* nv)
        {

            osg::StateSet* stateset = node->getStateSet();
            if (stateset)
            {
                // we have an existing stateset, so lets animate it.
                animateState(stateset);
            }

            // note, callback is responsible for scenegraph traversal so
            // should always include call the traverse(node,nv) to ensure
            // that the rest of callbacks and the scene graph are traversed.
            traverse(node,nv);
        }
};

/** create 2,2 square with center at 0,0,0 and aligned along the XZ plan */
osg::Drawable* createSquare(float textureCoordMax=1.0f)
{
    // set up the Geometry.
    geom_ = new osg::Geometry;

    osg::Vec3Array* coords = new osg::Vec3Array(4);
    (*coords)[0].set(-1.0f,0.0f,1.0f);
    (*coords)[1].set(-1.0f,0.0f,-1.0f);
    (*coords)[2].set(1.0f,0.0f,-1.0f);
    (*coords)[3].set(1.0f,0.0f,1.0f);
    geom_->setVertexArray(coords);

    osg::Vec3Array* norms = new osg::Vec3Array(1);
    (*norms)[0].set(0.0f,-1.0f,0.0f);
    geom_->setNormalArray(norms, osg::Array::BIND_OVERALL);

    osg::Vec2Array* tcoords = new osg::Vec2Array(4);
    (*tcoords)[0].set(0.0f,textureCoordMax);
    (*tcoords)[1].set(0.0f,0.0f);
    (*tcoords)[2].set(textureCoordMax,0.0f);
    (*tcoords)[3].set(textureCoordMax,textureCoordMax);
    geom_->setTexCoordArray(0,tcoords);

    geom_->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS,0,4));

    return geom_;
}

osg::Node* createModel()
{
    // create the geometry of the model, just a simple 2d quad right now.
    osg::Geode* geode = new osg::Geode;

    geode->addDrawable(createSquare());

    //geode->setUpdateCallback(new UpdateStateCallback());

    geode->setStateSet(createState());

    geom_->setEventCallback(handler_);
    geom_->setCullCallback(handler_);

    return geode;
}


int main(int , char **)
{
    // construct the viewer.
    osgViewer::Viewer viewer;

    // create a model from the images and pass it to the viewer.
    viewer.setSceneData(createModel());
    viewer.setUpViewInWindow(50, 50, 640, 480, 2);

    if (0)
    {
        // run the frame loop, interleaving Qt and the main OSG frame loop
        while(!viewer.done())
        {
           // process Qt events - this handles both events and paints the browser image
           QCoreApplication::processEvents(QEventLoop::AllEvents, 100);
           viewer.frame();
        }
    }
    else if (0)
    {
        // create a thread to run the viewer's frame loop
        ViewerFrameThread viewerThread(&viewer, true);
        viewerThread.startThread();

        // now start the standard Qt event loop, then exists when the viewerThead sends the QApplication::exit() signal.
        return QApplication::exec();
    }
    else
    {
        viewer.getSceneData()->setUpdateCallback(new UpdateStateCallback());
        return viewer.run();
    }
}
