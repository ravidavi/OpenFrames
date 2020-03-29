/***********************************
   Copyright 2020 Ravishankar Mathur

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

#include "renderthread.h"
#include <QWindow>
#include <QMutex>
#include <QOpenGLShaderProgram>
#include <QOpenGLContext>
#include <QCoreApplication>

#include <iostream>
#define _USE_MATH_DEFINES
#include <math.h>

#include <OpenFrames/CurveArtist.hpp>
#include <OpenFrames/DrawableTrajectory.hpp>
#include <OpenFrames/FrameManager.hpp>
#include <OpenFrames/MarkerArtist.hpp>
#include <OpenFrames/RadialPlane.hpp>
#include <OpenFrames/SegmentArtist.hpp>
#include <OpenFrames/TrajectoryFollower.hpp>

RenderThread::RenderThread(QObject *parent)
    : QThread(parent),
      m_window(0x0),
      m_context(0x0),
      m_winproxy(0x0),
      m_spacestation(0x0),
      m_axes(0x0),
      m_stereo(false)
{
    // Create embedded WindowProxy that handles all OpenFrames drawing
    m_winproxy = new OpenFrames::WindowProxy(0, 0, 450, 300, 2, 1, true);
    m_winproxy->setID(0);
    m_winproxy->setMakeCurrentFunction(OFRenderPool::dealMakeCurrent);
    m_winproxy->setUpdateContextFunction(OFRenderPool::dealMakeCurrent);
    m_winproxy->setSwapBuffersFunction(OFRenderPool::dealSwapBuffers);
    m_winproxy->setTimeScale(1.0);
    
    // Create the models that will populate the scene using
    // Model(name, color(r,g,b,a))
    m_spacestation = new OpenFrames::Model("Space Station", 0, 1, 0, 0.9);
    OpenFrames::Model *hubble = new OpenFrames::Model("Hubble", 1, 0, 0, 0.9);

    // Set the 3D models
    m_spacestation->setModel("../Models/SpaceStation.3ds");
    hubble->setModel("../Models/Hubble.3ds");

    // Create the trajectory using
    // Trajectory(DOF, number of optionals)
    OpenFrames::Trajectory *traj = new OpenFrames::Trajectory(3, 1);

    // Create a drawable trajectory for the spacecraft window using
    // DrawableTrajectory(name, color(r,g,b,a))
    OpenFrames::DrawableTrajectory *drawtraj = new OpenFrames::DrawableTrajectory("traj", 1, 0, 0, 0.9);
    drawtraj->showAxes(OpenFrames::ReferenceFrame::NO_AXES);
    drawtraj->showAxesLabels(OpenFrames::ReferenceFrame::NO_AXES);
    drawtraj->showNameLabel(false);

    // Create a CurveArtist for the trajectory.  By default the CurveArtist
    // will use x/y/z positions from the trajectory for plotting.
    OpenFrames::CurveArtist *ca = new OpenFrames::CurveArtist(traj);
    ca->setWidth(2.0); // Line width for the trajectory
    ca->setColor(1, 0, 0);
    drawtraj->addArtist(ca);

    OpenFrames::Trajectory::DataSource data; // Data source for artists
    data._src = OpenFrames::Trajectory::POSOPT;

    // Create an artist for velocity vectors
    OpenFrames::SegmentArtist *sa = new OpenFrames::SegmentArtist(traj);
    sa->setColor(0.5, 0, 0);
    data._element = 0;
    sa->setStartXData(data);
    data._element = 1;
    sa->setStartYData(data);
    data._element = 2;
    sa->setStartZData(data);
    data._element = 0;
    data._opt = 1;
    sa->setEndXData(data);
    data._element = 1;
    sa->setEndYData(data);
    data._element = 2;
    sa->setEndZData(data);
    drawtraj->addArtist(sa);

    // Create a drawable trajectory for the time history window
    OpenFrames::DrawableTrajectory *timehist = new OpenFrames::DrawableTrajectory("TimeHist", 1, 0, 0, 0.9);
    timehist->showAxes(OpenFrames::ReferenceFrame::NO_AXES);
    timehist->showAxesLabels(OpenFrames::ReferenceFrame::NO_AXES);
    timehist->showNameLabel(false);

    // Create an artist to draw position vs time
    ca = new OpenFrames::CurveArtist(traj);
    ca->setColor(1, 0, 0);
    data._src = OpenFrames::Trajectory::POSOPT;
    data._opt = 0;
    data._element = 0; // Use X position for X coordinate
    data._scale = 0.01; // Scale down since positions are large
    ca->setXData(data);
    data._element = 2; // Use Z position for Y coordinate
    ca->setYData(data);
    data._src = OpenFrames::Trajectory::TIME; // Use time for Z coordinate
    data._scale = 1.0; // Don't scale time
    ca->setZData(data);
    timehist->addArtist(ca);

    // Create an artist to draw start/intermediate/end markers
    OpenFrames::MarkerArtist *ma = new OpenFrames::MarkerArtist(traj);
    ma->setMarkers(OpenFrames::MarkerArtist::START + OpenFrames::MarkerArtist::INTERMEDIATE + OpenFrames::MarkerArtist::END);
    ma->setAutoAttenuate(true);
    ma->setMarkerColor(OpenFrames::MarkerArtist::START, 0, 1, 0);
    ma->setMarkerColor(OpenFrames::MarkerArtist::END, 1, 0, 0);
    ma->setMarkerColor(OpenFrames::MarkerArtist::INTERMEDIATE, 1, 1, 0);
    ma->setMarkerImage("../Images/fuzzyparticle.tiff");
    ma->setMarkerSize(10);
    data._src = OpenFrames::Trajectory::POSOPT;
    data._element = 0; // Use X position for X coordinate
    data._scale = 0.01; // Scale down since positions are large
    ma->setXData(data);
    data._element = 2; // Use Z position for Y coordinate
    ma->setYData(data);
    data._src = OpenFrames::Trajectory::TIME; // Use time for Z coordinate
    data._scale = 1.0; // Don't scale time
    ma->setZData(data);
    timehist->addArtist(ma);

    // Draw markers at equally spaced distances
    ma->setIntermediateType(OpenFrames::MarkerArtist::DISTANCE);
    ma->setIntermediateSpacing(10.0); // Every 10 distance units
    ma->setIntermediateDirection(OpenFrames::MarkerArtist::END); // From end of trajectory

    // Create a set of Coordinate Axes for time history plot
    m_axes = new OpenFrames::CoordinateAxes("axes", 0.0, 0.8, 0.8, 1);
    m_axes->setAxisLength(2.0*M_PI);
    m_axes->setTickSpacing(M_PI, 0.25*M_PI);
    m_axes->setTickSize(8, 5);
    m_axes->setTickImage("../Images/circle.tiff");
    m_axes->setXLabel("X");
    m_axes->setYLabel("Z");
    m_axes->setZLabel("t");

    // Create a ReferenceFrame to show model's position in time history plot
    OpenFrames::ReferenceFrame *trace = new OpenFrames::ReferenceFrame("trace", 1, 1, 1, 1);

    // Create a drawable trajectory to hold the trace center marker
    OpenFrames::DrawableTrajectory *drawcenter = new OpenFrames::DrawableTrajectory("center marker", 1, 0, 0, 1);
    drawcenter->showAxes(OpenFrames::ReferenceFrame::NO_AXES);
    drawcenter->showAxesLabels(OpenFrames::ReferenceFrame::NO_AXES);
    drawcenter->showNameLabel(false);

    // Create a MarkerArtist to draw the center marker
    OpenFrames::MarkerArtist *centermarker = new OpenFrames::MarkerArtist();
    centermarker->setMarkerImage("../Images/target.tiff");
    centermarker->setMarkerSize(10);

    // Add the markerartist to the drawable trajectory
    drawcenter->addArtist(centermarker);

    // Create a RadialPlane to show trace frame's orientation
    OpenFrames::RadialPlane *rp = new OpenFrames::RadialPlane("radial", 1, 1, 1, 1);
    rp->showAxes(OpenFrames::ReferenceFrame::NO_AXES);
    rp->showAxesLabels(OpenFrames::ReferenceFrame::NO_AXES);
    rp->showNameLabel(false);
    rp->setParameters(10.0, 2.5, 60.0*M_PI / 180.0);

    // Set up reference frame heirarchies.
    m_spacestation->addChild(drawtraj);
    m_spacestation->addChild(hubble);
    m_axes->addChild(timehist);
    m_axes->addChild(trace);
    trace->addChild(drawcenter);
    m_axes->addChild(rp);

    // Tell model to follow trajectory (by default in LOOP mode)
    OpenFrames::TrajectoryFollower *tf = new OpenFrames::TrajectoryFollower(traj);
    hubble->getTransform()->setUpdateCallback(tf);

    // Tell trace frame to follow time history
    tf = new OpenFrames::TrajectoryFollower(traj);
    data._src = OpenFrames::Trajectory::POSOPT;
    data._opt = 0;
    data._element = 0; // Use X position for X coordinate
    data._scale = 0.01; // Scale down since positions are large
    tf->setXData(data);
    data._element = 2; // Use Z position for Y coordinate
    tf->setYData(data);
    data._src = OpenFrames::Trajectory::TIME; // Use time for Z coordinate
    data._scale = 1.0;
    tf->setZData(data);
    trace->getTransform()->setUpdateCallback(tf);

    // Tell radial frame to follow time history's orientation
    tf = new OpenFrames::TrajectoryFollower(traj);
    tf->setFollowType(OpenFrames::TrajectoryFollower::ATTITUDE, OpenFrames::TrajectoryFollower::LOOP);
    rp->getTransform()->setUpdateCallback(tf);
    rp->setPosition(0.0, 0.0, 0.0);

    // Create views
    m_views[0][0] = new OpenFrames::View(m_spacestation, m_spacestation);
    m_views[0][1] = new OpenFrames::View(m_spacestation, hubble);
    m_views[1][0] = new OpenFrames::View(m_axes, m_axes);
    m_views[1][1] = new OpenFrames::View(m_axes, trace);

    // Create a manager to handle the spatial scene
    OpenFrames::FrameManager* fm = new OpenFrames::FrameManager;
    fm->setFrame(m_spacestation);

    // Create a manager to handle the time history scene
    OpenFrames::FrameManager* fm2 = new OpenFrames::FrameManager;
    fm2->setFrame(m_axes);

    // Set up the scene
    m_winproxy->setScene(fm, 0, 0);
    m_winproxy->setScene(fm2, 1, 0);
    m_winproxy->getGridPosition(0, 0)->setSkySphereTexture("../Images/StarMap.tif");
    m_winproxy->getGridPosition(0, 0)->addView(m_views[0][0]);
    m_winproxy->getGridPosition(0, 0)->addView(m_views[0][1]);
    m_winproxy->getGridPosition(1, 0)->addView(m_views[1][0]);
    m_winproxy->getGridPosition(1, 0)->addView(m_views[1][1]);

    // Add the actual positions and attitudes for the trajectory.
    osg::Quat att; // Quaternion for attitude transformations
    double pos[3], vel[3];
    pos[1] = vel[1] = 0.0;
    for (double t = 0.0; t <= 2.0*M_PI; t += M_PI / 90.0)
    {
        pos[0] = 500.0*sin(t);
        pos[2] = 500.0*cos(t);
        vel[0] = pos[0] + 0.5*pos[2];
        vel[2] = pos[2] - 0.5*pos[0];
        att.makeRotate(t, 0, 1, 0);

        traj->addTime(t);
        traj->addPosition(pos);
        traj->setOptional(0, vel);
        traj->addAttitude(att[0], att[1], att[2], att[3]);
    }

    // Specify the key press callback
    m_winproxy->setKeyPressCallback(OFRenderPool::dealKeyPressCallback);
}

RenderThread::~RenderThread()
{
    // Wait for the window proxy to shutdown before deleting the context
    if (m_context != 0x0) {
        delete m_context;
        m_context = 0x0;
    }
    // else No m_context to delete
}

void RenderThread::begin(QWindow *w)
{
    if (w != 0x0) {
        m_window = w;
        start();
    }
    // else Do not start
}

void RenderThread::end()
{
    m_winproxy->shutdown();
    wait();
}

void RenderThread::run()
{
    bool success = false;

    if (m_context == 0x0) {
        if (m_window != 0x0) {
            m_context = new QOpenGLContext();
            m_context->create();
            success = m_context->makeCurrent(m_window);
            if (success) {
                initializeOpenGLFunctions();
                m_context->doneCurrent();
            }
        }
    }
    // else Continue using the old context

    if (success) {
        m_winproxy->run();
    }
    // else Had an error, do not start running
}

void RenderThread::keyPressCallback(int key)
{
    if (key == 'p') {
        // Pause/unpause animation
        m_winproxy->pauseTime(!m_winproxy->isTimePaused());
    }
    else if (key == 'r') {
        // Reset time to epoch. All ReferenceFrames that are following
        // a Trajectory will return to their starting positions.
      m_winproxy->setTime(0.0);
    }
    else if ((key == Qt::Key_Plus) || (key == Qt::Key_Equal)) {
        // Speed up time
        m_winproxy->setTimeScale(m_winproxy->getTimeScale() + 0.1);
    }
    else if ((key == Qt::Key_Minus) || (key == Qt::Key_Underscore)) {
        // Slow down time
        m_winproxy->setTimeScale(m_winproxy->getTimeScale() - 0.1);
    }
    else if (key == Qt::Key_Right) {
        // Shift time forward
        m_winproxy->setTime(m_winproxy->getTime() + 0.1);
    }
    else if (key == Qt::Key_Left) {
        // Shift time backward
        m_winproxy->setTime(m_winproxy->getTime() - 0.1);
    }
    else {
        // Ignore other keys, but view may have changed
        int upperIndex = 0;
        int lowerIndex = 0;
        if (m_winproxy != 0x0) {
            OpenFrames::View *currentView = m_winproxy->getGridPosition(0, 0)->getCurrentView();
            for (int ii = 0; ii < 2; ii++) {
                if (currentView == m_views[0][ii]) {
                    upperIndex = ii;
                    break;
                }
            }
            currentView = m_winproxy->getGridPosition(1, 0)->getCurrentView();
            for (int ii = 0; ii < 2; ii++) {
                if (currentView == m_views[1][ii]) {
                    lowerIndex = ii;
                    break;
                }
            }
            userSelectedView(upperIndex, lowerIndex);
        }
    }
}

bool RenderThread::makeCurrent()
{
    bool success;

    if (m_context != 0x0 && m_window != 0x0) {
        success = m_context->makeCurrent(m_window);

        // check OpenGL errors
        GLenum err;
        while ((err = glGetError()) != GL_NO_ERROR) {
            std::cout << "OpenGL error: " << err << std::endl;
        }
    }
    // else Cannot make current unless initialized properly

    return success;
}

void RenderThread::swapBuffers()
{
    if (m_context != 0x0 && m_window != 0x0) {
        // call swapbuffers
        m_context->swapBuffers(m_window);
    }
    // else Cannot swap buffers unless initialized properly
}
