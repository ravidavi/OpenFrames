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

#include <OpenFrames/CurveArtist.hpp>
#include <OpenFrames/DrawableTrajectory.hpp>
#include <OpenFrames/FrameManager.hpp>
#include <OpenFrames/FrameTransform.hpp>
#include <OpenFrames/Model.hpp>
#include <OpenFrames/Trajectory.hpp>
#include <OpenFrames/TrajectoryFollower.hpp>
#include <OpenFrames/WindowProxy.hpp>

#include <osgParticle/ExplosionDebrisEffect>
#include <osgParticle/ExplosionEffect>
#include <osgParticle/FireEffect>
#include <osgParticle/SmokeTrailEffect>

using namespace OpenFrames;

const double pathRadius = 100.0;
osg::ref_ptr<WindowProxy> theWindow;
Model* theModel;

void createParticleEffects(Model *model);
void freezeParticleEffects(Model *model, bool shouldFreeze);

/** The function called when the user presses a key */
void KeyPressCallback(unsigned int *winID, unsigned int *row, unsigned int *col, int *key)
{
  // Pause/unpause animation
  if(*key == 'p')
  {
    bool shouldPause = !theWindow->isTimePaused();
    theWindow->pauseTime(shouldPause);
    freezeParticleEffects(theModel, shouldPause);
  }
  
  // Reset time to epoch. All ReferenceFrames that are following
  // a Trajectory will return to their starting positions.
  else if(*key == 'r')
  {
    theWindow->setTime(0.0);
    theWindow->getViewer()->getFrameStamp()->setSimulationTime(0.0); // Needed to properly reset particle systems
    createParticleEffects(theModel);
  }
  
  // Speed up time
  else if((*key == '+') || (*key == '='))
  {
    theWindow->setTimeScale(theWindow->getTimeScale() + 0.01);
  }
  
  // Slow down time
  else if((*key == '-') || (*key == '_'))
  {
    theWindow->setTimeScale(theWindow->getTimeScale() - 0.01);
  }
}

void freezeParticleEffects(Model *model, bool shouldFreeze)
{
  for(int i = 0; i < model->getExtras()->getNumChildren(); ++i)
  {
    osgParticle::ParticleEffect* effect = dynamic_cast<osgParticle::ParticleEffect*>(model->getExtras()->getDrawable(i));
    if(effect) effect->getParticleSystem()->setFrozen(shouldFreeze);
  }
}

void createParticleEffects(Model *model)
{
  // Remove existing effects
  for(int i = model->getTransform()->getNumChildren() - 1; i >= 0; --i)
  {
    osgParticle::ParticleEffect* effect = dynamic_cast<osgParticle::ParticleEffect*>(model->getTransform()->getChild(i));
    if(effect) model->getTransform()->removeChild(i);
  }
  model->getExtras()->removeChildren(0, model->getExtras()->getNumChildren());

  osg::Vec3d pos(8.0, 10.0, -3.0); // Position of right engine
  float scale = 5.0;
  float intensity = 1.0;

  // Create nice particle effects
  osgParticle::FireEffect *fire = new osgParticle::FireEffect(pos, scale, intensity);
  osgParticle::SmokeTrailEffect *smoke = new osgParticle::SmokeTrailEffect(pos, scale, intensity);
  osgParticle::ExplosionEffect *explosion = new osgParticle::ExplosionEffect(pos, 2.0*scale, 2.0*intensity);
  osgParticle::ExplosionDebrisEffect *explosionDebris = new osgParticle::ExplosionDebrisEffect(pos, 0.5*scale, 5.0*intensity);

  // Make effects start at specified time (seconds of simulation time)
  explosion->setStartTime(5.0);          // explosion ...
  explosionDebris->setStartTime(5.0);    // with debris ...
  fire->setStartTime(6.0);               // followed by fire ...
  smoke->setStartTime(6.0);              // and smoke

  // Make effects last for specified amount of time (seconds of simulation time)
  explosion->setEmitterDuration(1.0);
  explosionDebris->setEmitterDuration(0.1);
  fire->setEmitterDuration(10.0);
  smoke->setEmitterDuration(60.0);

  // Change default color for explosion debris particles
  osgParticle::Particle pTemplate = explosionDebris->getDefaultParticleTemplate();
  pTemplate.setColorRange(osgParticle::rangev4(
    osg::Vec4(158.0/255.0, 117.0/255.0, 78.0/255.0, 1.0f),   // Pale Brown
    osg::Vec4(179.0/255.0, 23.0/255.0, 34.0/255.0, 0.5f)));  // Red-Brown
  explosionDebris->setDefaultParticleTemplate(pTemplate);
  explosionDebris->setUpEmitterAndProgram();

  // Set wind for smoke trail (m/s in the global reference frame)
  smoke->setWind(osg::Vec3(0, 10, 0));

  // We'll handle adding the effect's particle system to scene graph ourselves
  // Necessary since the model moves w.r.t the root node
  fire->setUseLocalParticleSystem(false);
  smoke->setUseLocalParticleSystem(false);
  explosion->setUseLocalParticleSystem(false);
  explosionDebris->setUseLocalParticleSystem(false);
  
  // Add effects next to model so the particles will be emmitted from the correct location
  model->getTransform()->addChild(fire);
  model->getTransform()->addChild(smoke);
  model->getTransform()->addChild(explosion);
  model->getTransform()->addChild(explosionDebris);

  // Add particle systems above model so particles are independent after being emitted
  model->getExtras()->addDrawable(fire->getParticleSystem());
  model->getExtras()->addDrawable(smoke->getParticleSystem());
  model->getExtras()->addDrawable(explosion->getParticleSystem());
  model->getExtras()->addDrawable(explosionDebris->getParticleSystem());
}

/** 
 Demonstrates using OSG's particle system with a model in OpenFrames
 **/
int main()
{
  // Create the root frame
  ReferenceFrame* root = new ReferenceFrame("root");
  root->moveXAxis(osg::Vec3d(), pathRadius / 2.0);
  root->moveYAxis(osg::Vec3d(), pathRadius / 2.0);
  root->moveZAxis(osg::Vec3d(), pathRadius / 2.0);

  // Create a drawable trajectory to hold all trajectory artists
  // This will also act as the root of the reference frame hierarchy
  DrawableTrajectory *drawtraj = new DrawableTrajectory("drawtraj", 1, 0, 0, 0.9);
  drawtraj->showAxes(ReferenceFrame::NO_AXES);
  drawtraj->showAxesLabels(ReferenceFrame::NO_AXES);
  drawtraj->showNameLabel(false);
  root->addChild(drawtraj);

  // Create a circular trajectory that the model will follow
  osg::ref_ptr<Trajectory> traj = new Trajectory;
  {
    double pos[3];
    osg::Quat att;
    double rmag = 100.0;
    const double eps = 1.0e-10;
    for(double t = 0.0; t <= 2.0*osg::PI + eps; t += osg::PI / 90.0)
    {
      // Compute position along circle
      pos[0] = rmag * cos(t);
      pos[1] = rmag * sin(t);
      pos[2] = 0.0;

      // Compute orientation tangent to circle
      att.makeRotate(t, osg::Vec3d(0, 0, 1));

      // Add position
      traj->addTime(t);
      traj->addPosition(pos);
      traj->addAttitude(att.x(), att.y(), att.z(), att.w());
    }

    // Create a CurveArtist for the trajectory. By default the CurveArtist
    // will plot the trajectory's x/y/z positions.
    CurveArtist *ca = new CurveArtist(traj);
    ca->setWidth(2.0); // Line width for the trajectory
    ca->setColor(1, 0, 0);
    drawtraj->addArtist(ca);
  }

  // Load the model and have it follow the trajectory
  Model *cessna = new Model("cessna");
  {
    theModel = cessna;
    cessna->setModel("cessna.osg");
    cessna->getTransform()->setUpdateCallback(new TrajectoryFollower(traj));
    cessna->setModelAttitude(osg::Quat(osg::PI, osg::Vec3d(0, 0, 1))); // Model faces in -Y direction
    root->addChild(cessna);
  }

  // Create particle effects on the model
  createParticleEffects(cessna);
  
  // Create a window
  theWindow = new WindowProxy(30, 30, 1024, 768, 1, 1, false);
  theWindow->setKeyPressCallback(KeyPressCallback); // Specify keypress callback

  // Set up views
  View *view = new View(root, root);
  view->setDefaultViewParameters(osg::Vec3d(0, -200, 200), osg::Vec3d(), osg::Vec3d(0, 0, 1));
  View *view2 = new View(root, cessna);
  theWindow->getGridPosition(0, 0)->addView(view);
  theWindow->getGridPosition(0, 0)->addView(view2);

  // Create a manager to handle access to the scene
  FrameManager* fm = new FrameManager;
  fm->setFrame(root);
  
  // Add the scene to the window
  theWindow->setScene(fm, 0, 0);
  
  theWindow->startThread(); // Start window animation
  theWindow->join(); // Wait for window animation to finish
  
  return 0;
}
