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

/** \file DistanceAccumulator.hpp
 * Declaration of DistanceAccumulator class.
 */

#ifndef _OF_DISTANCEACCUMULATOR_
#define _OF_DISTANCEACCUMULATOR_

#include <OpenFrames/Export.h>
#include <osg/Group>
#include <osg/NodeVisitor>
#include <osg/Polytope>
#include <osg/fast_back_stack>

namespace OpenFrames
{
  /**
   * \class DistanceAccumulator
   *
   * \brief This class computes distances to drawables and splits the scene if necessary.
   *
   * This class traverses the scene, computes the distance to each
   * visible drawable and splits up the scene if the latter are
   * too far away (in the z direction) from each other.
   */
  class OF_EXPORT DistanceAccumulator : public osg::NodeVisitor
  {
  public:
    typedef std::pair<double, double> DistancePair;
    typedef std::vector<DistancePair> PairList;
    
    DistanceAccumulator();
    
    // DistanceAccumulator considers these types of nodes
    virtual void apply(osg::Node &node);
    virtual void apply(osg::Projection &proj);
    virtual void apply(osg::Transform &transform);
    virtual void apply(osg::Geode &geode);
    
    // Specify the modelview & projection matrices
    void setMatrices(const osg::Matrix &modelview,
                     const osg::Matrix &projection);
    
    // Reset visitor before a new traversal
    virtual void reset();
    
    // Create a (near,far) distance pair for each camera of the specified
    // distance pair list and distance limits.
    void computeCameraPairs();
    
    // Get info on the cameras that should be used for scene rendering
    PairList& getCameraPairs() { return _cameraPairs; }
    
    // Get info on the computed distance pairs
    PairList& getDistancePairs() { return _distancePairs; }
    
    // Get info on the computed nearest/farthest distances
    DistancePair& getLimits() { return _limits; }
    
    // Set/get the desired near/far ratio
    void setNearFarRatio(double ratio);
    inline double getNearFarRatio() const { return _nearFarRatio; }
    
    // Set the max traversal depth into the scene graph
    inline void setMaxDepth(unsigned int depth) { _maxDepth = depth; }
    inline unsigned int getMaxDepth() const { return _maxDepth; }
    
    // Set the minimum allowable near plane. Zero means no minimum.
    inline void setMinZNear(double minZNear) { if(minZNear >= 0.0) _minZNear = minZNear; }
    inline double getMinZNear() { return _minZNear; }
    
  protected:
    virtual ~DistanceAccumulator();
    
    void pushLocalFrustum();
    void pushDistancePair(double zNear, double zFar);
    bool shouldContinueTraversal(osg::Node &node);
    
    // Stack of matrices accumulated during traversal
    osg::fast_back_stack<osg::Matrix> _viewMatrices;
    osg::fast_back_stack<osg::Matrix> _projectionMatrices;
    
    // Main modelview/projection matrices
    osg::Matrix _modelview, _projection;
    
    // The view frusta in local coordinate space
    osg::fast_back_stack<osg::Polytope> _localFrusta;
    
    // Bounding box corners that should be used for cull computation
    typedef std::pair<unsigned int, unsigned int> bbCornerPair;
    osg::fast_back_stack<bbCornerPair> _bbCorners;
    
    // Nar/far planes that should be used for each camera
    PairList _cameraPairs;
    
    // Accumulated pairs of min/max distances
    PairList _distancePairs;
    
    // The closest & farthest distances found while traversing
    DistancePair _limits;
    
    // Ratio of nearest/farthest clip plane for each section of the scene
    double _nearFarRatio;
    
    // Maximum depth to traverse to
    unsigned int _maxDepth, _currentDepth;
    
    // Minimum zNear value
    double _minZNear;
  };
  
}

#endif
