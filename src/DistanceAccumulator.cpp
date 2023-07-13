/***********************************
 Copyright 2023 Ravishankar Mathur
 
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

/** \file DistanceAccumulator.cpp
 * Definitions for the DistanceAccumulator class.
 */

#include <OpenFrames/DistanceAccumulator.hpp>
#include <osg/Geode>
#include <osg/Transform>
#include <osg/Projection>
#include <algorithm> // Needed for std::swap on Win32
#include <climits>

namespace OpenFrames
{
  /** Determine whether one DistancePair should come before another in
   a sorted list. Used to sort the vector of DistancePairs. */
  bool precedes(const DistanceAccumulator::DistancePair &a,
                const DistanceAccumulator::DistancePair &b)
  {
    // This results in sorting in order of descending far distances
    if(a.second > b.second) return true;
    else return false;
  }
  
  /** Computes distance (in z direction) betwen a point and the viewer's eye,
   given by a view matrix */
  double distance(const osg::Vec3d &coord, const osg::Matrix& matrix)
  {
    // Here we are taking only the z coordinate of the point transformed
    // by the matrix, ie coord*matrix. The negative sign is because we
    // want to consider into the screen as INCREASING distance.
    return -( coord[0]*matrix(0,2) + coord[1]*matrix(1,2) +
             coord[2]*matrix(2,2) + matrix(3,2) );
  }
  
  DistanceAccumulator::DistanceAccumulator()
  : osg::NodeVisitor(TRAVERSE_ALL_CHILDREN),
  _nearFarRatio(0.0005), _maxDepth(UINT_MAX), _minZNear(1.0e-5)
  {
    setMatrices(osg::Matrix::identity(), osg::Matrix::identity());
    reset();
  }
  
  DistanceAccumulator::~DistanceAccumulator() {}
  
  void DistanceAccumulator::pushLocalFrustum()
  {
    osg::Matrix& currMatrix = _viewMatrices.back();
    
    // Compute the frustum in local space
    osg::Polytope localFrustum;
    localFrustum.setToUnitFrustum(false, false);
    localFrustum.transformProvidingInverse(currMatrix*_projectionMatrices.back());
    _localFrusta.push_back(localFrustum);
    
    // Compute new bounding box corners
    bbCornerPair corner;
    corner.second = (currMatrix(0,2)<=0?1:0) |
    (currMatrix(1,2)<=0?2:0) |
    (currMatrix(2,2)<=0?4:0);
    corner.first = (~corner.second)&7;
    _bbCorners.push_back(corner);
  }
  
  void DistanceAccumulator::pushDistancePair(double zNear, double zFar)
  {
    // Make sure drawable is visible
    if(std::isfinite(zNear) && std::isfinite(zFar) && (zFar > _minZNear))
    {
      if(zNear < _minZNear) zNear = _minZNear;
      
      // Add distance pair for current drawable
      _distancePairs.push_back(DistancePair(zNear, zFar));
      
      // Override the current nearest/farthest planes if necessary
      if(zNear < _limits.first) _limits.first = zNear;
      if(zFar > _limits.second) _limits.second = zFar;
    }
  }
  
  /** Return true if the node should be traversed, and false if the bounding sphere
   of the node is small enough to be rendered by one Camera. If the latter
   is true, then store the node's near & far plane distances. */
  bool DistanceAccumulator::shouldContinueTraversal(osg::Node &node)
  {
    // Allow traversal to continue if culling is disabled, e.g. if subgraph contans
    // an ABSOLUTE_RF child
    if(node.isCullingActive() == false) return true;
    
    // Potentially allow traversal to continue if we haven't reached maximum depth
    bool keepTraversing = (_currentDepth < _maxDepth);
    
    osg::BoundingSphere bs = node.getBound();
    double zNear = 0.0, zFar = 0.0;
    
    // Make sure bounding sphere is valid
    if(bs.valid())
    {
      // Make sure bounding sphere is within the viewing volume
      if(!_localFrusta.back().contains(bs)) keepTraversing = false;
      
      else // Compute near and far planes for this node
      {
        // Since the view matrix could involve complex transformations,
        // we need to determine a new BoundingSphere that would encompass
        // the transformed BoundingSphere.
        const osg::Matrix &l2w = _viewMatrices.back();
        
        // Get the transformed x-axis of the BoundingSphere
        osg::Vec3d newX = bs._center;
        newX.x() += bs._radius; // Get X-edge of bounding sphere
        newX = newX * l2w;
        
        // Get the transformed y-axis of the BoundingSphere
        osg::Vec3d newY = bs._center;
        newY.y() += bs._radius; // Get Y-edge of bounding sphere
        newY = newY * l2w;
        
        // Get the transformed z-axis of the BoundingSphere
        osg::Vec3d newZ = bs._center;
        newZ.z() += bs._radius; // Get Z-edge of bounding sphere
        newZ = newZ * l2w;
        
        // Get the transformed center of the BoundingSphere
        bs._center = bs._center * l2w;
        
        // Compute lengths of transformed x, y, and z axes
        double newXLen = (newX - bs._center).length();
        double newYLen = (newY - bs._center).length();
        double newZLen = (newZ - bs._center).length();
        
        // The encompassing radius is the max of the transformed lengths
        bs._radius = newXLen;
        if(newYLen > bs._radius) bs._radius = newYLen;
        if(newZLen > bs._radius) bs._radius = newZLen;
        
        // Now we can compute the near & far planes, noting that for
        // complex view transformations (ie. involving scales) the new
        // BoundingSphere may be bigger than the original one.
        // Note that the negative sign on the bounding sphere center is
        // because we want distance to increase INTO the screen.
        zNear = -bs._center.z() - bs._radius;
        zFar = -bs._center.z() + bs._radius;
        
        // If near/far ratio is big enough, then we don't need to keep
        // traversing children of this node.
        if(zNear >= zFar*_nearFarRatio) keepTraversing = false;
      }
    }
    
    // If traversal should stop, then store this node's (near,far) pair
    if(!keepTraversing) pushDistancePair(zNear, zFar);
    
    return keepTraversing;
  }
  
  void DistanceAccumulator::apply(osg::Node &node)
  {
    if(shouldContinueTraversal(node))
    {
      // Traverse this node
      ++_currentDepth;
      traverse(node);
      --_currentDepth;
    }
  }
  
  void DistanceAccumulator::apply(osg::Projection &proj)
  {
    if(shouldContinueTraversal(proj))
    {
      // Push the new projection matrix view frustum
      _projectionMatrices.push_back(proj.getMatrix());
      pushLocalFrustum();
      
      // Traverse the group
      ++_currentDepth;
      traverse(proj);
      --_currentDepth;
      
      // Reload original matrix and frustum
      _localFrusta.pop_back();
      _bbCorners.pop_back();
      _projectionMatrices.pop_back();
    }
  }
  
  void DistanceAccumulator::apply(osg::Transform &transform)
  {
    // ABSOLUTE_RF transform is the equivalent of resetting the
    // modelview matrix, so only check whether traversal is required
    // if transform is not absolute.
    bool continueTraversal = true;
    if (transform.getReferenceFrame() != osg::Transform::ABSOLUTE_RF)
      continueTraversal = shouldContinueTraversal(transform);

    if(continueTraversal)
    {
      // Compute transform for current node
      osg::Matrix currMatrix = _viewMatrices.back();
      bool pushMatrix = transform.computeLocalToWorldMatrix(currMatrix, this);
      
      if(pushMatrix)
      {
        // Store the new modelview matrix and view frustum
        _viewMatrices.push_back(currMatrix);
        pushLocalFrustum();
      }
      
      ++_currentDepth;
      traverse(transform);
      --_currentDepth;
      
      if(pushMatrix)
      {
        // Restore the old modelview matrix and view frustum
        _localFrusta.pop_back();
        _bbCorners.pop_back();
        _viewMatrices.pop_back();
      }
    }
  }
  
  void DistanceAccumulator::apply(osg::Geode &geode)
  {
    // Contained drawables will only be individually considered if we are
    // allowed to continue traversing.
    if(shouldContinueTraversal(geode))
    {
      osg::Drawable *drawable;
      double zNear, zFar;
      
      // Handle each drawable in this geode
      for(unsigned int i = 0; i < geode.getNumDrawables(); ++i)
      {
        drawable = geode.getDrawable(i);
        if (!validNodeMask(*drawable)) continue;
        
        const osg::BoundingBox &bb = drawable->getBoundingBox();
        if(bb.valid())
        {
          // Make sure drawable will be visible in the scene
          if(!_localFrusta.back().contains(bb)) continue;
          
          // Compute near/far distances for current drawable
          zNear = distance(bb.corner(_bbCorners.back().first),
                           _viewMatrices.back());
          zFar = distance(bb.corner(_bbCorners.back().second),
                          _viewMatrices.back());
          if(zNear > zFar) std::swap(zNear, zFar);
          pushDistancePair(zNear, zFar);
        }
      }
    }
  }
  
  void DistanceAccumulator::setMatrices(const osg::Matrix &modelview,
                                        const osg::Matrix &projection)
  {
    _modelview = modelview;
    _projection = projection;
  }
  
  void DistanceAccumulator::reset()
  {
    // Clear vectors & values
    _distancePairs.clear();
    _cameraPairs.clear();
    _limits.first = DBL_MAX;
    _limits.second = 0.0;
    _currentDepth = 0;
    
    // Initial transform matrix is the modelview matrix
    _viewMatrices.clear();
    _viewMatrices.push_back(_modelview);
    
    // Set the initial projection matrix
    _projectionMatrices.clear();
    _projectionMatrices.push_back(_projection);
    
    // Create a frustum without near/far planes, for cull computations
    _localFrusta.clear();
    _bbCorners.clear();
    pushLocalFrustum();
  }
  
  void DistanceAccumulator::computeCameraPairs()
  {
    // Nothing in the scene, so no cameras needed
    if(_distancePairs.empty()) return;
    
    // Entire scene can be handled by just one camera
    if(_limits.first >= _limits.second*_nearFarRatio)
    {
      _cameraPairs.push_back(_limits);
      return;
    }
    
    PairList::iterator i,j;
    
    // Sort the list of distance pairs by descending far distance
    std::sort(_distancePairs.begin(), _distancePairs.end(), precedes);
    
    // Combine overlapping distance pairs. The resulting set of distance
    // pairs (called combined pairs) will not overlap.
    PairList combinedPairs;
    DistancePair currPair = _distancePairs.front();
    for(i = _distancePairs.begin(); i != _distancePairs.end(); ++i)
    {
      // Current distance pair does not overlap current combined pair, so
      // save the current combined pair and start a new one.
      if(i->second < 0.99*currPair.first)
      {
        combinedPairs.push_back(currPair);
        currPair = *i;
      }
      
      // Current distance pair overlaps current combined pair, so expand
      // current combined pair to encompass distance pair.
      else
        currPair.first = std::min(i->first, currPair.first);
    }
    combinedPairs.push_back(currPair); // Add last pair
    
    // Compute the (near,far) distance pairs for each camera.
    // Each of these distance pairs is called a "view segment".
    double currNearLimit, numSegs, new_ratio;
    const double ratio_invlog = 1.0/log(_nearFarRatio);
    unsigned int temp;
    for(i = combinedPairs.begin(); i != combinedPairs.end(); ++i)
    {
      currPair = *i; // Save current view segment
      
      // Compute the fractional number of view segments needed to span
      // the current combined distance pair.
      currNearLimit = currPair.second*_nearFarRatio;
      if(currPair.first >= currNearLimit) numSegs = 1.0;
      else
      {
        numSegs = log(currPair.first/currPair.second)*ratio_invlog;
        
        // Compute the near plane of the last view segment
        //currNearLimit *= pow(_nearFarRatio, -floor(-numSegs) - 1);
        for(temp = (unsigned int)(-floor(-numSegs)); temp > 1; --temp)
        {
          currNearLimit *= _nearFarRatio;
        }
      }
      
      // See if the closest view segment can absorb other combined pairs
      for(j = i+1; j != combinedPairs.end(); ++j)
      {
        // No other distance pairs can be included
        if(j->first < currNearLimit) break;
      }
      
      // If we did absorb another combined distance pair, recompute the
      // number of required view segments.
      if(i != j-1)
      {
        i = j-1;
        currPair.first = i->first;
        if(currPair.first >= currPair.second*_nearFarRatio) numSegs = 1.0;
        else numSegs = log(currPair.first/currPair.second)*ratio_invlog;
      }
      
      /* Compute an integer number of segments by rounding the fractional
       number of segments according to how many segments there are.
       In general, the more segments there are, the more likely that the
       integer number of segments will be rounded down.
       The purpose of this is to try to minimize the number of view segments
       that are used to render any section of the scene without violating
       the specified _nearFarRatio by too much. */
      if(numSegs < 10.0) numSegs = floor(numSegs + 1.0 - 0.1*floor(numSegs));
      else numSegs = floor(numSegs);
      
      
      // Compute the near/far ratio that will be used for each view segment
      // in this section of the scene.
      new_ratio = pow(currPair.first/currPair.second, 1.0/numSegs);
      
      // Add numSegs new view segments to the camera pairs list
      for(temp = (unsigned int)numSegs; temp > 0; temp--)
      {
        currPair.first = currPair.second*new_ratio;
        _cameraPairs.push_back(currPair);
        currPair.second = currPair.first;
      }
    }
  }
  
  void DistanceAccumulator::setNearFarRatio(double ratio)
  {
    if(ratio <= 0.0 || ratio >= 1.0) return;
    _nearFarRatio = ratio;
  }
  
} // !namespace OpenFrames
