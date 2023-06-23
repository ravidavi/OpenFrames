/** \file SensorVisibilityCallback.cpp
 * Definitions for the SensorVisibilityCallback class
 */

#include <limits>

#include <OpenFrames/SensorVisibilityCallback.hpp>
#include <iostream>

namespace OpenFrames
{

SensorVisibilityCallback::SensorVisibilityCallback(OpenFrames::ReferenceFrame* root)
    : _root(root), _minDist(0.0), _maxDist(-1.0)
{
    _rayIntersector = new osgUtil::RayIntersector();
    _iv.setIntersector(_rayIntersector);
    _iv.setTraversalMask(~0x1);
}

SensorVisibilityCallback::IntersectionData::IntersectionData()
{
    reset();
}

void SensorVisibilityCallback::IntersectionData::reset()
{
    position.set(NAN, NAN, NAN);
    normal.set(NAN, NAN, NAN);
    endpointSource.set(NAN, NAN, NAN);
    endpointTarget.set(NAN, NAN, NAN);
}
    
void SensorVisibilityCallback::ignoreReferenceFrame(OpenFrames::ReferenceFrame* frame) const
{
    if(frame)
    {
        // Remove intersection mask bits from frame's mask
        osg::Node::NodeMask currMask = frame->getGroup()->getNodeMask();
        frame->getGroup()->setNodeMask(currMask & ~getIntersectionMask());
    }
}
    
// Required: get number of segments
unsigned int SensorVisibilityCallback::getNumSegments() const
{
    return _frameData.size();
}

// Required: get data for given segment
void SensorVisibilityCallback::getSegmentData(const unsigned int &segID, osg::Vec3 &posA, osg::Vec4 &colorA, osg::Vec3 &posB, osg::Vec4 &colorB) const
{
    // Vertex A corresponds to Frame A (the sensor)
    TransformAccumulator* frameAXform = std::get<0>(_frameData[segID]);
    osg::Matrixd matLocalToWorld_A = frameAXform->getLocalToWorld();
    osg::Vec3d startLocal = std::get<2>(_frameData[segID]);
    osg::Vec3d startWorld = matLocalToWorld_A.preMult(startLocal);
    posA = startWorld;
    ReferenceFrame *frameA = frameAXform->getOrigin();
    colorA = frameA->getColor();

    // Vertex B corresponds to Frame B (the target)
    TransformAccumulator *frameBXform = std::get<1>(_frameData[segID]);
    ReferenceFrame *frameB = frameBXform->getOrigin();
    osg::Matrixd matLocalToWorld_B = frameBXform->getLocalToWorld();
    osg::Vec3d endLocal = std::get<3>(_frameData[segID]);
    osg::Vec3d endWorld = matLocalToWorld_B.preMult(endLocal);    
    colorB = frameB->getColor();

    // Create intersection test from start to end points
    _iv.reset();
    _rayIntersector->setStart(startWorld);
    _rayIntersector->setDirection(endWorld - startWorld);

    // Test for intersection and set segment endpoint accordingly
    _root->getGroup()->accept(_iv);
    osgUtil::RayIntersector::Intersection intersection = _rayIntersector->getFirstIntersection();
    _intersectionData[segID].reset();
    posB = posA; // Assume no intersection
    if (intersection.distance != -1) // Check for intersection
    {
        // Want to eventually change this so the line segment only has a real second endpoint when the ray intersects with the
        // desired object (frameB), not something between frameA and frameB
        osg::Vec3d intWorld = intersection.getWorldIntersectPoint();

        // Test whether intersection is in sensor's FOV
        bool isVisible = true;
        PolyhedralCone *sensor = dynamic_cast<PolyhedralCone*>(frameA);
        if (sensor)
        {
            osg::Vec3d intLocalSensor = osg::Matrixd::inverse(matLocalToWorld_A).preMult(intWorld); // Intersection in sensor space
            isVisible = sensor->isVisible(intLocalSensor, _minDist, (_maxDist < _minDist) ? sensor->getConeLength() : _maxDist);
        }
        
        // If intersection point is visible, compute statistics
        if(isVisible)
        {
            posB = intWorld;
            computeIntersectionData(segID, intersection, startWorld);
        }
    }
}

void SensorVisibilityCallback::computeIntersectionData(const unsigned int& segID,
                                                       const osgUtil::RayIntersector::Intersection& intersection,
                                                       const osg::Vec3d& endpointWorld) const
{
    IntersectionData& intersectionData = _intersectionData[segID];
        
    intersectionData.position = intersection.getLocalIntersectPoint(); // Intersection point in local frame
    intersectionData.normal = intersection.getLocalIntersectNormal(); // Surface normal at intersection
    intersectionData.endpointSource = osg::Matrixd::inverse(*(intersection.matrix)).preMult(endpointWorld); // Intersection line source
    intersectionData.endpointTarget = std::get<3>(_frameData[segID]); // Intersection line target
}

// Add a pair of ReferenceFrames that will have a line segment drawn between them
void SensorVisibilityCallback::addSegment(PolyhedralCone *frameA, ReferenceFrame *frameB, const osg::Vec3d &posA, const osg::Vec3d &posB)
{
    if((frameA == nullptr) || (frameB == nullptr)) return; // Sanity check
    
    // Make sure this callback's data is not being used
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(mMutex);

    _frameData.emplace_back(FrameData(new TransformAccumulator(_root, frameA), new TransformAccumulator(_root, frameB), posA, posB));
    _intersectionData.emplace_back(IntersectionData());
}

int SensorVisibilityCallback::getSegmentID(PolyhedralCone *frameA, ReferenceFrame *frameB, const osg::Vec3d &posA, const osg::Vec3d &posB)
{
    // Check if the frames and positions match up with any existing frame data
    for (int i = 0; i < _frameData.size(); i++) {
        if ( (std::get<0>(_frameData[i])->getOrigin() == frameA) &&
             (std::get<1>(_frameData[i])->getOrigin() == frameB) &&
             (std::get<2>(_frameData[i]) == posA) &&
             (std::get<3>(_frameData[i]) == posB) )
        {
            return i;       // return the index of the matching FrameData
        }
    }
    return -1;    // no matching segment in the _frameData vector
}

PolyhedralCone* SensorVisibilityCallback::getSegmentFrameA(const unsigned int &segID) 
{
    return dynamic_cast<PolyhedralCone*>((std::get<0>(_frameData[segID]))->getOrigin());
}

ReferenceFrame* SensorVisibilityCallback::getSegmentFrameB(const unsigned int &segID)
{
    return (std::get<1>(_frameData[segID]))->getOrigin();
}

SensorVisibilityCallback::~SensorVisibilityCallback() { }

} // !namespace OpenFrames
