/** \file SensorVisibilityCallback.cpp
 * Definitions for the SensorVisibilityCallback class
 */

#include <OpenFrames/SensorVisibilityCallback.hpp>
#include <iostream>

namespace OpenFrames
{

SensorVisibilityCallback::SensorVisibilityCallback(OpenFrames::ReferenceFrame* root)
    : _root(root)
{
    _rayIntersector = new osgUtil::RayIntersector();
    _iv.setIntersector(_rayIntersector);
    _iv.setTraversalMask(~0x1);
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
    // colorA.a() = 0.4; // Make line color slightly transparent

    // Vertex B corresponds to Frame B (the target)
    TransformAccumulator *frameBXform = std::get<1>(_frameData[segID]);
    ReferenceFrame *frameB = frameBXform->getOrigin();
    osg::Matrixd matLocalToWorld_B = frameBXform->getLocalToWorld();
    osg::Vec3d endLocal = std::get<3>(_frameData[segID]);
    osg::Vec3d endWorld = matLocalToWorld_B.preMult(endLocal);    
    colorB = frameB->getColor();
    // colorB.a() = 0.4; // Make line color slightly transparent

    // Test whether point B is in sensor A's FOV
    PolyhedralCone *sensor = dynamic_cast<PolyhedralCone*>(frameA);     // frameA is guaranteed to be a PolyhedralCone because it is enforced by addSegment()
    bool isVisible = true;
    if (sensor)
    {
        // Get vector to target's origin in sensor's space
        osg::Vec3d targetVec = osg::Matrixd::inverse(matLocalToWorld_A).preMult(endWorld);

        // isVisible = sensor->isVisible(targetVec);
        isVisible = sensor->isVisible(targetVec, 0, sensor->getConeLength());
    } 

    IntersectionData newIntersectionData;

    osg::Vec3 directionVector;
    // If visible from sensor then do intersection test
    if (isVisible)
    {
        // Create intersection test from start to end points
        _iv.reset();
        _rayIntersector->setStart(startWorld);
        _rayIntersector->setDirection(endWorld - startWorld);

        // Test for intersection and set segment endpoint accordingly
        _root->getGroup()->accept(_iv);
        osgUtil::RayIntersector::Intersection intersection = _rayIntersector->getFirstIntersection();
        if (intersection.distance == -1) {
            posB = posA;    // Set line segment to zero length because there's no intersection
            // Set the intersection data to its default value which indicates no intersection:
            newIntersectionData.position = osg::Vec3(INFINITY, INFINITY, INFINITY);
            newIntersectionData.angleOfIncidence = INFINITY;
        } else {
            // Want to eventually change this so the line segment only has a real second endpoint when the ray intersects with the
            // desired object (frameB), not something between frameA and frameB
            posB = intersection.getWorldIntersectPoint();

            newIntersectionData.position = intersection.getLocalIntersectPoint();
            // Get the normal of the surface at the intersection point:
            osg::Vec3 normal = intersection.getWorldIntersectNormal();
            normal.normalize();
            // Get the direction of the vector from posA to posB:
            directionVector = posB - posA;
            directionVector.normalize();
            // the angle of incidence is the inverse cosine of the dot product of the two vectors:
            newIntersectionData.angleOfIncidence = acos(directionVector * normal);
            // if the angle of incidence is greater than 90 degrees, fix it:
            if (newIntersectionData.angleOfIncidence > osg::PI_2) {
                newIntersectionData.angleOfIncidence = osg::PI - newIntersectionData.angleOfIncidence;
            }
        }
    } else {
        posB = posA; // Otherwise set line segment to zero length
        // Set the intersection data to its default value which indicates no intersection:
        newIntersectionData.position = osg::Vec3(INFINITY, INFINITY, INFINITY);
        newIntersectionData.angleOfIncidence = INFINITY;
    }
    _intersectionData[segID] = newIntersectionData;
}

// Add a pair of ReferenceFrames that will have a line segment drawn between them
void SensorVisibilityCallback::addSegment(PolyhedralCone *frameA, ReferenceFrame *frameB, const osg::Vec3d &posA, const osg::Vec3d &posB)
{
    // Make sure this callback's data is not being used
    OpenThreads::ScopedLock<OpenThreads::Mutex> lock(mMutex);

    _frameData.push_back(FrameData(new TransformAccumulator(_root, frameA), new TransformAccumulator(_root, frameB), posA, posB));

    IntersectionData newIntersectionData{osg::Vec3(INFINITY, INFINITY, INFINITY), INFINITY};
    _intersectionData.push_back(newIntersectionData);
}

int SensorVisibilityCallback::getSegmentID(PolyhedralCone *frameA, ReferenceFrame *frameB, const osg::Vec3d &posA, const osg::Vec3d &posB)
{
    FrameData frameDataForSegment{new TransformAccumulator(_root, frameA), new TransformAccumulator(_root, frameB), posA, posB};
    for (int i = 0; i < _frameData.size(); i++) {
        // Check if all components of the new FrameData match all of the components of an existing FrameData in the _frameData vector
        // FIXME: Need to check for origin also
        if ( (std::get<0>(_frameData[i])->getRoot() == std::get<0>(frameDataForSegment)->getRoot()) &&
             (std::get<1>(_frameData[i])->getRoot() == std::get<1>(frameDataForSegment)->getRoot()) &&
             (std::get<2>(_frameData[i]) == std::get<2>(frameDataForSegment)) &&
             (std::get<3>(_frameData[i]) == std::get<3>(frameDataForSegment)) )
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


osg::Vec3 SensorVisibilityCallback::getIntersectionPosition(const unsigned int &segID)
{
    return _intersectionData[segID].position;
}

float SensorVisibilityCallback::getIntersectionAngle(const unsigned int &segID)
{
    return _intersectionData[segID].angleOfIncidence;
}

SensorVisibilityCallback::~SensorVisibilityCallback() {  }

} // !namespace OpenFrames