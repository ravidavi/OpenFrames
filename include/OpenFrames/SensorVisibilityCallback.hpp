/** \file SensorVisibilityCallback.hpp
 * Declaration of SensorVisibilityCallback class
 */

#ifndef _OF_SENSORVISIBILITYCALLBACK_
#define _OF_SENSORVISIBILITYCALLBACK_

#include <OpenFrames/Export.h>
#include <OpenFrames/CustomLineSegments.hpp>
#include <OpenFrames/PolyhedralCone.hpp>
#include <OpenFrames/TransformAccumulator.hpp>
#include <osgUtil/RayIntersector>

namespace OpenFrames
{
    /**
    * \class SensorVisibilityCallback
    *
    * \brief Computes the visibility of a ReferenceFrame from a PolyhedralCone
    *
    * This class is used to determine if a point in a ReferenceFrame is visible from a
    * PolyhedralCone. If the ReferenceFrame is within the PolyhedralCone, a line segment
    * is drawn from the PolyhedralCone to the first intersection point between the cone
    * and the ReferenceFrame; otherwise no line segment is drawn. The local intersection
    * point and intersection angle of incidence are available to the user via
    * getIntersectionPosition() and getIntersectionAngle().
    * 
    */
    class OF_EXPORT SensorVisibilityCallback : public CustomLineSegments::Callback
    {
    public:
        SensorVisibilityCallback(OpenFrames::ReferenceFrame* root);

        virtual unsigned int getNumSegments() const;

        virtual void getSegmentData(const unsigned int &segID, osg::Vec3 &posA, osg::Vec4 &colorA, osg::Vec3 &posB, osg::Vec4 &colorB) const;

        void addSegment(PolyhedralCone *frameA, ReferenceFrame *frameB, const osg::Vec3d &posA, const osg::Vec3d &posB);

        int getSegmentID(PolyhedralCone *frameA, ReferenceFrame *frameB, const osg::Vec3d &posA, const osg::Vec3d &posB);

        osg::Vec3 getIntersectionPosition(const unsigned int &segID);

        float getIntersectionAngle(const unsigned int &segID);

    protected:
        virtual ~SensorVisibilityCallback();

        // A frame pair is two ReferenceFrames that should have a line drawn between them
        ReferenceFrame *_root;
        typedef std::tuple< osg::ref_ptr<TransformAccumulator>, osg::ref_ptr<TransformAccumulator>, osg::Vec3d, osg::Vec3d> FrameData;
        std::vector<FrameData> _frameData; // List of frame data

        // Intersection test
        osg::ref_ptr<osgUtil::RayIntersector> _rayIntersector;
        mutable osgUtil::IntersectionVisitor _iv;

        // Intersection data
        struct IntersectionData
        {
            osg::Vec3 position;         // position at which the line segment intersects with frameB in the local frame
            float angleOfIncidence;     // angle of incidence where the line segment intersects at frameB
        } ;
        mutable std::vector<IntersectionData> _intersectionData;


    };
} // !namespace OpenFrames

#endif