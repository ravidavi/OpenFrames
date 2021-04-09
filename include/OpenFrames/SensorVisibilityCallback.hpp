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
    * \brief Computes the visibility of a ReferenceFrame from a PolyhedralCone using a 
    * CustomLineSegment. It is intended to be set as the LineSegmentCallback for a
    * CustomLineSegments object.
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

        // Intersection data
        struct IntersectionData
        {
            IntersectionData();
            void reset();
            
            // All of these quantities are in the target frame's local coordinates
            osg::Vec3d position;  // intersection point
            osg::Vec3 normal;     // intersection normal vector
            osg::Vec3d endpointSource;  // intersection line segment source
            osg::Vec3d endpointTarget;  // intersection line segment target
        } ;
        
        /// Set the node mask used to check a ReferenceFrame when computing intersections
        void setIntersectionMask(osg::Node::NodeMask mask) { _iv.setTraversalMask(mask); }
        osg::Node::NodeMask getIntersectionMask() const { return _iv.getTraversalMask(); }
        
        /// Ignore a ReferenceFrame and all of its children when computing intersections
        void ignoreReferenceFrame(OpenFrames::ReferenceFrame* frame) const;
        
        /// Inherited
        /// Get the number of segments to draw
        virtual unsigned int getNumSegments() const;
      
        /// Inherited
        /// Get endpoint and color data for segment with specified ID
        virtual void getSegmentData(const unsigned int &segID, osg::Vec3 &posA, osg::Vec4 &colorA, osg::Vec3 &posB, osg::Vec4 &colorB) const;
      
        /// Add a line segment from a PolyhedralCone to a ReferenceFrame
        void addSegment(PolyhedralCone *frameA, ReferenceFrame *frameB, const osg::Vec3d &posA, const osg::Vec3d &posB);
        
        /// Set the min/max search distance from the sensor's origin
        /// Provide max < min to use the sensor's length to compute max distance
        /// Note that min & max can be negative, which denotes a point "behind" the sensor origin
        void setMaxSensorDistance(const double& minDist, const double& maxDist) { _minDist = minDist; _maxDist = maxDist; }
        void getMaxSensorDistance(double& minDist, double& maxDist) const { minDist = _minDist; maxDist = _maxDist; }

        /// Get a specific line segment
        int getSegmentID(PolyhedralCone *frameA, ReferenceFrame *frameB, const osg::Vec3d &posA, const osg::Vec3d &posB);

        /// Get the frame associated with the segment endpoints
        PolyhedralCone* getSegmentFrameA(const unsigned int &segID);
        ReferenceFrame* getSegmentFrameB(const unsigned int &segID);
        
        /// Get the intersection data
        std::vector<IntersectionData>& getIntersectionData() const { return _intersectionData; }

    protected:
        virtual ~SensorVisibilityCallback();

        // A frame pair is two ReferenceFrames that should have a line drawn between them
        ReferenceFrame *_root;
        typedef std::tuple< osg::ref_ptr<TransformAccumulator>, osg::ref_ptr<TransformAccumulator>, osg::Vec3d, osg::Vec3d> FrameData;
        std::vector<FrameData> _frameData; // List of frame data

        // Intersection test
        osg::ref_ptr<osgUtil::RayIntersector> _rayIntersector;
        mutable osgUtil::IntersectionVisitor _iv;
        
        // Intersection search distance
        double _minDist, _maxDist;
        
        // Intersection data for each segment
        mutable std::vector<IntersectionData> _intersectionData;
        void computeIntersectionData(const unsigned int& segID,
                                     const osgUtil::RayIntersector::Intersection& intersection,
                                     const osg::Vec3d& endpointWorld) const;
    };
} // !namespace OpenFrames

#endif
