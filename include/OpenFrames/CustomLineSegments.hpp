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

/** \file CustomLineSegments.hpp
 * Declaration of CustomLineSegments class.
 */

#ifndef _OF_CUSTOMLINESEGMENTS_
#define _OF_CUSTOMLINESEGMENTS_

#include <OpenFrames/Export.h>
#include <OpenFrames/ReferenceFrame.hpp>
#include <osg/Geode>
#include <osg/LineWidth>

namespace OpenFrames
{
  /**
   * \class CustomLineSegments
   *
   * \brief A ReferenceFrame that renders line segments with custom endpoints and parameters
   *
   * This class allows creating customized line segments, where each segment's endpoints are
   * dynamically updated based on a callback. Shaders used to render the lines are also customizable.
   */
  class OF_EXPORT CustomLineSegments : public ReferenceFrame
  {
  public:
    CustomLineSegments( const std::string &name );
    CustomLineSegments( const std::string &name, const osg::Vec3 &color );
    CustomLineSegments( const std::string &name, const osg::Vec4 &color );
    CustomLineSegments( const std::string &name , float r, float g, float b, float a = 1.0 );
    
    // Show/hide this frame's contents, e.g. everything a frame shows (excluding axes, labels, and children)
    // Inherited from ReferenceFrame
    virtual void showContents(bool showContents);
    virtual bool getContentsShown() const;

    // Set number of line segments to draw
    void setNumSegments(unsigned int numSegments);
    unsigned int getNumSegments() const;

    // Set segment line width
    void setLineWidth(float width);
    float getLineWidth() const { return _lineWidth->getWidth(); }

    // Set fragment shader file for segment lines
    bool setLineShader(const std::string &fname);

    // Callback that computes vertex positions and colors for each line segment
    // Subclass this to implement your callback
    class Callback : public osg::Referenced
    {
    public:
      Callback() {}

      // Required callback function
      virtual void getSegmentData(const unsigned int &segID, osg::Vec3 &posA, osg::Vec4 &colorA, osg::Vec3 &posB, osg::Vec4 &colorB) = 0;

      // Parameters available to the callback
      double mFrameTime; // Frame time (i.e. wall clock time)
      double mSimTime;   // Simulation time

    protected:
      virtual ~Callback() {}
    };

    void setLineSegmentCallback(Callback *cb);

     
    /** Inherited from ReferenceFrame. */
    virtual const osg::BoundingSphere& getBound() const;

    /// Inherited
    virtual std::string frameInfo() const { return "CustomLineSegments"; }
    
  protected:
    virtual ~CustomLineSegments();
        
    osg::ref_ptr<osg::Geode> _geode;
    osg::ref_ptr<osg::Geometry> _segmentGeom;
    osg::ref_ptr<osg::LineWidth> _lineWidth;
    osg::ref_ptr<osg::Shader> _fragShader; // Line fragment shader
    
  private:
    void _init();
  };
  
} // !namespace OpenFrames

#endif
