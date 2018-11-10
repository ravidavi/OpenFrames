/***********************************
   Copyright 2018 Ravishankar Mathur

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

/** \file SegmentArtist.hpp
 * Declaration of SegmentArtist class.
 */

#ifndef _OF_SEGMENTARTIST_
#define _OF_SEGMENTARTIST_

#include <OpenFrames/Export.h>
#include <OpenFrames/TrajectoryArtist.hpp>
#include <osg/LineStipple>
#include <osg/LineWidth>

namespace OpenFrames
{
  /**
   * \class SegmentArtist
   *
   * \brief Draws a line segment at each point in the Trajectory
   *
   * The SegmentArtist is a type of TrajectoryArtist that draws a line segment at each
   * point in the Trajectory. The starting and stopping X/Y/Z components of the line segments
   * can be independently specified to be any elements of the Trajectory.
   */
  class OF_EXPORT SegmentArtist : public TrajectoryArtist
  {
  public:
    SegmentArtist(const Trajectory *traj = NULL);

    // Copy constructor
    SegmentArtist(const SegmentArtist &ca,
      const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY);

    /** Standard OSG node methods. */
    virtual Object* cloneType() const { return new SegmentArtist(); }
    virtual Object* clone(const osg::CopyOp& copyop) const { return new SegmentArtist(*this, copyop); }
    virtual bool isSameKindAs(const osg::Object* obj) const { return dynamic_cast<const SegmentArtist*>(obj) != NULL; }
    virtual const char* libraryName() const { return "OpenFrames"; }
    virtual const char* className() const { return "SegmentArtist"; }

    /** Set the trajectory to be drawn. */
    virtual void setTrajectory(const Trajectory *traj);

    /** Set the data to be used for plotting x/y/z components */
    bool setStartXData(const Trajectory::DataSource &src);
    bool setEndXData(const Trajectory::DataSource &src);
    bool setStartYData(const Trajectory::DataSource &src);
    bool setEndYData(const Trajectory::DataSource &src);
    bool setStartZData(const Trajectory::DataSource &src);
    bool setEndZData(const Trajectory::DataSource &src);

    const Trajectory::DataSource* getStartDataSource() const { return _startSource; }
    const Trajectory::DataSource* getEndDataSource() const { return _endSource; }

    /** Set/get the offset between consecutive drawn points */
    void setStride(unsigned int stride);
    inline unsigned int getStride() const { return _stride; }

    /** Specify line attributes that should be used. */
    void setColor(float r, float g, float b);
    void setWidth(float width);
    void setPattern(GLint factor, GLushort pattern);

    /** Data was cleared from or added to the Trajectory. Inherited
        from TrajectoryArtist */
    virtual void dataCleared(const Trajectory* traj);
    virtual void dataAdded(const Trajectory* traj);

    bool isDataValid() const { return _dataValid; }
    bool isStartDataZero() const { return _startDataZero; }
    bool isEndDataZero() const { return _endDataZero; }

  protected:
    virtual ~SegmentArtist();

    void verifyData() const;

    Trajectory::DataSource _startSource[3]; // Data sources for starting point
    Trajectory::DataSource _endSource[3]; // Data sources for ending point

    unsigned int _stride; // Minimum offset between sucessive drawn points

    /** Line width, stipple pattern, and color. */
    osg::ref_ptr<osg::LineWidth> _lineWidth;
    osg::ref_ptr<osg::LineStipple>  _linePattern;
    osg::ref_ptr<osg::Vec4Array> _lineColors;

    mutable bool _dataValid; // If trajectory supports required data
    mutable bool _startDataZero;
    mutable bool _endDataZero;
  };

}

#endif
