#ifndef H_SILVERLININGNODE
#define H_SILVERLININGNODE

#include <osg/Geode>
#include <SilverLining.h>

class SilverLiningNode : public osg::Geode
{
  class SkyDrawable : public osg::Drawable
  {
  public:
    virtual void drawImplementation(osg::RenderInfo& renderInfo) const;
    virtual osg::BoundingBox computeBoundingBox() const;

    SkyDrawable(SilverLiningNode* s = NULL);
    SkyDrawable(const SkyDrawable& copy, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY);
    META_Object(osg, SkyDrawable)

  protected:
    SilverLiningNode* _silverLining;
  };

  class CloudDrawable : public osg::Drawable
  {
  public:
    virtual void drawImplementation(osg::RenderInfo& renderInfo) const;
    virtual osg::BoundingBox computeBoundingBox() const;

    CloudDrawable(SilverLiningNode* s = NULL);
    CloudDrawable(const CloudDrawable& copy, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY);
    META_Object(osg, CloudDrawable)

  protected:
    SilverLiningNode* _silverLining;
  };

  struct AtmosphereUpdater : public osg::Callback
  {
  public:
    /// Inherited from osg::Callback, implements the callback.
    virtual bool run(osg::Object* object, osg::Object* data);
  };

public:
  SilverLiningNode(const char* licenseUser = NULL, const char* licenseKey = NULL);
  SilverLiningNode(const SilverLiningNode& copy, const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY);
  META_Node(osg, SilverLiningNode);

  osg::Drawable* skyDrawable() { return _sky.get(); }
  const osg::Drawable* skyDrawable() const { return _sky.get(); }

  osg::Drawable* cloudDrawable() { return _cloud.get(); }
  const osg::Drawable* cloudDrawable() const { return _cloud.get(); }

  SilverLining::Atmosphere* atmosphere() { return _atmosphere; }
  const SilverLining::Atmosphere* atmosphere() const { return _atmosphere; }
  bool isAtmosphereValid() const { return _initialized; }

  void setResourcePath(const std::string& path) { _resourcePath = path; }
  const std::string& getResourcePath() const { return _resourcePath; }

  void setGlobalLight(osg::Light* l) { _light = l; }
  osg::Light* getGlobalLight() { return _light.get(); }
  const osg::Light* getGlobalLight() const { return _light.get(); }

  void setCameraPosition(const osg::Vec3d& pos) { _cameraPos = pos; }
  const osg::Vec3d& getCameraPosition() const { return _cameraPos; }

  void setCamera(osg::Camera* cam) { _camera = cam; }
  osg::Camera* getCamera() const { return _camera; }

  void setSkyBoxSize(const double& size) { _skyBoxSize = size; }
  double getSkyBoxSize() const { return _skyBoxSize; }

  // You must derive this method to create your atmosphere data at creation
  virtual void createAtmosphereData(osg::RenderInfo& renderInfo) {}

  // Called internally to start SilverLining environment
  virtual bool initializeSilverLining(osg::RenderInfo& renderInfo);

  // Called internally to update view location
  virtual void updateLocation();

  // Called internally to update attached light according to SilverLining sky
  virtual void updateGlobalLight();

protected:
  virtual ~SilverLiningNode();

  osg::observer_ptr<SkyDrawable> _sky;
  osg::observer_ptr<CloudDrawable> _cloud;
  osg::observer_ptr<osg::Light> _light;
  SilverLining::Atmosphere* _atmosphere;
  std::string _resourcePath;
  osg::Vec3d _cameraPos;
  osg::Camera* _camera;
  double _skyBoxSize;
  bool _initialized;
};

#endif
