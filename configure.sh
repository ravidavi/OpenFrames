#! /bin/bash
# Create the OpenFrames build system using CMake

# Get directory of this script
BASEDIR=$(pwd)

# Create the build tree for CMake
mkdir -p build
cd build

# Get whether to create Release/Debug build
read -p "Enter build type [Release/Debug]: " buildtype

# Get install location from user
echo "Please select a folder into which OpenFrames will be installed, or press [enter] for default install/ folder."
read -p "Enter full folder path [enter = install/]: " instdir

# Create build system with CMake
# The default install directory is OpenFramesDir/install, but this
# can be changed by using the CMake GUI to set CMAKE_INSTALL_PREFIX
echo
echo "Creating OpenFrames build system with CMake"
if [ -z "$instdir" ];
then
  cmake -DCMAKE_INSTALL_PREFIX="$BASEDIR/install" -DCMAKE_BUILD_TYPE=$buildtype -DCMAKE_OSX_ARCHITECTURES=x86_64 ..
else
  cmake -DCMAKE_INSTALL_PREFIX="$instdir" -DCMAKE_BUILD_TYPE=$buildtype -DCMAKE_OSX_ARCHITECTURES=x86_64 ..
fi
