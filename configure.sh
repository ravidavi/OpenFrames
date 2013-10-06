#! /bin/bash
# Create the OpenFrames build system using CMake

# Get directory of this script
BASEDIR=$(pwd)

# Create the build tree for CMake
mkdir -p build
cd build

# Get whether to create Release/Debug build
echo
echo "Please select whether to build a Release or Debug version of OpenFrames."
read -p "Enter build type [(Release)/Debug]: " buildtype
if [ -z "$buildtype" ];
then
  buildtype="Release"
fi

# Get install location from user
echo
echo "Please select a folder into which OpenFrames will be installed."
read -p "Enter full install folder path [(install/)]: " instdir
if [ -z "$instdir" ];
then
  instdir="$BASEDIR/install"
fi

# Get Fortran usage
echo
echo "Please select whether to compile the OpenFrames Fortran module."
echo "Note that a Fortran compiler must be installed."
read -p "Compile Fortran module? [y/(n)]: " usefortran
if [ -z "$usefortran" ];
then
  usefortran="n"
fi

# Create build system with CMake
# The default install directory is OpenFramesDir/install, but this
# can be changed by using the CMake GUI to set CMAKE_INSTALL_PREFIX
echo
echo "Creating OpenFrames build system with CMake"
cmake -DCMAKE_INSTALL_PREFIX="$instdir" -DCMAKE_BUILD_TYPE=$buildtype -DCMAKE_OSX_ARCHITECTURES=x86_64 -DOF_FORTRAN_MODULE=$usefortran ..
