@ECHO OFF
REM Get directory of this script
SET basedir=%~dp0
CD "%basedir%"

REM Create the build tree for CMake
MKDIR "build"
CD "build"

REM Create the build system with CMake
REM The default install directory is OpenFramesDir/install, but this
REM can be changed by using the CMake GUI to set CMAKE_INSTALL_PREFIX

echo Please type a CMake Generator name. Examples include:
echo   Visual Studio 8 2005
echo   Visual Studio 8 2005 Win64
echo   Visual Studio 9 2008
echo   Visual Studio 9 2008 Win64
echo   Visual Studio 10
echo   Visual Studio 10 Win64
echo   Visual Studio 11
echo   Visual Studio 11 Win64
echo   Visual Studio 12
echo   Visual Studio 12 Win64
SET /p generator="Enter exact generator name: "

echo.
echo Please select a folder into which OpenFrames will be installed, or press [enter] for default install/ folder.
SET /p instdir="Full folder path [enter = install\]: "
echo %instdir%

IF ("%instdir%") == () (
  cmake -G "%generator%" -DCMAKE_INSTALL_PREFIX="%basedir%\install" ..
) ELSE (
  cmake -G "%generator%" -DCMAKE_INSTALL_PREFIX="%instdir%" ..
)
