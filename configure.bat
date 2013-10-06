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
SET /p generator="Enter exact generator name [(Visual Studio 10 Win64)]: "
IF ("%generator%") == ("") (
  SET generator=Visual Studio 10 Win64
)

REM Get install location from user
echo.
echo Please select a folder into which OpenFrames will be installed.
SET /p instdir="Full folder path [(install\)]: "
IF ("%instdir%") == ("") (
  SET instdir=%basedir%\install
)

REM Get Fortran usage
echo.
echo Please select whether to compile the OpenFrames Fortran module.
echo Note that a Fortran compiler must be installed.
SET /p usefortran="Compile Fortran module? [y/(n)]: "
IF ("%usefortran%") == ("") (
  SET usefortran=n
)

echo.
echo Creating OpenFrames build system with CMake
cmake -G "%generator%" -DCMAKE_INSTALL_PREFIX="%instdir%" -DOF_FORTRAN_MODULE="%usefortran%" ..

SET /p val="Press enter to continue..."
