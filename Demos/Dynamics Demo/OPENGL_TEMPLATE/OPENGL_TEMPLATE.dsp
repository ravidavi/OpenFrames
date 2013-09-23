# Microsoft Developer Studio Project File - Name="OPENGL_TEMPLATE" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=OPENGL_TEMPLATE - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "OPENGL_TEMPLATE.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "OPENGL_TEMPLATE.mak" CFG="OPENGL_TEMPLATE - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "OPENGL_TEMPLATE - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe
# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "../"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt /winapp
# ADD F90 /compile_only /extend_source:132 /include:"c:\wint\lib.vf" /include:"..\..\..\lib" /nologo /warn:nofileopt /winapp
# SUBTRACT F90 /pad_source
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 version.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 winmm.lib opengl32.lib glu32.lib winter.lib kernel32.lib user32.lib gdi32.lib comdlg32.lib winspool.lib shell32.lib advapi32.lib OpenFrames.lib /nologo /subsystem:console /machine:I386 /libpath:"C:\wint\lib.vf" /libpath:"..\..\..\lib"
# SUBTRACT LINK32 /pdb:none
# Begin Target

# Name "OPENGL_TEMPLATE - Win32 Release"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\Integrat.for
# End Source File
# Begin Source File

SOURCE=..\MAIN.F90
DEP_F90_MAIN_=\
	"..\..\..\lib\OPENFRAMES.mod"\
	".\Release\DYNAMIC_VARIABLES.MOD"\
	".\Release\RESOURCE.MOD"\
	"c:\wint\lib.vf\WINTERACTER.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\MODULES.F90
# End Source File
# Begin Source File

SOURCE=..\RESOURCE.f90
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=..\program.ico
# End Source File
# Begin Source File

SOURCE=..\resource.rc
# ADD BASE RSC /l 0x409 /i "\OpenFrames\Demos\Dynamics Demo" /i "\Program Files\OpenFrames\Demos\Dynamics Demo" /i "\Documents and Settings\wrw412a\Desktop\Dynamics Demo" /i "\Documents and Settings\wrw412a\Desktop\OPENGL_DYNAMICS_TEMPLATE"
# ADD RSC /l 0x409 /i "\OpenFrames\Demos\Dynamics Demo" /i "\Program Files\OpenFrames\Demos\Dynamics Demo" /i "\Documents and Settings\wrw412a\Desktop\Dynamics Demo" /i "\Documents and Settings\wrw412a\Desktop\OPENGL_DYNAMICS_TEMPLATE" /i "c:\wint\include"
# End Source File
# End Group
# End Target
# End Project
