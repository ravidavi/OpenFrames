// Vertex shader for FPSM shadow pass
#version 120
uniform mat4 osg_ModelViewProjectionMatrix; // For the shadow camera

void main(void)
{
  // Position is just passed through
  gl_Position = osg_ModelViewProjectionMatrix*gl_Vertex;
}
