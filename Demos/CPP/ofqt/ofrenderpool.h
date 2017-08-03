/***********************************
   Copyright 2017 Ravishankar Mathur

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

#ifndef OFRENDERPOOL_H
#define OFRENDERPOOL_H

#include <OpenFrames/WindowProxy.hpp>
#include <QVector>

// forward declaration to avoid circular dependencies
QT_FORWARD_DECLARE_CLASS(QWindow)

/**********************************************************
 * Matthew Ruschmann
 * OpenFrames ofqt Example, class OFRendererIF
 * This class defines generic interfaces required by
 * OFRenderPool and OFWindow. It also manages the
 * instance's presence in the pool.
**********************************************************/
class OFRendererIF
{
public:
    /** Constructor, adds this instance to the pool of renderers */
    OFRendererIF() {}
    /** Destructor, removes this instance from the pool of renderers */
    virtual ~OFRendererIF() {}

    /** Returns a pointer to the instance of WindowProxy used by this instance */
    virtual OpenFrames::WindowProxy *winproxy() = 0;
    /** Callback from OFRenderPool (passed from OpenFrames) to make current on the OpenGL surface */
    virtual bool makeCurrent() = 0;
    /** Callback from OFRenderPool (passed from OpenFrames) to swap buffers on the OpenGL surface */
    virtual void swapBuffers() = 0;
    /** Callback from OFRenderPool (passed from OpenFrames) to process a keyboard key press */
    virtual void keyPressCallback(int key) = 0;

    /** Called by OFWindow to start WindowProxy rendering of the scene */
    virtual void begin(QWindow *w) = 0;
    /** Called by OFWindow to end WindowProxy rendering of the scene */
    virtual void end() = 0;
};

/**********************************************************
 * Matthew Ruschmann
 * OpenFrames ofqt Example, class OFRenderPool
 * A static class tracks all instances of OFRendererIF and
 * provides static functions that deal OpenFrames callbacks
 * to the OFRendererIF containing a WindowProxy with the
 * given window ID.
**********************************************************/
class OFRenderPool
{
public:
    /** Adds an OFRendererIF to the POOL of all instances */
    static void addInstance(OFRendererIF *renderer) { POOL.append(renderer); }
    /** Removes an OFRendererIF from the POOL of all instances */
    static void removeInstance(OFRendererIF *renderer) { POOL.removeAll(renderer);  }

    /** Deals a keypress callback from OpenFrames to the OFRendererIF with the provided winID */
    static void dealKeyPressCallback(unsigned int *winID, unsigned int *row, unsigned int *col, int *key);
    /** Deals a makecurrent callback from OpenFrames to the OFRendererIF with the provided winID */
    static void dealMakeCurrent(unsigned int *winID, bool *success);
    /** Deals a swapbuffers callback from OpenFrames to the OFRendererIF with the provided winID */
    static void dealSwapBuffers(unsigned int *winID);

private:
    /** Finds the OFRendererIF that is using a WindowProxy with the provided winID */
    static OFRendererIF *findInstanceWithWinID(unsigned int *winID);

    /** Private declaration reserves constructor and destructor because this class is static only */
    OFRenderPool() {};
    ~OFRenderPool() {};

    /** A vector that tracks all active instances of OFRendererIF */
    static QVector<OFRendererIF *> POOL;
};

#endif
