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

#include "ofrenderpool.h"

// Pool of all Renderers for winID reconciliation in callbacks
QVector<OFRendererIF *> OFRenderPool::POOL;

OFRendererIF::OFRendererIF()
{
    OFRenderPool::addInstance(this);
}

OFRendererIF::~OFRendererIF()
{
    OFRenderPool::removeInstance(this);
}

OFRendererIF *OFRenderPool::findInstanceWithWinID(unsigned int *winID)
{
    OFRendererIF *renderer = 0x0;

    if (winID != 0x0) {
        // locate the thread that contains winID in the pool
        for (QVector<OFRendererIF *>::iterator it = POOL.begin(); it != POOL.end(); it++) {
            if ((*it)->winproxy() != 0x0) {
                 if ((*it)->winproxy()->getID() == *winID) {
                     renderer = (*it);
                    break;
                }
            }
        }
    }

    return renderer;
}

void OFRenderPool::dealKeyPressCallback(unsigned int *winID, unsigned int *row, unsigned int *col, int *key)
{
    OFRendererIF *renderer = findInstanceWithWinID(winID);

    // Pass key press to the appropriate thread
    if (renderer != 0x0) {
        if (key != 0x0) {
            renderer->keyPressCallback(*key);
        }
    }
}

void OFRenderPool::dealMakeCurrent(unsigned int *winID, bool *success)
{
    OFRendererIF *renderer = findInstanceWithWinID(winID);

    // Pass make current to the appropriate thread
    if (renderer != 0x0) {
        *success = renderer->makeCurrent();
    }
    else {
        *success = false;
    }
}

void OFRenderPool::dealSwapBuffers(unsigned int *winID)
{
    OFRendererIF *renderer = findInstanceWithWinID(winID);

    // Pass swapbuffers to the appropriate thread
    if (renderer != 0x0) {
        renderer->swapBuffers();
    }
}
