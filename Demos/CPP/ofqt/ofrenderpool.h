/****************************************************************************
**
** Copyright (C) 2016 The Qt Company Ltd.
** Contact: https://www.qt.io/licensing/
**
** This file is part of the examples of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:BSD$
** Commercial License Usage
** Licensees holding valid commercial Qt licenses may use this file in
** accordance with the commercial license agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and The Qt Company. For licensing terms
** and conditions see https://www.qt.io/terms-conditions. For further
** information use the contact form at https://www.qt.io/contact-us.
**
** BSD License Usage
** Alternatively, you may use this file under the terms of the BSD license
** as follows:
**
** "Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are
** met:
**   * Redistributions of source code must retain the above copyright
**     notice, this list of conditions and the following disclaimer.
**   * Redistributions in binary form must reproduce the above copyright
**     notice, this list of conditions and the following disclaimer in
**     the documentation and/or other materials provided with the
**     distribution.
**   * Neither the name of The Qt Company Ltd nor the names of its
**     contributors may be used to endorse or promote products derived
**     from this software without specific prior written permission.
**
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
** "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
** LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
** A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
** OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
** SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
** LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
** DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
** THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
** OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
**
** $QT_END_LICENSE$
**
****************************************************************************/

#ifndef OFRENDERPOOL_H
#define OFRENDERPOOL_H

#include <OpenFrames/WindowProxy.hpp>
#include <QVector>

class OFRendererIF
{
public:
    OFRendererIF() {}
    ~OFRendererIF() {}

    virtual OpenFrames::WindowProxy *winproxy() = 0;
    virtual bool makeCurrent() = 0;
    virtual void swapBuffers() = 0;
    virtual void doneCurrent() = 0;
    virtual void keyPressCallback(int key) = 0;
};

class OFRenderPool
{
public:
    static void PoolAddInstance(OFRendererIF *renderer) { POOL.append(renderer); }
    static void PoolRemoveInstance(OFRendererIF *renderer) { POOL.removeAll(renderer);  }
    static OFRendererIF *PoolFindInstanceWithWinID(unsigned int *winID);
    static void PoolKeyPressCallback(unsigned int *winID, unsigned int *row, unsigned int *col, int *key);
    static void PoolMakeCurrent(unsigned int *winID, bool *success);
    static void PoolSwapBuffers(unsigned int *winID);

private:
    // Private declaration reserves constructor and destructor because this class is static only
    OFRenderPool() {};
    ~OFRenderPool() {};

    static QVector<OFRendererIF *> POOL;
};

#endif
