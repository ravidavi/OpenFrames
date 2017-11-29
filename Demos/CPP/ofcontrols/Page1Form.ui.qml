import QtQuick 2.7
import QtQuick.Controls 2.0
import QtQuick.Layouts 1.3

Item {
    property alias textField1: textField1
    property alias button1: button1

    RowLayout {
        anchors.horizontalCenter: parent.horizontalCenter
        anchors.topMargin: 20
        anchors.top: parent.top

        TextField {
            id: textField1
            placeholderText: qsTr("Text Field")
            activeFocusOnPress: true
            echoMode: Normal
            selectByMouse: True
        }

        Button {
            id: button1
            text: qsTr("Press Me")
        }
    }

    Slider {
        id: slider
        x: 167
        y: 66
        value: 0.5
    }
}
