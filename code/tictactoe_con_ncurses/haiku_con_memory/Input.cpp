#include "Input.h"

Input::Input() : mouseEnabled(false) {
    // Enable mouse events via ncurses
    mousemask(ALL_MOUSE_EVENTS, nullptr);
    mouseinterval(0);
}

Input::~Input() {
    mousemask(0, nullptr);
}

int Input::getKeypress() {
    return getch();
}

bool Input::getMouseEvent(MouseEvent& event) {
    MEVENT mevent;
    
    int ch = getch();
    if (ch == KEY_MOUSE && getmouse(&mevent) == OK) {
        event.x = mevent.x;
        event.y = mevent.y;
        event.button = mevent.bstate & (BUTTON1_PRESSED | BUTTON2_PRESSED | BUTTON3_PRESSED);
        event.pressed = (mevent.bstate & (BUTTON1_PRESSED | BUTTON2_PRESSED | BUTTON3_PRESSED)) != 0;
        return true;
    }
    
    return false;
}

void Input::enableMouse() {
    mousemask(ALL_MOUSE_EVENTS, nullptr);
    mouseinterval(0);
    mouseEnabled = true;
}

void Input::disableMouse() {
    mousemask(0, nullptr);
    mouseEnabled = false;
}
