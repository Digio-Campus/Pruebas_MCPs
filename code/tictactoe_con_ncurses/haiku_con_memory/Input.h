#ifndef INPUT_H
#define INPUT_H

#include <ncurses.h>

struct MouseEvent {
    int x;
    int y;
    int button;
    bool pressed;
};

class Input {
private:
    bool mouseEnabled;

public:
    Input();
    ~Input();
    
    // Input handling
    int getKeypress();
    bool getMouseEvent(MouseEvent& event);
    void enableMouse();
    void disableMouse();
};

#endif // INPUT_H
