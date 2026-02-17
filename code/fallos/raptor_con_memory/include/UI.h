#ifndef UI_H
#define UI_H

#include "Board.h"
#include "Settings.h"
#include <vector>

class Game;

class UI {
public:
    UI(Game* g, Settings* s);
    ~UI();
    void init();
    void draw();
    void drawMenuScreen();
    void drawHelpScreen();
    void drawSettingsScreen();
    void processInput(int ch);
    void refreshLayout();
private:
    Game* game;
    Settings* settings;
    int rows, cols;
    int selBoard, cursorR, cursorC;
    enum Screen { MENU, SETTINGS, HELP, PLAY } screen;
    // layout info for each board
    struct Rect { int top,left,height,width; };
    std::vector<Rect> boardRects;
    void computeLayout();
    void drawBoards();
    void drawSingleBoard(int idx, const Rect &rct, bool highlight);
    void handleMouse();
    void mapClickToCell(int mx,int my,int &b,int &r,int &c);
};

#endif
