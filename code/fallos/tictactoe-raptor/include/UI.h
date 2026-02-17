#pragma once

#include <vector>
#include "Board.h"
#include "Settings.h"

class Game; // forward

class UI {
public:
    UI();
    ~UI();

    bool init();
    void shutdown();

    void draw(const std::vector<Board> &boards, int selectedBoard, const Settings &settings);
    void drawMenu();
    void drawHelp();

    // map screen coordinates / cell selection helpers
    int mapKeyToCellMove(int key, int &cx, int &cy);

    // layout calculation helpers
    void computeBoardLayout(int numBoards, int &rows, int &cols) const;

    void clearScreen();
};
