#pragma once

#include <vector>
#include "Board.h"
#include "Settings.h"

class UI;

class Game {
public:
    Game();
    void run();

    // game operations
    bool playerPlace(int boardIdx, int cellIdx); // returns true if move happened
    void autoStep(); // perform automatic moves according to settings
    void resetBoard(int boardIdx);

    // accessors used by UI
    const std::vector<Board>& getBoards() const { return boards; }
    int getSelected() const { return selected; }
    void setSelected(int s) { selected = s; }
    Settings settings;

private:
    std::vector<Board> boards;
    int selected;
    UI *ui;

    void ensureBoards();
};
