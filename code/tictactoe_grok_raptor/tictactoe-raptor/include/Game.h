#pragma once

#include <ncurses.h>
#include "Board.h"
#include "UI.h"
#include "Settings.h"
#include <vector>

namespace ttt {

class Game {
public:
    Game();
    int run();

private:
    void initBoards();
    void mainLoop();
    void handleKey(int ch);
    void handleMouse(MEVENT& m);
    void autoStepAllBoards();

    std::vector<Board> boards_;
    Settings settings_;
    UI ui_;
    int selectedBoard_;
    int mode_; // alias for settings_.numPlayers()
    bool running_;
};

} // namespace ttt
