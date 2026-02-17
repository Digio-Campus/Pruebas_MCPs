#ifndef TICTACTOE_GAME_H
#define TICTACTOE_GAME_H

#include <vector>
#include "Board.h"
#include "Settings.h"
#include "Menu.h"
#include "UI.h"

class Game {
public:
    Game();
    void run();

private:
    Settings settings_;
    Menu menu_;
    UI ui_;
    std::vector<Board> boards_;
    int selectedBoard_;
    int cursorR_, cursorC_;

    void mainMenu();
    void settingsMenu();
    void helpScreen();
    void playLoop();
    void resetBoards();
    void checkAndUpdateStats(int idx);
};

#endif // TICTACTOE_GAME_H
