#ifndef GAME_H
#define GAME_H

#include <vector>
#include "UI.h"
#include "Settings.h"
#include "Board.h"

class Game {
private:
    UI& ui;
    Settings& settings;
    std::vector<Board> boards;
    int selectedBoard;
    int cursorRow, cursorCol;

    void handleMode0();
    void handleMode1();
    void handleMode2();
    void processInput(int ch);
    void checkGameEnd();

public:
    Game(UI& ui, Settings& settings);
    void run();
};

#endif // GAME_H