#ifndef GAME_H
#define GAME_H

#include <vector>
#include "Board.h"
#include "UI.h"
#include "Settings.h"

class Game {
private:
    UI& ui;
    Settings& settings;
    std::vector<Board> boards;
    size_t selectedBoard;
    int cursorRow;
    int cursorCol;

    void initializeBoards();
    void handleMode0();
    void handleMode1();
    void handleMode2();
    void handleInput(int ch);
    void makeMove(int boardIndex, int row, int col);
    void resetBoard(int boardIndex);
    void switchBoard();
    bool allBoardsFinished() const;

public:
    Game(UI& uiRef, Settings& settingsRef);
    ~Game() = default;

    void run();
};

#endif // GAME_H