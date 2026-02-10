#ifndef GAME_H
#define GAME_H

#include <vector>
#include <cstdlib>
#include <ctime>
#include "Board.h"
#include "UI.h"
#include "Input.h"
#include "Settings.h"

class Game {
private:
    std::vector<Board> boards;
    UI ui;
    Input input;
    Settings settings;
    int currentBoardIdx;
    int currentCellRow, currentCellCol;
    bool running;
    bool gameActive;

    // Game logic helpers
    void makeAIMove(int boardIdx);
    void makeRandomMove(int boardIdx);
    bool isBoardFull(int boardIdx) const;

public:
    Game();
    
    // Main game loop
    void run();
    void mainMenu();
    void settingsMenu();
    void helpMenu();
    void playGame();
    
    // Game state
    void initializeGame();
    void resetAllBoards();
    void updateGame();
};

#endif // GAME_H
