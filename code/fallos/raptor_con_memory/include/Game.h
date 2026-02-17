#ifndef GAME_H
#define GAME_H

#include "Board.h"
#include "Settings.h"
#include <vector>

class UI;

class Game {
public:
    Game();
    ~Game();
    void run();
    // accessors used by UI
    int boardCount() const;
    Board& getBoard(int i);
    int selectedBoard() const;
    void selectBoard(int i);
    void nextBoard();
    void prevBoard();
    void makeHumanMove(int boardIdx,int r,int c);
    void resetBoard(int idx);
    Settings& gameSettings();
    // make resize handler public so the SIGWINCH handler can call it
    void handleResize();
    // ensure boards match settings (public for UI interaction)
    void ensureBoards();
private:
    void mainMenu();
    void startGameLoop();
    void applyMode0Autoplay();
    Settings settings;
    std::vector<Board> boards;
    int selBoard;
    UI* ui;
};

#endif
