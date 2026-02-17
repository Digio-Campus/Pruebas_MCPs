#ifndef TICTACTOE_UI_H
#define TICTACTOE_UI_H

#include "Game.h"
#include <string>
#include <ncurses.h>

namespace ttt {

class UI {
public:
    UI(Game& game);
    int run(); // bucle principal

private:
    Game& game_;
    int selectedBoard_;
    int selRow_, selCol_;
    bool shouldExit_;

    void initCurses();
    void deinitCurses();

    // pantallas
    void showMainMenu();
    void showSettings();
    void showHelp();
    void runGameLoop();

    // dibujo y utilidades
    void drawBoards();
    void drawSingleBoard(int idx, int top, int left, int height, int width, bool highlighted);
    void handleInput(int ch);
    void handleMouse(MEVENT& ev);
    void performAutoPlayIfNeeded();
    void resizeHandler();
    void centerText(int y, const std::string& s);

    bool screenToBoardCell(int y, int x, int& boardIdx, int& row, int& col);
};

} // namespace ttt

#endif // TICTACTOE_UI_H
